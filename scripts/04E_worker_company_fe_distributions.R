##################################################################################
### Worker / Company fe analysis #####
# This script creates graphs of worker fe / company fe distributions per firm type for different models
# It also creates .dta files to apply the Combes et al 2012 method in Stata
# Note that the selected company fe in this script serve as input for the company fe regressions in 05B

across.firm.interactions <- FALSE

##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")


if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")
if (!require("ggpubr")) install.packages("ggpubr"); library("ggpubr") # arranging plots
if (packageVersion("ggpubr")!="0.4.0") warning("Analysis ran in ggpubr version 0.4.0 - consider up- or downgrading")
if (!require("viridis")) install.packages("viridis"); library("viridis") # Color palette
if (packageVersion("viridis")!="0.6.2") warning("Analysis ran in viridis version 0.6.2 - consider up- or downgrading")

if (!require("haven")) install.packages("haven"); library("haven") # save .dta (STATA) file
if (packageVersion("haven")!="2.5.0") warning("Analysis ran in haven version 2.5.0 - consider up- or downgrading")

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step4/"))) dir.create(paste0(map_data_analysis, "step4/")) # to store regression object for further analysis

if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
if (!dir.exists(paste0(map_output, "analysis/wages/sorting/"))) dir.create(paste0(map_output, "analysis/wages/sorting/"))

map_output_here <- paste0(map_output, "analysis/wages/sorting/") # for final outputs

#### load data  ####
# main wage model
main_wage_model <- readRDS(paste0(map_data_analysis, "step4/main_model", fifelse(across.firm.interactions, "_w_across", "_wo_across"), ".rds"))
# wage model with workerfe interactions.
workerfe_interaction_model <- readRDS(paste0(map_data_analysis, "step4/worker_fe/worker_FE_interaction_model.rds"))
# main dataset
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 


##################################################################################
##################################################################################
#### add fes from wage ability model #######
# retrieve fes
fes <- fixef(workerfe_interaction_model, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) 
# extract worker fixed effects
workerfe_interaction_model$worker.fes.panel <- merge(reg.table[, c("worker.ID", "year", "Nemployer.year", "company.type.1", "OG_BEID")],
                   data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID),
                   by="worker.ID")
workerfe_interaction_model$worker.fes.panel[,company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
# company fes
workerfe_interaction_model$firm.fes.table <- merge(reg.table[, .(company.type.1=unique(company.type.1)), by="OG_BEID"],
                                                   data.table(OG_BEID = as.integer(names(fes$OG_BEID)), fe_OG_BEID = fes$OG_BEID),
                                                   by="OG_BEID")
workerfe_interaction_model$firm.fes.table[,company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]

rm(fes)


#### prepare data #####
# identify first observation per worker
setorderv(reg.table, cols = c("worker.ID", "year", "Nemployer.year")) 
reg.table[, n := 1:.N, by=worker.ID]

# add to workerfe_interaction_model and main model 
workerfe_interaction_model$worker.fes.panel <- merge(workerfe_interaction_model$worker.fes.panel, reg.table[, c("worker.ID", "year", "Nemployer.year", "n", "job.number")], by=c("worker.ID", "year", "Nemployer.year"))
main_wage_model$worker.fes.panel <- merge(main_wage_model$worker.fes.panel, reg.table[, c("worker.ID", "year", "Nemployer.year", "n", "job.number")], by=c("worker.ID", "year", "Nemployer.year"))

# redorder data s.t. a domestic firm is first
## find a large  company, extract rows and add at beginning
ref_firm <- reg.table[, .N, by=OG_BEID][, pick := N==max(N)][pick==TRUE, OG_BEID]
reg.table <- rbindlist(list(reg.table[OG_BEID==ref_firm, ], reg.table[OG_BEID != ref_firm, ]))
rm(ref_firm)

#### remove unnecessary items #####
main_wage_model[-which(names(main_wage_model) %in% c("firm.fes.table", "worker.fes.panel"))] <- NULL
workerfe_interaction_model[-which(names(workerfe_interaction_model) %in% c("firm.fes.table", "worker.fes.panel"))] <- NULL

##################################################################################
#### Estimate basic models without experience #####
##### Set formula parts ####
# size
size <- "+ log(company.group.size)"

##### Run regressions #######
# company.group.size influences the company fixed effect estimates, so we take two basic versions: one with size and one without

# let's results in a nested list
fe_model <- list()
fe_model$w_size$type <- "with size included"
fe_model$wo_size$type <- "w/o size included"

for (add.size in c(TRUE, FALSE)){
  ###### Run regression ######
  reg_fe <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                        ifelse(add.size, size, "0"),
                                        "",
                                        "| ",
                                        # fixed effects
                                        "worker.ID ",
                                        "+ OG_BEID",
                                        ""
  )),
  #cluster="OG_BEID",
  # ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
  vcov="iid",
  nthreads=7,
  data=reg.table[,])
  ######  retrieve fixed effects  ###### 
  fes <- fixef(reg_fe, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
  ###### Company fixed effects #####
  ### save estimates in table, mark reference, identify switchers
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OG_BEID=fes$OG_BEID, reference=FALSE)
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table[which(fe_model[[ifelse(add.size, "w_size", "wo_size")]]$OG_BEID==names(fes$OG_BEID)[which(fes$OG_BEID==0)]), reference := TRUE]
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table <- merge(fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table, 
                                                                                        reg.table[, .(company.type.1=unique(company.type.1), company.group.size=mean(company.group.size)), by=c("OG_BEID")], 
                                                                                        by=c("OG_BEID"))
  ### Identify switching firms
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
  #firm.fes.table[company.type.1=="MNE", uniqueN(OG_BEID), by=company.type.1.change] # 37% of MNEs change the type.
  ### create panel version of company fes as well
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.panel <- merge(reg.table[, c("worker.ID","year", "Nemployer.year", "OG_BEID", "company.type.1", "first_job", "company.group.size")],
                                                                                        fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table[, !c("company.group.size")], by=c("OG_BEID", "company.type.1"))
  #### calculate per group means, as employment weighted average
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.means <- fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table[company.type.1.change==FALSE, .(fe_OG_BEID_mean=sum(fe_OG_BEID*company.group.size)/sum(company.group.size)), by=company.type.1]
  
  ###### Worker fixed effects #####
  # Also store worker fes. reference is in OG_BEIDs, not worker.IDs
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$worker.fes.table <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID, reference=FALSE)
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$worker.fes.panel <- merge(fe_model[[ifelse(add.size, "w_size", "wo_size")]]$worker.fes.table, reg.table[, c("worker.ID","year", "Nemployer.year", "OG_BEID", "company.type.1", "first_job", "company.group.size")], by=c("worker.ID"))
  fe_model[[ifelse(add.size, "w_size", "wo_size")]]$worker.fes.panel[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
  
  rm(fes) 
  rm(reg_fe)
}

##################################################################################
#### Estimate the full model w/o size control ###
##### Set formula parts ####
# save types for formula
types <- reg.table[, unique(company.type.1)]
## empirical model formula parts

# within firm tenure (growth)
within_firm <- paste0(" + i(tenure_group_OGBEID_spell, ref=\"[-Inf,1]\")")
within_firm_interact <- paste0(" + i(company.type.1, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\")")

# within firm tenure by job number (general effect)
within_firm_job <- paste0(" + i(tenure_group_OGBEID_spell, i.job_no_trunc, ref=\"[-Inf,1]\", ref2=\"1\")")

# across firm experience returns
across_firm <- paste0(" + i(exp_group_", types, "_", "1", ", ref = \"[-Inf,0]\")", collapse = "")

# controls
controls <- "+ i(job_no_trunc, ref=\"1\")" 

##### Run regression #######
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]


# add it to same list as above 

reg_fe <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                        within_firm,
                                        within_firm_interact,
                                        within_firm_job,
                                        across_firm, 
                                        controls, # here controls does not include company size
                                        "",
                                        "| ",
                                        # fixed effects
                                        "worker.ID ",
                                        "+ OG_BEID",
                                        ""
                      )),
                      vcov = "iid",
                      nthreads=7,
                      data=reg.table)

######  retrieve fixed effects  ###### 
fes <- fixef(reg_fe, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
###### Company fixed effects #####
### save estimates in table, mark reference, identify switchers
fe_model$fullwosize$firm.fes.table <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OG_BEID=fes$OG_BEID, reference=FALSE)
fe_model$fullwosize$firm.fes.table[which(fe_model$fullwosize$firm.fes.table==names(fes$OG_BEID)[which(fes$OG_BEID==0)]), reference := TRUE]
fe_model$fullwosize$firm.fes.table <- merge(fe_model$fullwosize$firm.fes.table, 
                                                                          reg.table[, .(company.type.1=unique(company.type.1), company.group.size=mean(company.group.size)), by=c("OG_BEID")], 
                                                                          by=c("OG_BEID"))
### Identify switching firms
fe_model$fullwosize$firm.fes.table[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
#firm.fes.table[company.type.1=="MNE", uniqueN(OG_BEID), by=company.type.1.change] # 37% of MNEs change the type.
### create panel version of company fes as well
fe_model$fullwosize$firm.fes.panel <- merge(reg.table[, c("worker.ID","year", "Nemployer.year", "OG_BEID", "company.type.1", "first_job", "company.group.size")],
                                            fe_model$fullwosize$firm.fes.table[, !c("company.group.size")], by=c("OG_BEID", "company.type.1"))
#### calculate per group means, as employment weighted average
fe_model$fullwosize$firm.fes.means <- fe_model$fullwosize$firm.fes.table[company.type.1.change==FALSE, .(fe_OG_BEID_mean=sum(fe_OG_BEID*company.group.size)/sum(company.group.size)), by=company.type.1]

###### Worker fixed effects #####
# Also store worker fes. reference is in OG_BEIDs, not worker.IDs
fe_model$fullwosize$worker.fes.table <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID, reference=FALSE)
fe_model$fullwosize$worker.fes.panel <- merge(fe_model$fullwosize$worker.fes.table, reg.table[, c("worker.ID","year", "Nemployer.year", "OG_BEID", "company.type.1", "first_job", "company.group.size")], by=c("worker.ID"))
fe_model$fullwosize$worker.fes.panel[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]

rm(fes) 
rm(reg_fe)


##################################################################################
#### plot distributions: black and white (paper) #####
# let's store the plots in a nested list
fe_plots <- list()
fe_plots$w_size$type <- "basic model with size included"
fe_plots$wo_size$type <- "basic model w/o size included"
fe_plots$main_wage_model$type <- "full model"
fe_plots$workerfe_interaction_model$type <- "full model with worker fe interactions"
fe_plots$starters$type <- "full model with worker fe interactions (Labor market entrants only)"
fe_plots$fullwosize$type <- "full model without company size control"

##### Worker fixed effects #######
# for worker fe's need to pick a date in time, e.g. year==2013, Nemployer.year==1 (roughly middle of panel)
time.point <- 2014
Nemp <- 1

# worker fe distribution: fe_model (2 specifications)
for (add.size in c(TRUE, FALSE)){
  fe_plots[[ifelse(add.size, "w_size", "wo_size")]]$worker_fes <- 
    ggplot(data=fe_model[[ifelse(add.size, "w_size", "wo_size")]]$worker.fes.panel[year %in% time.point & Nemployer.year %in% Nemp & company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
           aes(x=fe_workerID, group=company.type.1)) +
    geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
    xlab("Worker fixed effect") +
    ylab("Density") +
    scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
    theme_bw()  +
    #scale_x_continuous(limits=c(-3,3)) + #limit x range --> need to adjust this manually
   # scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    theme(legend.position="bottom", 
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          panel.background = element_blank(), panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank())  
}


# worker fe distribution: main_wage_model
fe_plots[["main_wage_model"]]$worker_fes <- 
  ggplot(data=main_wage_model$worker.fes.panel[year %in% time.point & Nemployer.year %in% Nemp & company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_workerID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
 # scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  #theme(legend.title = element_blank(), legend.position="bottom")
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

# worker fe distribution: workerfe_interaction_model
fe_plots[["workerfe_interaction_model"]]$worker_fes <- 
  ggplot(data=workerfe_interaction_model$worker.fes.panel[year %in% time.point & Nemployer.year %in% Nemp & company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_workerID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  #scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  #theme(legend.title = element_blank(), legend.position="bottom")
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

# worker fe distribution: workerfe_interaction_model (starters only)
fe_plots[["starters"]]$worker_fes <- 
  ggplot(data=workerfe_interaction_model$worker.fes.panel[n==1 & year %in% time.point & company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_workerID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  #scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  #theme(legend.title = element_blank(), legend.position="bottom")
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

# worker fe distribution: main_wage_model without company size control
fe_plots[["fullwosize"]]$worker_fes <- 
  ggplot(data=fe_model$fullwosize$worker.fes.panel[year %in% time.point & Nemployer.year %in% Nemp & company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_workerID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  # scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  #theme(legend.title = element_blank(), legend.position="bottom")
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

##### Create output plot #######

# save individual plots
fe_plots$wo_size$worker_fes
fe_plots$w_size$worker_fes
fe_plots$main_wage_model$worker_fes
fe_plots$workerfe_interaction_model$worker_fes
fe_plots$starters$worker_fes
fe_plots$fullwosize$worker_fes

ggsave(filename=paste0(map_output_here, "plot_worker_fe_static_wo_size.pdf"), fe_plots$wo_size$worker_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_worker_fe_static_w_size.pdf"), fe_plots$w_size$worker_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_worker_fe_dynamic_main.pdf"), fe_plots$main_wage_model$worker_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_worker_fe_dynamic_main_wo_size.pdf"), fe_plots$fullwosize$worker_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_worker_fe_dynamic_interactions.pdf"), fe_plots$workerfe_interaction_model$worker_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_worker_fe_dynamic_starters.pdf"), fe_plots$starters$worker_fes,
       width=16, height=9, scale=0.4, dpi=300)



# create output panel of interesting plots
g <- ggarrange(fe_plots$w_size$worker_fes + ggtitle(paste0("a) Static model", " with size control (year = ", time.point, ")")),
               fe_plots$main_wage_model$worker_fes + ggtitle(paste0("b) Experience model", " (year = ", time.point, ")")),
               fe_plots$workerfe_interaction_model$worker_fes + ggtitle(paste0("c) Experience model with worker fe interactions", " (year = ", time.point, ")")),
               fe_plots$starters$worker_fes + ggtitle(paste0("d) Labor market entrant in experience model with worker fe interactions", " (year = ", time.point, ")")),
               ncol=2, nrow=2, common.legend = TRUE,  legend="bottom")

ggsave(filename=paste0(map_output_here, "plot_worker_fe_comparison.pdf"), g,
       width=16*0.8, height=9*0.8, scale=1.5, dpi=300)

##### Company fixed effects #######

# company fe distribution: fe_model  (2 specifications)
for (add.size in c(TRUE, FALSE)){
  fe_plots[[ifelse(add.size, "w_size", "wo_size")]]$company_fes <- 
    ggplot(data=fe_model[[ifelse(add.size, "w_size", "wo_size")]]$firm.fes.table[company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
           aes(x=fe_OG_BEID, group=company.type.1)) +
    geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
    xlab("Firm fixed effect") +
    ylab("Density") +
    scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
    theme_bw()  +
    scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
    scale_x_continuous(expand = c(0.01, 0.01), limit = c(-3,3)) +
    theme(legend.position="bottom", 
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          panel.background = element_blank(), panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank())  
}


# company fe distribution: main_wage_model
fe_plots[["main_wage_model"]]$company_fes <- 
  ggplot(data=main_wage_model$firm.fes.table[company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_OG_BEID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Firm fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  scale_x_continuous(expand = c(0.01, 0.01), limit = c(-3,3)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

# company fe distribution: workerfe_interaction_model
fe_plots[["workerfe_interaction_model"]]$company_fes <- 
  ggplot(data=workerfe_interaction_model$firm.fes.table[company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_OG_BEID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Firm fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  scale_x_continuous(expand = c(0.01, 0.01), limit = c(-3,3)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

# company fe distribution: main_wage_model without size control
fe_plots[["fullwosize"]]$company_fes <- 
  ggplot(data=fe_model$fullwosize$firm.fes.table[company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"),],
         aes(x=fe_OG_BEID, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Firm fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  scale_x_continuous(expand = c(0.01, 0.01), limit = c(-3,3)) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())  

##### Create output plot #######

# check individual plots here
fe_plots$wo_size$company_fes
fe_plots$w_size$company_fes
fe_plots$main_wage_model$company_fes
fe_plots$workerfe_interaction_model$company_fes
fe_plots$fullwosize$company_fes

ggsave(filename=paste0(map_output_here, "plot_company_fe_static_wo_size.pdf"), fe_plots$wo_size$company_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_company_fe_static_w_size.pdf"), fe_plots$w_size$company_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_company_fe_dynamic_main.pdf"), fe_plots$main_wage_model$company_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_company_fe_dynamic_main_wo_size.pdf"), fe_plots$fullwosize$company_fes,
       width=16, height=9, scale=0.4, dpi=300)
ggsave(filename=paste0(map_output_here, "plot_company_fe_dynamic_interactions.pdf"), fe_plots$workerfe_interaction_model$company_fes,
       width=16, height=9, scale=0.4, dpi=300)


# creat output panel of interesting plots
g <- ggarrange(fe_plots$wo_size$company_fes  + ggtitle(paste0("a) Static model w/o size control")), 
               fe_plots$fullwosize$company_fes + ggtitle(paste0("b) Experience model w/o size control")),
               fe_plots$main_wage_model$company_fes   + ggtitle(paste0("c) Experience model")),
               fe_plots$workerfe_interaction_model$company_fes + ggtitle(paste0("d) Experience model with worker fe interactions")),
               ncol=2, nrow=2, common.legend = TRUE,  legend="bottom")

ggsave(filename=paste0(map_output_here, "plot_company_fe_comparison.pdf"), g,
       width=16*0.8, height=9*0.8, scale=1.5, dpi=300)

##################################################################################

#### Output table for Stata #####
# I will run the Combes et al 2012 method using the Stata package estquant (for worker fixed effects) 
# create a .dta file for stata here

##### worker fixed effects #####

# main model
workerfes <- main_wage_model$worker.fes.panel[, !c("reference", "company.group.size")]
setnames(workerfes, "fe_workerID", "main")
workerfes[,.N]

# worker fe interacted model
workerfes <- merge(workerfes, workerfe_interaction_model$worker.fes.panel[, .(worker.ID, year, Nemployer.year, 
                                                                              interacted = fe_workerID)],
                   by=c("worker.ID", "year", "Nemployer.year"))
workerfes[,.N]

# worker fe interacted model (starters) -- set starter restriction below
workerfes[, starter := interacted]

# size control model
workerfes <- merge(workerfes,fe_model$w_size$worker.fes.panel[, .(worker.ID, year, Nemployer.year, 
                                                                              basicsize = fe_workerID)],
                   by=c("worker.ID", "year", "Nemployer.year"))
workerfes[,.N]

# basic model
workerfes <- merge(workerfes,fe_model$wo_size$worker.fes.panel[, .(worker.ID, year, Nemployer.year, 
                                                                       basic = fe_workerID)],
                   by=c("worker.ID", "year", "Nemployer.year"))
workerfes[,.N]

# full model without size (= main model ; without workerfe interactions)
workerfes <- merge(workerfes,fe_model$fullwosize$worker.fes.panel[, .(worker.ID, year, Nemployer.year, 
                                                                      fullwosize = fe_workerID)],
                   by=c("worker.ID", "year", "Nemployer.year"))
workerfes[,.N]

# save FULL file
saveRDS(workerfes, file=paste0(map_data_analysis, "step4/workerfes_full.rds"))

# apply same restriction to data set as in plots with time point
workerfes <- workerfes[year %in% time.point & Nemployer.year %in% Nemp & company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"), ]
# apply same restriction for starters
workerfes[n > 1, starter := NA_real_]

# only keep absolutely necessary columns 
workerfes <- workerfes[, c("worker.ID", "company.type.1", "basic", "basicsize",  "main", "interacted",  "starter", "fullwosize")]
workerfes[, MNE := company.type.1 == "MNE"] 
workerfes[, workerID := worker.ID]
workerfes[, c("company.type.1", "worker.ID") := NULL]


# save as STATA file
write_dta(workerfes, path=paste0(map_data_analysis, "step4/workerfes.dta"), version=15)


##### company fixed effects #####

companyfes <- main_wage_model$firm.fes.table[, !c("company.group.size")]
setnames(companyfes, "fe_OG_BEID", "main")
companyfes[,.N]

companyfes <- merge(companyfes, workerfe_interaction_model$firm.fes.table[, .(OG_BEID, company.type.1,
                                                                              interacted = fe_OG_BEID)],
                   by=c("OG_BEID", "company.type.1"))
companyfes[,.N]

companyfes <- merge(companyfes, fe_model$w_size$firm.fes.table[, .(OG_BEID, company.type.1,
                                                                              basicsize = fe_OG_BEID)],
                    by=c("OG_BEID", "company.type.1"))
companyfes[,.N]


companyfes <- merge(companyfes, fe_model$wo_size$firm.fes.table[, .(OG_BEID, company.type.1,
                                                                       basic = fe_OG_BEID)],
                    by=c("OG_BEID", "company.type.1"))
companyfes[,.N]

# full model without size (= main model ; without workerfe interactions)
companyfes <- merge(companyfes,fe_model$fullwosize$firm.fes.table[, .(OG_BEID, company.type.1,
                                                                      fullwosize = fe_OG_BEID)],
                    by=c("OG_BEID", "company.type.1"))
companyfes[,.N]

# save FULL file
saveRDS(companyfes, file=paste0(map_data_analysis, "step4/companyfes_full.rds"))

# apply same restriction to data set as in plots
companyfes <- companyfes[company.type.1.change==FALSE & company.type.1 %in% c("DOMESTIC", "MNE"), ]

# only keep absolutely necessary columns 
companyfes <- companyfes[, c("OG_BEID", "company.type.1", "main", "interacted", "basicsize", "basic", "fullwosize")]
companyfes[, MNE := company.type.1 == "MNE"] 
companyfes[, c("company.type.1") := NULL]

# save as STATA file
write_dta(companyfes, path=paste0(map_data_analysis, "step4/companyfes.dta"), version=15)

