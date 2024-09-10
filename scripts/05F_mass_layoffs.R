##################################################################################
### Wage level analysis: Causal evidence from mass layoffs, including bootstrapped standard errors (workerfe interactions) #### 

##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")
if (!require("xtable")) install.packages("xtable"); library("xtable") # .tex of matrices/data.tables
if (packageVersion("xtable")!="1.8.4") warning("Analysis run in xtable version 1.8.4 - consider up- or downgrading")

if (!require("marginaleffects")) install.packages("marginaleffects"); library("marginaleffects") # flexible hypothesis tests on fixest model coefficients
if (packageVersion("marginaleffects")!="0.11.1") warning("Analysis run in marginaleffects version 0.11.1 - consider up- or downgrading")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
if (!dir.exists(paste0(map_output, "analysis/wages/masslayoffs/"))) dir.create(paste0(map_output, "analysis/wages/masslayoffs/"))

map_output_here <- paste0(map_output, "analysis/wages/masslayoffs/") # for final outputs

#### load data  ####
# graduates network
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
# full network
POLIS <- readRDS(paste0(map_data_analysis, "step2/POLIS.rds"))
# ability regression model with worker fe interactions (to retrieve fixed effects)
ability_model <- readRDS(paste0(map_data_analysis, "step4/worker_fe/worker_FE_interaction_model.rds")) # get worker fixed effects
# main model fe estimates 
main_model <- readRDS(paste0(map_data_analysis, "step4/main_model_wo_across.rds"))

##################################################################################
##################################################################################
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

#### Add firm and worker fes, with and without interaction specification ###### 
##### No-interaction model ##### 
main_model$worker.fes.table[, reference := NULL]
setnames(main_model$worker.fes.table, "fe_workerID", "fe_workerID_basic")
reg.table <- merge(reg.table, main_model$worker.fes.table, by="worker.ID")

setnames(main_model$firm.fes.table, "fe_OG_BEID", "fe_OGBEID_basic")
reg.table <- merge(reg.table, unique(main_model$firm.fes.table[, c("OG_BEID", "fe_OGBEID_basic")]), by="OG_BEID")
rm(main_model)
##### Interaction model #####
# substract estimates of firm and worker fixed effects
# retrieve fes
fes <- fixef(ability_model, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
# extract worker fixed effects and standardise
workerfes <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID_interacted = fes$worker.ID)
firmfes <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OGBEID_interacted = fes$OG_BEID)
# add to reg.table
reg.table <- merge(reg.table, workerfes, by="worker.ID")
reg.table <- merge(reg.table, firmfes, by="OG_BEID")
rm(firmfes, workerfes, fes)
rm(ability_model)


##### calculate wage net of fixed effects (and industry year) #####
reg.table[, lhwage_resid_basic := lhwage_detrended - fe_workerID_basic - fe_OGBEID_basic]
reg.table[, lhwage_resid_interacted := lhwage_detrended - fe_workerID_interacted - fe_OGBEID_interacted]

#### Identify mass layoffs from POLIS #####
# remove possible duplicates from POLIS
POLIS <- unique(POLIS[, c("worker.ID", "year", "OG_BEID", "job.entry")])

# identify worker exits from firms
setorderv(POLIS, c("worker.ID", "year", "job.entry"))
# lead OG_BEID and find exits exits
POLIS[, OG_BEID.lead := shift(OG_BEID, n=1, type="lead", fill=NA), by=worker.ID]
POLIS[, exiter := OG_BEID!=OG_BEID.lead]
POLIS[is.na(exiter) & year < 2021, exiter := TRUE] #adjust for end of worker observation before 2021

#aggregate to firm level
#and count number of exiters (within SBEID and OG_BEID)
OG_BEIDs <- POLIS[, .(workers=.N, exiters=sum(exiter)), by=c("OG_BEID", "year")] 
setorderv(OG_BEIDs, cols=c("OG_BEID", "year"))

# idenfiy last observed year per firm and remove 2021
OG_BEIDs[, closure := year==max(year), by=OG_BEID]
OG_BEIDs <- OG_BEIDs[year<2021,]

# DEFINITION OF MASS LAYOFF

# keep potential mass layoffs and firm closures:
# firm closure or 
# >=80% of workers exit
# and the firm has at least 20 employees
OG_BEIDs <- OG_BEIDs[(exiters/workers>=0.8 & workers>=10), ] #exiters/workers==1 | #(exiters/workers>=0.8 & workers>=20)
# no more than 30% of exiting workers are found at the same establishment in their next observation
## extract exiters and calculate shares, subset to max_share of new OG_BEID < 0.3
tt <- merge(POLIS[exiter==T,], OG_BEIDs, by=c("OG_BEID", "year"))
tt[is.na(OG_BEID.lead), OG_BEID.lead := -1] # fake lead OG_BEID for panel exiters
tt <- tt[, max(unlist(lapply(unique(OG_BEID.lead), function(x) sum(OG_BEID.lead==x)/.N))), by=c("OG_BEID", "year")][V1<0.3,]
OG_BEIDs <- merge(OG_BEIDs, tt[, !c("V1")], by=c("OG_BEID", "year"))

# plot distribution of potential mass layoffs per year, split up by firm closures
ggplot(data=OG_BEIDs[, .N, by=c("year", "closure")], aes(x=year, y=N, fill=closure)) + 
  geom_col() 

# add identifier col
OG_BEIDs[, mass_layoff := TRUE]

#### Identify graduates in mass layoffs (that are observed at next firm) #####
# add to reg.table
reg.table <- merge(reg.table, OG_BEIDs[, c("OG_BEID", "year", "mass_layoff", "closure")], by=c("OG_BEID", "year"), all.x=T)
reg.table[is.na(mass_layoff), mass_layoff := F]

# extract workers that are involved in mass layoffs
reg.table[, keep := any(mass_layoff), by=worker.ID]
masslayoffs <- reg.table[keep==T,]
setorderv(masslayoffs, c("worker.ID", "year", "Nemployer.year"))

## lead OG_BEID and figure out exiters to another job
masslayoffs[, OG_BEID.lead := shift(OG_BEID, n=1, type = "lead", fill = NA), by=worker.ID]
masslayoffs[, exiter := OG_BEID != OG_BEID.lead] #  | is.na(OG_BEID.lead)
masslayoffs[is.na(OG_BEID.lead), exiter := FALSE] # ignore workers that are not observed at next employer
masslayoffs[, mass_layoff := mass_layoff & exiter]

# extract observations at old and new employer around mass layoff
masslayoffs[, keep := any(mass_layoff), by=worker.ID]
masslayoffs <- masslayoffs[keep==T, !c("keep")]
masslayoffs[, entry_after_layoff := shift(mass_layoff, n=1, type = "lag", fill = NA), by=worker.ID]
masslayoffs[, entry_after_layoff := entry_after_layoff[1], by=c("worker.ID", "OG_BEID", "spell.id")]
newjob <- masslayoffs[!is.na(entry_after_layoff) & entry_after_layoff==T, ] # observations in next job
masslayoffs[, keep := any(mass_layoff), by=c("worker.ID", "OG_BEID", "spell.id")] # observations in job before closure
masslayoffs <- masslayoffs[keep==T, !c("keep")]
##################################################################################
###### How many layoffs are there? #####
sink(paste0(map_output_here, "mass_layoffs.txt"))
masslayoffs[, .(Firms=uniqueN(OG_BEID), Workers=uniqueN(worker.ID)), by=company.type.1]
sink()

# print table
print(xtable(masslayoffs[, .(Firms=uniqueN(OG_BEID), Workers=uniqueN(worker.ID)), by=company.type.1], caption="Mass layoffs", label="tab:layoffs_overview",
             digits=2),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mass_layoffs.tex"))


##### Do the worker fes differ? ######

###### involved in firm mass_layoffs vs. rest #####
workerfes <- reg.table[, .(fe_workerID_interacted=fe_workerID_interacted[1]), by=worker.ID]
workerfes[, closure := worker.ID %in% masslayoffs[, unique(worker.ID)]]

ggplot(data=workerfes, aes(x=fe_workerID_interacted, group=closure)) +
  geom_density(aes(linetype=closure), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  scale_x_continuous(limits=c(-2,1), expand = c(0.01, 0.01)) + #limit x range --> need to adjust this manually
  # scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(linetype = guide_legend(title="Mass layoff"))

ggsave(filename=paste0(map_output_here, "plot_worker_fe_mass_layoffs.pdf"), last_plot(),
       width=16, height=9, scale=0.4, dpi=300)

###### involved in firm mass_layoffs, MNE vs. domestic #####
workerfes <- masslayoffs[mass_layoff==T & company.type.1 %in% c("MNE", "DOMESTIC"),  .(fe_workerID_interacted=fe_workerID_interacted[1]), by=c("worker.ID", "company.type.1")]

ggplot(data=workerfes, aes(x=fe_workerID_interacted, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  scale_x_continuous(limits=c(-2,1), expand = c(0.01, 0.01)) + #limit x range --> need to adjust this manually
  # scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(linetype = guide_legend(title="Old firm"))

ggsave(filename=paste0(map_output_here, "plot_worker_fe_mass_layoffs_old_firm.pdf"), last_plot(),
       width=16, height=9, scale=0.4, dpi=300)

###### new employer, MNE vs. domestic #####
workerfes <- newjob[ company.type.1 %in% c("MNE", "DOMESTIC"),  .(fe_workerID_interacted=fe_workerID_interacted[1]), by=c("worker.ID", "company.type.1")]

ggplot(data=workerfes, aes(x=fe_workerID_interacted, group=company.type.1)) +
  geom_density(aes(linetype=company.type.1), trim=TRUE, bw="SJ", kernel="gaussian", size=3/4, n=2^12, alpha=1, adjust=3/4) +
  xlab("Worker fixed effect") +
  ylab("Density") +
  scale_linetype_manual(values=c("dashed", "solid")) + # for 2 groups, change linetypes (for 3 add here)
  theme_bw()  +
  scale_x_continuous(limits=c(-2,1), expand = c(0.01, 0.01)) + #limit x range --> need to adjust this manually
  # scale_color_viridis(discrete=TRUE, alpha=1, end=0.6, direction=1) +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(linetype = guide_legend(title="New firm"))

ggsave(filename=paste0(map_output_here, "plot_worker_fe_mass_layoffs_new_firm.pdf"), last_plot(),
       width=16, height=9, scale=0.4, dpi=300)

###################################################
#### Regression analyses #####
##### wages in first job after layoff: linear profile ##### 
reg_simple <- list()

# w/o interaction
reg_simple[[1]] <- feols(lhwage_detrended ~ -1 + #i(company.type.1, ref="DOMESTIC") + 
                           exp_years_DOMESTIC_1 + exp_years_INT_COMP_1 + exp_years_MNE_1  + 
                    tenure_years_OGBEID_spell  +
                    i(company.type.1, tenure_years_OGBEID_spell, ref="DOMESTIC") +
                    log(company.group.size) + i(job_no_trunc, ref=2) + i(job_no_trunc, tenure_years_OGBEID_spell, ref=2) + # sample can't be first job
                      fe_workerID_basic + fe_OGBEID_basic,
                  data=newjob, cluster="worker.ID")

# with interaction
reg_simple[[2]] <- feols(lhwage_detrended ~ -1 + #i(company.type.1, ref="DOMESTIC") + 
                           exp_years_DOMESTIC_1 + exp_years_INT_COMP_1 + exp_years_MNE_1  + 
                          tenure_years_OGBEID_spell  +
                          i(company.type.1, tenure_years_OGBEID_spell, ref="DOMESTIC") +
                           exp_years_DOMESTIC_1*fe_workerID_interacted - fe_workerID_interacted + 
                           exp_years_INT_COMP_1*fe_workerID_interacted - fe_workerID_interacted + 
                           exp_years_MNE_1*fe_workerID_interacted - fe_workerID_interacted   + 
                           tenure_years_OGBEID_spell*fe_workerID_interacted - fe_workerID_interacted  +
                           i(job_no_trunc, ref=2)*fe_workerID_interacted - fe_workerID_interacted +
                           i(job_no_trunc, tenure_years_OGBEID_spell, ref=2)*fe_workerID_interacted - fe_workerID_interacted +
                           i(company.type.1, tenure_years_OGBEID_spell, ref="DOMESTIC")*fe_workerID_interacted - fe_workerID_interacted +
                          log(company.group.size) + i(job_no_trunc, ref=2) + i(job_no_trunc, tenure_years_OGBEID_spell, ref=2) + # sample can't be first job
                           fe_workerID_interacted + fe_OGBEID_interacted,
                        data=newjob, vcov="iid")
etable(reg_simple)

###### t-test on coefficient difference (simple model w/o worker fe interactions) #####
ttest <- hypotheses(reg_simple[[1]], "exp_years_MNE_1 = exp_years_DOMESTIC_1", vcov = vcov(reg_simple[[1]]), df=degrees_freedom(reg_simple[[1]], type="t"))

sink(paste0(map_output_here, "ttest_across_no_interaction.txt"))
ttest 
cat("\n")
cat(paste("df =", degrees_freedom(reg_simple[[1]], type="t")))
sink()

#### Bootstrap (worker fe interactions - worker fe's estimates before already) #####

# number of bootstrapped worker fixed effect estimates
bootstraps <-  100 

# set a starting seed: to get same draws as in for worker fes, set same same seed
# set initial seed: will update seed in each iteration for easier replication in steps (bootstrap takes long to long)
s <- 4934 # seeds will be (4934+1):(4934+100)

# matrix of bootstrap coefficients
bcoefs <- matrix(NA, nrow=bootstraps, ncol=length(reg_simple[[2]]$coefficients))
colnames(bcoefs) <- names(reg_simple[[2]]$coefficients)

## save fe_workerID_interacted 
newjob[, fe_workerID_interacted_SAVED := fe_workerID_interacted]

# loop through bootstraps, add bootstrapped worker fes, standardize, estimate model coefficients

timer <- Sys.time()
for (bootstrap in 1:bootstraps){
  
  # message
  cat(paste0(Sys.time(), ": starting ", bootstrap, "/", bootstraps), "\n")  
  
  # delete fe_workerID_interacted 
  if (any(names(newjob)=="fe_workerID_interacted")) newjob[, fe_workerID_interacted := NULL]
  
  ##### Add bootstrapped worker fes ####
  # load new worker fixed effects
  fes  <- readRDS(paste0(map_data_analysis, "step5/bootstrap_workerfe/", "fes_", bootstrap, ".rds"))
  # adjust name
  setnames(fes$workerfes, "fe_workerID", "fe_workerID_interacted")
  # add to reg.table
  newjob <- merge(newjob, fes$workerfes, by="worker.ID", all.x=TRUE) # writes NA for missing fe_workerIDs
  rm(fes)
  
  # adjust subsample to avoid missings
  newjob[, subsample_boot := TRUE]
  newjob[is.na(fe_workerID_interacted), subsample_boot := FALSE]
  
  ##### create block bootstrap sample of newjob #####
  # Note that this sample differs from the sample used to estimate the bootstrapped workerfes
  # update seed 
  set.seed(s+bootstrap)
  # get worker IDs in fe bootstrap sample
  sids <- newjob[subsample_boot==TRUE, unique(worker.ID)]
  # draw worker IDs with replacement
  sids <- sample(sids, length(sids), replace = TRUE)
  # create data.table with replacement from sids
  ## logic is to repeatedly add reg.table to itself until all occurrences of worker.ID in sids are fulfilled
  tt_boot <- newjob[worker.ID %in% unique(sids),]
  sids <- sids[duplicated(sids)]
  while (sum(duplicated(sids)) > 0){
    tt_boot <- rbindlist(list(tt_boot, newjob[worker.ID %in% unique(sids),]))
    sids <- sids[duplicated(sids)]
  }
  rm(sids)
  
  
  ##### run regressions on bootstrap sample #####
  cat(paste0(Sys.time(), ": running regression"), "\n")  
  
  
  breg1 <-  feols(lhwage_detrended ~ -1 + #i(company.type.1, ref="DOMESTIC") + 
                    exp_years_DOMESTIC_1 + exp_years_INT_COMP_1 + exp_years_MNE_1  + 
                    tenure_years_OGBEID_spell  +
                    i(company.type.1, tenure_years_OGBEID_spell, ref="DOMESTIC") +
                    exp_years_DOMESTIC_1*fe_workerID_interacted - fe_workerID_interacted + 
                    exp_years_INT_COMP_1*fe_workerID_interacted - fe_workerID_interacted + 
                    exp_years_MNE_1*fe_workerID_interacted - fe_workerID_interacted   + 
                    tenure_years_OGBEID_spell*fe_workerID_interacted - fe_workerID_interacted  +
                    i(job_no_trunc, ref=2)*fe_workerID_interacted - fe_workerID_interacted +
                    i(job_no_trunc, tenure_years_OGBEID_spell, ref=2)*fe_workerID_interacted - fe_workerID_interacted +
                    i(company.type.1, tenure_years_OGBEID_spell, ref="DOMESTIC")*fe_workerID_interacted - fe_workerID_interacted +
                    log(company.group.size) + i(job_no_trunc, ref=2) + i(job_no_trunc, tenure_years_OGBEID_spell, ref=2) + # sample can't be first job
                    fe_workerID_interacted + fe_OGBEID_interacted,
                  data=tt_boot, vcov="iid")
  
  # add coefficients to matrix
  bcoefs[bootstrap, ] <- breg1$coefficients

  # clean up
  rm(breg1, tt_boot)
}
timer <- Sys.time() - timer
timer 

# readd main worker fes
reg.table[, fe_workerID_interacted := fe_workerID_interacted_SAVED]
reg.table[, fe_workerID_interacted_SAVED := NULL]

##### Calculate bootstrapped vcov matrix ####
bvcov <- list()
bvcov[[1]] <- vcov(reg_simple[[1]])
bvcov[[2]] <- cov(bcoefs)

###### create output (with bsrap standard errors) ###### 

etable(reg_simple, #lapply(paste0("regs[[",length(regs):1, "]]"), function(x) eval(parse(text=x))), 
       file=paste0(map_output_here, "reg_wages_masslayoff_simple", ".tex"),  
       vcov = bvcov,
       title="Wage profiles",
       label="tab:main_wage", 
       depvar = TRUE,
       digits = "r4",
       fitstat = c("n", "r2"),
       digits.stats = "r4",
       fixef_sizes = TRUE,
       fixef_sizes.simplify = TRUE,
       powerBelow = -6,
       float = TRUE,
       dict = c(company.type.1 = "", MNE = "MNE", INT_COMP = "International company", tenure_years_OGBEID_spell = "Within company tenure",
                exp_years_MNE_1 = "MNE experience", exp_years_INT_COMP_1 = "International company experience", exp_years_DOMESTIC_1 = "Domestic company experience",
                `log(company.group.size.2)` = "log(company size)", job_no_trunc = "Employer number", 
                fe_workerID_interacted = "Worker fe", fe_workerID_basic = "Worker fe",
                fe_OGBEID_interacted = "Firm fe", fe_OGBEID_basic = "Firm fe"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (worker level) in parantheses."),
       replace = TRUE)


  