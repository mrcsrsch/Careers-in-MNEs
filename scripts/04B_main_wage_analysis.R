##################################################################################
### Wage level analysis #####
# Is that an experience wage premium for workers with experience in MNEs?
# This script runs the main wage regression, retrieve the main fixed effect estimates and performs a back of the envelope calculation.
# It allows to generalize the model, i.e. to include across firm interactions (see robustness checks) - unfortunately not all functions are optimized for this setting.
##################################################################################
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")


#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) # Own functions for analysis

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step4/"))) dir.create(paste0(map_data_analysis, "step4/")) # to store regression object for further analysis

if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
map_output_here <- paste0(map_output, "analysis/wages/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
##################################################################################
##################################################################################
#### Prepare data ######
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

##### Reorder data for fe estimation and adjust OG_BEID fixed effect by internationalisation status #####
# redorder data s.t. a large firm is first. I use this company to keep things stable with the bootstrap approach,
ref_firm <- reg.table[, .N, by=OG_BEID][, pick := N==max(N)][pick==TRUE, OG_BEID]
reg.table <- rbindlist(list(reg.table[OG_BEID==ref_firm, ], reg.table[OG_BEID != ref_firm, ]))

##################################################################################
#### Main wage analysis: settings ####
##### Parameters of analysis ###### 
gc() # collect garbage

sample.use <- FALSE
sample.size <- 0.001*10^6
across.firm.interactions <- FALSE
back.of.envelope <- TRUE # adds back of envelope calculations --> What is the "excess" contribution of MNEs to wages in NL? 
# back.of.envelope only implement for case without across firm interactions
if (across.firm.interactions) back.of.envelope <- FALSE

##### Use full sample or subsample with worker size sample.use? ####
if (sample.use){
  # use a  random sample of workers for quicker calculations
  set.seed(7854)
  reg.table[, subsample := (worker.ID %in% sample(worker.ID, sample.size))]
} else{
  # full sample
  reg.table[, subsample := TRUE]
}

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
if (across.firm.interactions){
  across_firm_interact <- paste0(" + i(company.type.1, exp_group_", types, "_", "1",  ", ref=\"DOMESTIC\"", ", ref2 = \"[-Inf,0]\")", collapse = "")
} else {
  across_firm_interact <- ""
}

# controls
controls <- "+ log(company.group.size) + i(job_no_trunc, ref=\"1\")" 
if (across.firm.interactions) {
   controls <- paste(controls, "+ i(company.type.1, i.job_no_trunc , ref=\"DOMESTIC\", ref2=\"1\")") ##"+ log(company.group.size)"
}  


##################################################################################
#### Main regression analysis: Non-linear returns #######

reg_main <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                      within_firm,
                                      within_firm_interact,
                                      within_firm_job,
                                      across_firm, 
                                      across_firm_interact,
                                      controls,
                                      "",
                                      "| ",
                                      # fixed effects
                                      "worker.ID ",
                                      "+ OG_BEID",
                                      ""
                                      )),
                cluster="worker.ID",
                ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
                nthreads=7,
                data=reg.table[subsample==TRUE])

##### Print table ####
etable(reg_main, 
       file=paste0(map_output_here, "reg_wages_non_linear_", ifelse(across.firm.interactions, "w_", "wo_"), "across", ".tex"),  
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
       dict = c(company.type.1 = "", MNE = "MNE", INT_COMP = "International firm", tenure_group_OGBEID_spell = "Within company tenure",
                exp_group_MNE_1 = "MNE experience", exp_group_INT_COMP_1 = "International firm experience", exp_group_DOMESTIC_1 = "Domestic company experience",
                `log(company.group.size.2)` = "log(company size)", job_no_trunc = "Employer number"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (worker level) in parantheses."),
       replace = TRUE)

##### Run graphical analysis ####
###### A) Mover wage growth #####
if (across.firm.interactions){
  pdf(file=paste0(map_output_here, "coefplot_across_firm_w_across.pdf"), width=2.10*5.5, height=2.97*5.5)
  
  par(mfrow=c(length(types),2))
  
  # within domestic firm
  # total
  estimates <- total.general.prem(reg_main, exp.types = types, k=1)
  company.type.plot(estimates, type="Total", legend.add = FALSE, only.legend = FALSE, highlight.MNE = TRUE)
  
  # empty plot with legend
  company.type.plot(estimates, type="Total", legend.add = FALSE, only.legend = TRUE, highlight.MNE = TRUE)
  
  # within other types than domestic
  for (type in types){
    if (type=="DOMESTIC") next
    # Total
    estimates <- total.company.type(reg_main, company.type= type, exp.types=types, k=1)
    company.type.plot(estimates, type="Total", legend.add = FALSE, only.legend = FALSE, highlight.MNE = TRUE)
    
    # Also output estimates to a .txt file
    sink(paste0(map_output_here, "coefs_across_firm_w_across_", type, "_total.txt"))
    estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
    sink()
    
    
    # Excess
    estimates <- excess.company.type(reg_main, company.type = type, exp.types=types, k=1)
    company.type.plot(estimates, type="Excess", legend.add = FALSE, only.legend = FALSE, highlight.MNE = TRUE)
    
    # Also output estimates to a .txt file
    sink(paste0(map_output_here, "coefs_across_firm_w_across_", type, "_excess.txt"))
    estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
    sink()
  }
  
  dev.off()
} else{
  # black and white (paper)
  pdf(file=paste0(map_output_here, "coefplot_across_firm_wo_across_bw.pdf"), width=16*0.7, height=9*0.7)
  estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
  company.type.plot(estimates, type="Total", legend.add = TRUE, 
                    legend.names = c("MNE", "Int. firm"), 
                    only.legend = FALSE, title = "", graycolors = TRUE)
  dev.off()
  
  # in color (presentation)
  pdf(file=paste0(map_output_here, "coefplot_across_firm_wo_across_col.pdf"), width=16*0.7, height=9*0.7)
  estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
  company.type.plot(estimates, type="Total", legend.add = TRUE, 
                    legend.names = c("MNE", "Int. firm"), 
                    only.legend = FALSE, title = "", graycolors = FALSE, highlight.MNE = TRUE)
  dev.off()
  
  # Also output estimates to a .txt file
  sink(paste0(map_output_here, "coefs_across_firm_wo_across.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
  
}

rm(estimates)

###### B) Stayer wage growth ###### 
if (across.firm.interactions){
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_w_across.pdf"), width=4*3.2, height=3*3.2)
  estimates <- stayer.wages.estimates(reg_main, company.types=types, k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE)
  dev.off()
  
  # Also output estimates to a .txt file
  sink(paste0(map_output_here, "coefs_stayer_wages_w_across.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
  
} else{
  # black and white (paper)
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_wo_across_bw.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                    legend.names= c("MNE", "Int. firm"), graycolors=TRUE)
  dev.off()
  
  # color (presentation)
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_wo_across_col.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                    legend.names= c("MNE", "Int. firm"), graycolors=FALSE)
  dev.off()
  
  # Also output estimates to a .txt file
  sink(paste0(map_output_here, "coefs_stayer_wages_wo_across.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
}
rm(estimates)

##################################################################################
#### Retrieve fixed effect estimates #####
# this function picks a reference in the second fe dimension (OG_BEID) --> the first OG_BEID in the data. Above I resort the data.
## retrieve fe estimates relative to domestic OG_BEID 
fes <- fixef(reg_main, sorted=TRUE, nthreads=7, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
# let's store the manipulated results in a list
main_reg_model <- list()
##### Company fixed effects #####
### save estimates in table, mark reference, identify switchers
main_reg_model$firm.fes.table <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OG_BEID=fes$OG_BEID, reference=FALSE)
main_reg_model$firm.fes.table[which(main_reg_model$firm.fes.table$OG_BEID==names(fes$OG_BEID)[which(fes$OG_BEID==0)]), reference := TRUE]
main_reg_model$firm.fes.table <- merge(main_reg_model$firm.fes.table, 
                        reg.table[, .(company.type.1=unique(company.type.1), company.group.size=mean(company.group.size)), by=c("OG_BEID")], 
                        by=c("OG_BEID"))
### Identify switching firms
main_reg_model$firm.fes.table[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
#firm.fes.table[company.type.1=="MNE", uniqueN(OG_BEID), by=company.type.1.change] # 37% of MNEs change the type.
### create panel version of company fes as well
main_reg_model$firm.fes.panel <- merge(reg.table[, c("worker.ID","year", "Nemployer.year", "OG_BEID", "company.type.1", "first_job", "company.group.size")],
                                             main_reg_model$firm.fes.table[, !c("company.group.size")], by=c("OG_BEID", "company.type.1"))
### calculate per group means, as employment weighted average (average employment of company, thus ignoring company entries and exits)
main_reg_model$firm.fes.means <- main_reg_model$firm.fes.table[company.type.1.change==FALSE, .(fe_OG_BEID_mean=sum(fe_OG_BEID*company.group.size)/sum(company.group.size)), by=company.type.1]
#### in 06C I perform a bootstrapped t-test on the difference

##### Worker fixed effects #####
# Also store worker fes. reference is in OG_BEIDs, not worker.IDs
main_reg_model$worker.fes.table <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID, reference=FALSE)
main_reg_model$worker.fes.panel <- merge(main_reg_model$worker.fes.table, reg.table[, c("worker.ID","year", "Nemployer.year", "OG_BEID", "company.type.1", "company.group.size")], by=c("worker.ID"))
### Identify switching firms
main_reg_model$worker.fes.panel[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
rm(fes)

##### add model parameters for 04C_wage_paths.R #####
main_reg_model$fml <- reg_main$fml # formula
main_reg_model$coefs <- reg_main$coefficients
main_reg_model$vcov <- vcov(reg_main)

##################################################################################
##### save for 04C_wage_paths.R #####
saveRDS(main_reg_model, 
        file = paste0(map_data_analysis, "step4/main_model", fifelse(across.firm.interactions, "_w_across", "_wo_across"), ".rds"),
        compress=TRUE)
gc()
##################################################################################
#### "Back of the envelope calculations" #####
# Here I want to calculate the additional contribution of MNEs to the total (residual) wage bill
if (back.of.envelope){
  ##### Get relevant overall data matrix and coefficients #####
  # get dummy matrix
  X <- model.matrix(reg_main)
  ## subset to relevant coloumns in X
  nn_internal <- grep("company.type.1::MNE:", colnames(X)) # internal
  nn_MNE <- grep("exp_group_MNE_1::", colnames(X)) # external 
  X <- X[, c(nn_internal, nn_MNE)]
  # get coefficients 
  beta <- reg_main$coefficients
  ## get relevant coefficients (for external need exp_group_MNE - exp_group_DOM as coefficient)
  nn_DOM <- grep("exp_group_DOMESTIC_1::", names(beta))
  beta <- c(beta[c(nn_internal)], beta[c(nn_MNE)]-beta[c(nn_DOM)])
  ### quick check if ordering is indeed correct
  all(names(beta)==colnames(X))
  
  ##### Weigh data matrix by hours worked #####
  X <- X*reg.table[, SREGULIEREUREN+SOVERWERKUREN]
  
  ##### Get residual wage vector #####
  # Note: much quicker would be rowSums(X_full) --> but that would ignore estimates not captures in model.matrix (e.g. linear estimators of included?)
  # add fe estimates to new data.table, will use that to take out total fixed effects
  ## an issue is that merge reorders the data, so let's keep track of that
  reg.table[, ord := 1:.N]
  reg.table.2 <- merge(reg.table[, c("ord", "worker.ID", "year", "Nemployer.year", "OG_BEID", "company.type.1", "lhwage_detrended")], main_reg_model$firm.fes.table[, c("OG_BEID", "fe_OG_BEID", "company.type.1")], 
                       by=c("OG_BEID", "company.type.1"), all.x=TRUE)
  reg.table.2 <- merge(reg.table.2, main_reg_model$worker.fes.table[, c("worker.ID", "fe_workerID")],
                       by=c("worker.ID"), all.x=TRUE)
  setorderv(reg.table.2, cols="ord") # set back to initial ordering
  ### extract y vector and take out fixed effects + residuals 
  ### Note: y is already demeaned by industry-year fixed effects. So, y_res captures wages after industry-year fe's, firm fe's, company fe's and after residuals (= assuming that reg_main is an appropriate approximation of the economy) 
  y <-  reg.table.2[, lhwage_detrended]
  y_res <- y - reg_main$residuals - reg.table.2[, fe_OG_BEID]  -  reg.table.2[, fe_workerID]
  rm(y, reg.table.2) 
 
  ##### Weigh residual wage vector by hours worked #####
  y_res <- y_res*reg.table[, SREGULIEREUREN+SOVERWERKUREN]
  
  #### Quick check if composition is indeed correct ####
  # X_full <- model.matrix(reg_main)
  # X_full <- X_full*reg.table[, SREGULIEREUREN+SOVERWERKUREN]
  # beta_full <- reg_main$coefficients
  # sum((colSums(X_full)*beta_full)/sum(y_res)) 
  
  ##### Estimate excess MNE contribution per year #####
  # store new positions of internal and external contributions
  nn_internal <- grep("company.type.1::MNE:", colnames(X)) # internal
  nn_external <- grep("exp_group_MNE_1::", colnames(X)) # external 
  
  # create matrix for outputs 
  res <- matrix(data=NA, nrow=reg.table[, length(unique(year))], ncol=3)
  res[, 1] <- reg.table[, sort(unique(year))]
  colnames(res) <- c("year", "internal_contr", "external_contr")
  # loop through years
  for (r in 1:nrow(res)){
    current_year <- res[r, 1]
    # subset X and y to relevant rows
    rws <- reg.table[year==current_year, which=TRUE] # get rows in reg.table
    X_current <- X[rws, ]
    y_res_current <- y_res[rws]
    
    # predict contributions and get total residual wage bill
    ## first take colsums of X_current to get total allocation (weighted by hours worked), then predict using beta vecor
    contributions <- colSums(X_current)*beta
    ## total wages paid
    wage_bill <- sum(y_res_current)
    
    # calculate and store internal and external contributions as shares in total residual wage bill
    res[r, 2] <- sum(contributions[nn_internal])/wage_bill
    res[r, 3] <- sum(contributions[nn_external])/wage_bill
  }
  
  ##### Plot the result #####
  # create a long format data.table first (easier input for ggplot)
  tt <- melt(as.data.table(res), id.vars="year")
  tt[variable=="external_contr", variable := "Excess MNE experience value at outside firm"]
  tt[variable=="internal_contr", variable := "Excess MNE wage growth"]
  
  ggplot(data=tt[year>=2014,], aes(x=year, y=value, fill=variable)) + 
    geom_area() + 
    scale_x_continuous(breaks=reg.table[, sort(unique(year))]) + 
    scale_y_continuous(labels=scales::percent) + 
    xlab("Year") + ylab("Contribution to total (residual) income") + 
    theme_bw()  +
    scale_fill_viridis(discrete=TRUE, alpha=1, begin=0.4, end=0.9, direction=1) +
    theme(legend.title = element_blank(), legend.position="bottom")
  
  # save plot
  ggsave(filename=paste0(map_output_here, "plot_back_of_envelope_calcs.pdf"), last_plot(),
         width=16*0.8, height=9*0.8, scale=1)
		 
  ##### Output table ######
  res <- cbind(res, rowSums(res[, 2:3]))
  colnames(res)[ncol(res)] <- "total"
  write.table(res, file=paste0(map_output_here, "back_of_envelope.txt"), row.names = FALSE, col.names = TRUE)
}