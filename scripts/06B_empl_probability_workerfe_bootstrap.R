##################################################################################
##### Employment probability analysis: worker fe interactions ##### 
# This calculates the workerfe interaction model and implements a bootstrap procedure using bootstrapped worker fe estimates
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step6/"))) dir.create(paste0(map_data_analysis, "step6/")) # to store bootstrap iterations
if (!dir.exists(paste0(map_data_analysis, "step6/workerfe_bootstrap/"))) dir.create(paste0(map_data_analysis, "step6/workerfe_bootstrap/")) # to store bootstrap iteration

if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/cohort/"))) dir.create(paste0(map_output, "analysis/cohort/"))
if (!dir.exists(paste0(map_output, "analysis/cohort/by_worker_fe/"))) dir.create(paste0(map_output, "analysis/cohort/by_worker_fe/"))
map_output_here <- paste0(map_output, "analysis/cohort/by_worker_fe/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) # main wage panel
ability_model <- readRDS(paste0(map_data_analysis, "step4/worker_fe/worker_FE_interaction_model.rds")) # get worker fixed effects


##################################################################################
##################################################################################
#### prepare data ####
##### add workerfes from wage ability model to reg.table #######
# retrieve fes
fes <- fixef(ability_model, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
# extract worker fixed effects and merge
reg.table <- merge(reg.table,
                   data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID),
                   by="worker.ID")
rm(fes)

##### clean data #####
# truncate job.number
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

# identify labor market entry cohort for each worker
reg.table[, entry_cohort := min(year), by=worker.ID]

###### Get years since labor market entry ##### 
reg.table[, labor_exp_years := year-entry_cohort]
reg.table[, labor_exp_group_2 := cut(labor_exp_years, breaks=c(-Inf, seq(0,10,1), Inf), include.lowest=TRUE)]

##################################################################################
#### Analysis settings #####
sample.use <- FALSE
sample.size <- 0.1*10^6

##### use full sample or subsample with size sample.use? ####
if (sample.use){
  # use a  random sample of workers for quicker calculations
  set.seed(7854)
  reg.table[, subsample := (worker.ID %in% sample(worker.ID, sample.size))]
} else{
  # full sample
  reg.table[, subsample := TRUE]
}

##### set formula parts #### 
frm <-  paste0("+ i(labor_exp_group_2, ref=\"[-Inf,0]\")",
               "+ i(labor_exp_group_2, fe_workerID, ref=\"[-Inf,0]\")",
               "+ log(company.group.size)",
               "+ i(job_no_trunc)")
fixedeffs <- paste0("|",
                    "+ worker.ID",
                    "+ OG_BEID",
                    "+ year^NACE_LETTER") #year^NACE_LETTER - slows down calculations considerably without meaningful impact on results

##################################################################################
#### estimate main model to get coefficients ####

# run regression on full sample with clustered standard errors (for comparison)
main <- feols(as.formula(paste0("I(company.type.1==\"MNE\") ~ ",
                                  frm, 
                                  fixedeffs)),
                #vcov = "iid",
                cluster="worker.ID",
                ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
                nthreads=7,
                data=reg.table[subsample==TRUE])

##################################################################################
#### Run bootstrap #####

# save main worker fixed effects
reg.table[, fe_workerID_main := fe_workerID]

#### run bootstrap #####
# note: to run a blockbootstrap for all coefficients, we need to resample workers in each bootstrap
# where individual workers are oversampled
# the same was done for the bootstrapped workerfe estimates on the same draws

# set a starting seed: to get same draws as in for worker fes, set same same seed
# set initial seed: will update seed in each iteration for easier replication in steps (bootstrap takes long to long)
s <- 4934 # seeds will be (4934+1):(4934+100)

bootstraps <- 100 #100

# matrix of bootstrap coefficients
bcoefs <- matrix(NA, nrow=bootstraps, ncol=length(main$coefficients))
colnames(bcoefs) <- names(main$coefficients)

# loop through bootstraps, add bootstrapped worker fes, estimate model coefficients
timer <- Sys.time()
for (bootstrap in 1:bootstraps){
  reg.table[, fe_workerID := NULL]

  # message
  cat(paste0(Sys.time(), ": starting ", bootstrap, "/", bootstraps), "\n")  
  
  ##### Add bootstrapped worker fes ####
  # load new worker fixed effects and add to reg.table
  fes  <- readRDS(paste0(map_data_analysis, "step5/bootstrap_workerfe/", "fes_", bootstrap, ".rds"))
  reg.table <- merge(reg.table, fes$workerfes, by="worker.ID", all.x=TRUE) # writes NA for missing fe_workerIDs
  rm(fes)
  
  # adjust subsample to avoid missings
  reg.table[, subsample_boot := subsample]
  reg.table[is.na(fe_workerID), subsample_boot := FALSE]
  
  ##### create block bootstrap sample #####
  # update seed 
  set.seed(s+bootstrap)
  # get worker IDs in fe bootstrap sample
  sids <- reg.table[subsample_boot==TRUE, unique(worker.ID)]
  # draw worker IDs with replacement
  sids <- sample(sids, length(sids), replace = TRUE)
  # create data.table with replacement from sids
  ## logic is to repeatedly add reg.table to itself until all occurrences of worker.ID in sids are fulfilled
  tt_boot <- reg.table[worker.ID %in% unique(sids),]
  sids <- sids[duplicated(sids)]
  while (sum(duplicated(sids)) > 0){
    tt_boot <- rbindlist(list(tt_boot, reg.table[worker.ID %in% unique(sids),]))
    sids <- sids[duplicated(sids)]
  }
  rm(sids)
  
  
  ##### run regression on bootstrap sample #####
  cat(paste0(Sys.time(), ": running regression"), "\n")  
  
  breg <- feols(as.formula(paste0("I(company.type.1==\"MNE\") ~ ",
                                  frm, 
                                  fixedeffs)),
                vcov = "iid",
                nthreads=7,
                data=tt_boot[subsample_boot==TRUE])
  
  # save coefficients
  saveRDS(breg$coefficients, file = paste0(map_data_analysis, "step6/workerfe_bootstrap/coefs_", bootstrap, ".rds"))
  
  # add coefficients to matrix
  bcoefs[bootstrap, ] <- breg$coefficients
  
  # clean up
  rm(breg, tt_boot)
}
timer <- Sys.time() - timer
timer  # 4.15 hours

# readd main worker fes
reg.table[, fe_workerID := fe_workerID_main]
reg.table[, c("fe_workerID_main", "subsample_boot") := NULL]

##### Calculate bootstrapped vcov matrix ####
bvcov <- cov(bcoefs)

##### Save bootstrapped model #####
main$bcoefs <- bcoefs
main$bvcov <- bvcov
saveRDS(main, file = paste0(map_data_analysis, "step6/workerfe_bootstrap/cohort_workerfe_model.rds"))



##################################################################################
#### Outputs #####

##### Output table #####
etable(main, 
       vcov = main$bvcov,
       title="MNE vs. other companies (worker fe interactions)", 
       file=paste0(map_output_here, "reg_cohort_workerfe_bstrap.tex"),
       label="tab:cohort_workerfe", 
       depvar = TRUE,
       digits = "r4",
       fitstat = c("n", "r2"),
       digits.stats = "r4",
       fixef_sizes = TRUE,
       fixef_sizes.simplify = TRUE,
       powerBelow = -6,
       float = TRUE,
       dict = c(company.type.1 = "", MNE = "MNE", INT_COMP = "International Company", tenure_group_OGBEID_spell = "years in company",
                exp_group_MNE_1 = "MNE experience", exp_group_INT_COMP_1 = "International Company experience", exp_group_DOMESTIC_1 = "Domestic Company experience",
                `log(company.group.size)` = "log(company size)", job_no_trunc = "Employer number", fe_workerID = "worker fe", worker.ID = "worker", OG_BEID = "company",
                labor_exp_group_2 = "Labor market experience"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       order = c("years in company", "Domestic Company experience", "International Company experience", "MNE experience"),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Bootstrapped standard errors clustered at worker level in parantheses."),
       replace = TRUE)


##### Graphical analysis #####
###### identify worker fe groups #####
worker.fes.table <- reg.table[, .(fe_workerID=fe_workerID[1]), by=worker.ID]
worker.fes.table[, fe_workerID_percentile := cut(fe_workerID, breaks=c(-Inf,quantile(fe_workerID, probs=c(1/4, 3/4)), Inf), labels=3:1, include.lowest=TRUE)]
percentile_means <- worker.fes.table[, .(fe_workerID = mean(fe_workerID)), by=fe_workerID_percentile][order(fe_workerID_percentile)]
percentile_means[, fe_workerID_percentile := c("Bottom", "Middle", "Top")]
percentile_means[, cutoffs := c("<=0.25", "0.25-0.75", ">0.75")]


# save in file
sink(paste0(map_output_here, "workerfe_groups.txt"))
percentile_means
sink()

####### probability of entry by worker fe group #####
# add percentiles to reg.table --> 3 is Bottom, 1 is top
reg.table <- merge(reg.table, worker.fes.table[, -c("fe_workerID")], by="worker.ID")
rm(worker.fes.table)

## entries into MNEs
entries <-  reg.table[labor_exp_group_2=="[-Inf,0]" & entry.obs==TRUE, .(entries=mean(company.type.1=="MNE"), sd=sd(company.type.1=="MNE")), by=c("fe_workerID_percentile")][order(fe_workerID_percentile),]
entries[, fe_workerID_percentile := c("Bottom", "Middle", "Top")]

# save in file
sink(paste0(map_output_here, "entries_by_workerfe_group.txt"))
entries
sink()

###### create plots ######
####### without clustered standard errors ####
# get estimates using function
estimates <- cohort.estimates(main, workerfe_interaction = TRUE, percentile_means = percentile_means[fe_workerID_percentile != "Middle",][order(-fe_workerID)], bvcov=NA)
# creat plot using function
pdf(file=paste0(map_output_here, "coefplot_cohort_MNE_workerfe_cluster.pdf"), width=16*0.7, height=9*0.7)
cohort.plot(estimates, workerfe_interaction = TRUE, horizontal = TRUE, legend.names = c("High-ability", "Low-ability"), tit=NA, graycolors=TRUE)
dev.off()

# Also output estimates to a .txt file
sink(paste0(map_output_here, "coefs_cohort_MNE_workerfe_cluster.txt"))
estimates.table(estimates, df = degrees_freedom(main, type="t"))
sink()

####### with block bootstrap standard errors ####
# get estimates using function
estimates <- cohort.estimates(main, workerfe_interaction = TRUE, percentile_means = percentile_means[fe_workerID_percentile != "Middle",][order(-fe_workerID)], bvcov=main$bvcov)
# create plot using function (black and white for paper)
pdf(file=paste0(map_output_here, "coefplot_cohort_MNE_workerfe_bstrap.pdf"), width=16*0.7, height=9*0.7)
cohort.plot(estimates, workerfe_interaction = TRUE, horizontal = TRUE,
            legend.names = c("High-ability", "Low-ability"), tit=NA, graycolors=TRUE)
dev.off()
# create plot using function (color for presentation)
pdf(file=paste0(map_output_here, "coefplot_cohort_MNE_workerfe_bstrap_col.pdf"), width=16*0.7, height=9*0.7)
cohort.plot(estimates, workerfe_interaction = TRUE, horizontal = TRUE,
            legend.names = c("High-ability", "Low-ability"), tit=NA, graycolors=FALSE)
dev.off()


# Also output estimates to a .txt file
sink(paste0(map_output_here, "coefs_cohort_MNE_workerfe_bstrap.txt"))
estimates.table(estimates, df = degrees_freedom(main, type="t"))
sink()

# Calculate odds ratios and save in .txt file 
estimates$odds <- estimates$coefs
l <- 0
for (i in colnames(estimates$coefs)){
  l <- l+1
  estimates$odds[, l] <- (entries[fe_workerID_percentile==i, entries]+estimates$coefs[, l])/entries[fe_workerID_percentile==i, entries]
  
}
sink(paste0(map_output_here, "odd_ratios_cohort_MNE_workerfe.txt"))
estimates$odds
sink()
