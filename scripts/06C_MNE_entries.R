##################################################################################
##### MNE entrant analysis ########################
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step6/"))) dir.create(paste0(map_data_analysis, "step6/")) # to store model

if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/entries/"))) dir.create(paste0(map_output, "analysis/entries/"))
map_output_here <- paste0(map_output, "analysis/entries/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) # main matched employer-employee data
ability_model <- readRDS(paste0(map_data_analysis, "step4/worker_fe/worker_FE_interaction_model.rds")) # get worker fixed effects

##################################################################################
##################################################################################
#### prepare data ####
##### add workerfes from wage ability model to reg.table #######
# retrieve fes
fes <- fixef(ability_model, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
# extract worker fixed effects and standardise
workerfes <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID)
workerfes[, fe_workerID_stdzd := (fe_workerID - mean(fe_workerID))/sd(fe_workerID)]

# add to reg.table
reg.table <- merge(reg.table,
                   workerfes,
                   by="worker.ID")
rm(fes, workerfes)



###### identify entry positions #####
# first order again to be sure
setorderv(reg.table, cols=c("worker.ID", "year", "Nemployer.year"))

# then lead relevant vars
reg.table[, c("OG_BEID.lag", "NACE_2digit.lag", "company.type.1.lag", "company.group.size.lag", "NACE_LETTER.lag") := shift(.(OG_BEID, NACE_2digit, company.type.1, company.group.size, NACE_LETTER), n=c(-1), type="lead"), by=worker.ID]

# Identify first and last obs of worker to correct entries
reg.table[, n := 1:.N, by=worker.ID]
reg.table[, entry.obs := n==1]
reg.table[, n := NULL]

# find entry positions and correct for labor market entrant
reg.table[, entry := OG_BEID!=OG_BEID.lag]
reg.table[entry.obs==TRUE, entry := TRUE]

# identify cross industry entries (2 digit)
reg.table[entry == T, xindustry := NACE_2digit != NACE_2digit.lag]
reg.table[, NACE_2digit.lag := NULL]

# identify cross industry entries (1 digit)
reg.table[entry == T, xindustry_LETTER := NACE_LETTER != NACE_LETTER.lag]
reg.table[, NACE_2digit.lag := NULL]

# add job number
reg.table[, job.number.2 := cumsum(entry), by=worker.ID]
reg.table[, job_no_trunc := fifelse(job.number.2>3, 4, job.number.2)]

##### Calculate industry experience ######


##### standardize human capital measures #####
types <- reg.table[, unique(company.type.1)]

reg.table[, paste0("exp_years_",types,"_1_stdzd") := lapply(.SD, function(x) (x-mean(x))/sd(x)), .SDcols=paste0("exp_years_", types, "_1")]


##################################################################################

##### x-industry probability model (no bootstrap) ####
reg_xindustry <- list()
reg_xindustry[[1]] <- feols(fml = as.formula(paste0("xindustry ~ ",
                                                    "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                                    "|",
                                                    "year")), data=reg.table[entry == TRUE & entry.obs==FALSE,],
                            vcov = "iid")
reg_xindustry[[2]] <- feols(fml = as.formula(paste0("xindustry ~ ",
                                                    "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                                    "+ log(company.group.size) + log(company.group.size.lag)",
                                                    "|",
                                                    "year")), data=reg.table[entry == TRUE & entry.obs==FALSE,],
                            vcov = "iid")
reg_xindustry[[3]] <- feols(fml = as.formula(paste0("xindustry ~ ",
                                                    "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                                    "+ log(company.group.size) + log(company.group.size.lag) + fe_workerID_stdzd",
                                                    "|",
                                                    "year")), data=reg.table[entry == TRUE & entry.obs==FALSE,],
                            vcov = "iid")
etable(reg_xindustry)


##### MNE entry probability model ####
###### first estimate coefficients ######
reg <- list()
reg[[1]] <- feols(fml = as.formula(paste0("I(entry==TRUE & company.type.1 == \"MNE\") ~ ",
                                          "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                          "+ fe_workerID_stdzd", 
                                          "|",
                                          "NACE_LETTER^year")), data=reg.table[entry == TRUE & entry.obs==FALSE,],
                  vcov = "iid")
reg[[2]] <- feols(fml = as.formula(paste0("I(entry==TRUE & company.type.1 == \"MNE\") ~ ",
                                          "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                          "+ fe_workerID_stdzd",
                                          "+ log(company.group.size)",
                                          "|",
                                          "NACE_LETTER^year")), data=reg.table[entry == TRUE & entry.obs==FALSE,],
                                          vcov = "iid")
reg[[3]] <- feols(fml = as.formula(paste0("I(entry==TRUE & company.type.1 == \"MNE\") ~ ",
                                     "exp_years_MNE_1_stdzd + exp_years_INT_COMP_1_stdzd + exp_years_DOMESTIC_1_stdzd", 
                                     "+ fe_workerID_stdzd",
                                     "+ log(company.group.size)",
                                     "|",
                                     "NACE_LETTER^year")), data=reg.table[entry == TRUE & entry.obs==FALSE,],
                                      vcov = "iid")
etable(reg)




##### estimate standard errors through bootstrap ####
# save original standardized worker fixed effects
reg.table[, fe_workerID_stdzd_orig := fe_workerID_stdzd]
reg.table[, fe_workerID_orig := fe_workerID]

# number of bootstrapped worker fixed effect estimates
bootstraps <-  100 

# set a starting seed: to get same draws as in for worker fes, set same same seed
# set initial seed: will update seed in each iteration for easier replication in steps (bootstrap takes long to long)
s <- 4934 # seeds will be (4934+1):(4934+100)

# matrix of bootstrap coefficients
bcoefs <- list()
bcoefs[[1]] <- matrix(NA, nrow=bootstraps, ncol=length(reg[[1]]$coefficients))
colnames(bcoefs[[1]]) <- names(reg[[1]]$coefficients)
bcoefs[[2]] <- matrix(NA, nrow=bootstraps, ncol=length(reg[[2]]$coefficients))
colnames(bcoefs[[2]]) <- names(reg[[2]]$coefficients)
bcoefs[[3]] <- matrix(NA, nrow=bootstraps, ncol=length(reg[[3]]$coefficients))
colnames(bcoefs[[3]]) <- names(reg[[3]]$coefficients)

# loop through bootstraps, add bootstrapped worker fes, standardize, estimate model coefficients
timer <- Sys.time()
for (bootstrap in 1:bootstraps){
  # delete previous worker fixed effect estimates 
  reg.table[, fe_workerID_stdzd := NULL]
  reg.table[, fe_workerID := NULL]
  
  # message
  cat(paste0(Sys.time(), ": starting ", bootstrap, "/", bootstraps), "\n")  
  
  ##### Add bootstrapped worker fes ####
  # load new worker fixed effects
  fes  <- readRDS(paste0(map_data_paper2, "step5/bootstrap_workerfe/", "fes_", bootstrap, ".rds"))
  # extract worker fixed effects and standardise
  fes$workerfes[, fe_workerID_stdzd := (fe_workerID - mean(fe_workerID))/sd(fe_workerID)]
  # add to reg.table
  reg.table <- merge(reg.table, fes$workerfes, by="worker.ID", all.x=TRUE) # writes NA for missing fe_workerIDs
  rm(fes)
  
  # adjust subsample to avoid missings
  reg.table[, subsample_boot := TRUE]
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
  
  
  ##### run regressions on bootstrap sample #####
  cat(paste0(Sys.time(), ": running regression"), "\n")  
  
  
  breg1 <-  feols(fml = as.formula(paste0("I(entry==TRUE & company.type.1 == \"MNE\") ~ ",
                                          "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                          "+ fe_workerID_stdzd", 
                                          "|",
                                          "NACE_LETTER^year")), data=tt_boot[subsample_boot==TRUE & entry == TRUE & entry.obs==FALSE,],
                  vcov = "iid")
  
  breg2 <- feols(fml = as.formula(paste0("I(entry==TRUE & company.type.1 == \"MNE\") ~ ",
                                            "exp_years_MNE_1+ exp_years_INT_COMP_1 + exp_years_DOMESTIC_1", 
                                            "+ fe_workerID_stdzd",
                                            "+ log(company.group.size)",
                                            "|",
                                            "NACE_LETTER^year")), data=tt_boot[subsample_boot==TRUE & entry == TRUE & entry.obs==FALSE,],
                    vcov = "iid")
  breg3 <- feols(fml = as.formula(paste0("I(entry==TRUE & company.type.1 == \"MNE\") ~ ",
                                            "exp_years_MNE_1_stdzd + exp_years_INT_COMP_1_stdzd + exp_years_DOMESTIC_1_stdzd", 
                                            "+ fe_workerID_stdzd",
                                            "+ log(company.group.size)",
                                            "|",
                                            "NACE_LETTER^year")), data=tt_boot[subsample_boot==TRUE & entry == TRUE & entry.obs==FALSE,],
                    vcov = "iid")
  
  # add coefficients to matrix
  bcoefs[[1]][bootstrap, ] <- breg1$coefficients
  bcoefs[[2]][bootstrap, ] <- breg2$coefficients
  bcoefs[[3]][bootstrap, ] <- breg3$coefficients
  
  # clean up
  rm(breg1, breg2, breg3, tt_boot)
}
timer <- Sys.time() - timer
timer  # 4.15 hours

# readd main worker fes
reg.table[, fe_workerID_stdzd := fe_workerID_stdzd_orig]
reg.table[, fe_workerID := fe_workerID_orig]
reg.table[, c("fe_workerID_stdzd_orig", "fe_workerID_orig") := NULL]

##### Calculate bootstrapped vcov matrix ####
bvcov <- list()
bvcov[[1]] <- cov(bcoefs[[1]])
bvcov[[2]] <- cov(bcoefs[[2]])
bvcov[[3]] <- cov(bcoefs[[3]])


##### Save bootstrapped model #####
reg[[1]]$bcoefs <- bvcov[[1]]
reg[[2]]$bcoefs <- bvcov[[2]]
reg[[3]]$bcoefs <- bvcov[[3]]
saveRDS(reg, file = paste0(map_data_analysis, "step6/MNE_entry_model.rds"))

##################################################################################
#### Outputs #####

##### Output tables #####
etable(reg, 
       vcov = bvcov,
       title="Entries into MNEs, dependent on experience and career ability", 
       file=paste0(map_output_here, "reg_entry_bstrap.tex"),
       label="tab:entry", 
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

etable(reg_xindustry, 
       title="Cross-industry moves", 
       file=paste0(map_output_here, "reg_xindustry.tex"),
       label="tab:reg_xindustry", 
       cluster = "worker.ID",
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
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1"),
       replace = TRUE)
