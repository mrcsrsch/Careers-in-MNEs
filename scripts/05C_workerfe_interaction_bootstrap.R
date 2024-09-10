##################################################################################
### Wage analysis with differential returns by worker fes: bootstrap (standard errors + Combes et al method) #####
# This script implements a bootstrap around the worker fe interacted model (using the de la Roca 2017 algorithm)
# The fixed effect estimates are also used for a bootstrapped Combes et al 2012 calculation
# Warning: on regular CBS servers this takes very long to run (ca. 6 days)
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")
if (!require("lfe")) install.packages("lfe"); library("lfe") # to find connected set
if (packageVersion("lfe")!="2.8.7.1") warning("Analysis run in lfe version 2.8.7.1 - consider up- or downgrading")
if (!require("haven")) install.packages("haven"); library("haven") # save .dta (STATA) file
if (packageVersion("haven")!="2.5.0") warning("Analysis ran in haven version 2.5.0 - consider up- or downgrading")

#### sourcing scripts #####

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step5/"))) dir.create(paste0(map_data_analysis, "step5/")) # to store regression object for further analysis
if (!dir.exists(paste0(map_data_analysis, "step5/bootstrap_workerfe/"))) dir.create(paste0(map_data_analysis, "step5/bootstrap_workerfe/")) # to store regression object for further analysis

map_output_here <- paste0(map_data_analysis, "step5/bootstrap_workerfe/") # for bootstrap outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) # general regression table
workerfe_model <- readRDS(paste0(map_data_analysis, "step5/worker_fe/worker_FE_interaction_model.rds")) # worker fes from worker fe interacted model (as initial guess)

##################################################################################
##################################################################################
#### prepare data ####

# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

# identify company type changes
reg.table[, company.type.1.change := uniqueN(company.type.1)==1, by=OG_BEID]

# identifier for MNEs
reg.table[, MNE := company.type.1=="MNE"]


#### pick reference firm #####
# to get standard errors on the fixed effect estimates it's necessary to keep the reference firm stable. Otherwise variance of bootstrapped individual fixed effects make no sense. 
# let's pick a firm that is likely central in the network, i.e. has the most observations
ref_firm <- reg.table[, .N, by=OG_BEID][, pick := N==max(N)][pick==TRUE, OG_BEID]
reg.table <- rbindlist(list(reg.table[OG_BEID==ref_firm, ], reg.table[OG_BEID != ref_firm, ]))

# save order for below
reg.table[, order := 1:.N]


##### Add first set of worker fes to reg.table (from 04C - main workerfe interaction model) #####
# adjust workerfes to new ref_new
reg.table <- merge(reg.table, workerfe_model$worker.fes.table[, c("worker.ID", "fe_workerID")], by="worker.ID")
rm(workerfe_model)
setorderv(reg.table, cols="order")


#### clean up to save memory ##### 
# only keep necesary vars in reg.table
reg.table <- reg.table[, c("order", "worker.ID", "OG_BEID", "Nemployer.year", "year", "company.group.size", 
                           "company.type.1", "company.type.1.change", "MNE", "lhwage_detrended", "job_no_trunc",
                           "exp_group_MNE_1", "exp_group_INT_COMP_1", "exp_group_DOMESTIC_1", "tenure_group_OGBEID_spell", "fe_workerID")]
gc()
##################################################################################

#### Settings of analysis ####
#### Formula #####
across.firm.interactions <- FALSE 

# save types for formula
types <- reg.table[, unique(company.type.1)]

##### Set formula parts ####

### within firm tenure (growth)
within_firm <- paste0(" + i(tenure_group_OGBEID_spell, ref=\"[-Inf,1]\")")
within_firm_interact <- paste0(" + i(company.type.1, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\")")
# interaction with worker fes
within_firm_workerfe <- paste0(" + i(tenure_group_OGBEID_spell, fe_workerID, ref=\"[-Inf,1]\")") # tenure_group_OGBEID_spell x fe_workerID
within_firm_interact_workerfe <- paste0(" + i(company.type.1, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\") * fe_workerID - fe_workerID") # company type x tenure_group_OGBEID_spell x fe_workerID

#### within firm tenure by job number (general effect) + interaction with workerfe
within_firm_job_workerfe <- paste0(" + i(tenure_group_OGBEID_spell, i.job_no_trunc, ref=\"[-Inf,1]\", ref2=\"1\") * fe_workerID - fe_workerID") # tenure_group_OGBEID_spell x job number x workerfe (differential returns by job number and skill level)


### across firm experience returns
across_firm <- paste0(" + i(exp_group_", types, "_", "1", ", ref = \"[-Inf,0]\")", collapse = "")
# interaction with worker fes
across_firm_workerfe <- paste0(" + i(exp_group_", types, "_", "1,", "fe_workerID", ", ref = \"[-Inf,0]\")", collapse = "")

if (across.firm.interactions){
  across_firm_interact <- paste0(" + i(company.type.1, exp_group_", types, "_", "1",  ", ref=\"DOMESTIC\"", ", ref2 = \"[-Inf,0]\")", collapse = "")
  # interaction with worker fes
  across_firm_interact_workerfe <- paste0(" + i(company.type.1, exp_group_", types, "_", "1",  ", ref=\"DOMESTIC\"", ", ref2 = \"[-Inf,0]\")  * fe_workerID - fe_workerID", collapse = "")
} else {
  across_firm_interact <- ""
  across_firm_interact_workerfe <- ""
}

# controls
controls <- "+ log(company.group.size) + i(job_no_trunc, ref=\"1\")"
controls_workefe <- "+ i(job_no_trunc, fe_workerID, ref=\"1\")"
if (across.firm.interactions) {
  controls <- paste(controls, "+ i(company.type.1, i.job_no_trunc , ref=\"DOMESTIC\", ref2=\"1\")") ##"+ log(company.group.size)"
}

##################################################################################
#### Bootstrap samples ##### 

##### Settings bootstrap ####
# idea: loop over bootstrap iterations. Within each bootstrap iteration prepare 3 data sets: main model, 2 basic models. Save the fixed effect estimates.
# for bootstrap: 
# 1. draw workers 
# 2. find connected set
# 3. fit model (see next)
# 4. save output for further analysis

# set initial seed: will update seed in each iteration for easier replication in steps (bootstrap takes days to run)
s <- 4934 # seeds will be (4934+1):(4934+100)

# number of bootstrap iterations
bootstraps <- 100

# store unique worker IDs (cluster level)
workerids <- reg.table[, unique(worker.ID)]

# empty matrix to track network size
metric.matrix <- matrix(NA, nrow=bootstraps, ncol=5)
colnames(metric.matrix) <- c("reps ref firm", "components", "b workers", "b firms", "b obs")

#### Settings worker fe interaction ####
# Algorithm to find worker fixed effect: 
# first set of worker fe estimate
# 0. if i>1, store old regression
# 1. run new regression, store somewhere new
# 2. store estimates for interactions, if i > 1 compare to previous run (with error margin)
## if convergence, exit and return old regression's output
# 3. extract worker fes 
# 4. update worker fes in reg.table
# 5. repeat

maxiter <- 30 # maximum iterations
coef_dist <- 1e-4 # early exit: max. euclidean distance between coef vecs in two successive iters (convergence #1)
worekrFE_dist <- 1e-3 # early exit: + max. MSE of worker fe vecs in two successive iters (convervenge #2)

timer <- Sys.time()


for (bootstrap in 1:bootstraps){
  # update seed 
  set.seed(s+bootstrap)
  
  # message
  cat(paste(Sys.time(), "iter", bootstrap, "/", bootstraps), "\n")
  
  # save metric on repetitions
  metric.matrix[bootstrap, 1] <- 0
  # initialize ref firm check
  ref_included <- FALSE
  
  # keep on drawing workers while ref_firm is not part of the largest network
  cat(paste(Sys.time(), "getting bootstrap sample"), "\n")
  
  while (ref_included==FALSE){
    # save metric on repetitions
    metric.matrix[bootstrap, 1] <- metric.matrix[bootstrap, 1]+1
    
    ###### draw bootstrap sample: clustered by worker.ID ######
    # draw worker.IDs with replacement
    sids <- sample(workerids, length(workerids), replace = TRUE)
    # create data.table with replacement from sids
    ## logic is to repeatedly add reg.table to itself until all occurrences of worker.ID in sids are fulfilled
    tt_boot <- reg.table[worker.ID %in% unique(sids),]
    sids <- sids[duplicated(sids)]
    while (sum(duplicated(sids)) > 0){
      tt_boot <- rbindlist(list(tt_boot, reg.table[worker.ID %in% unique(sids),]))
      sids <- sids[duplicated(sids)]
    }
    
    ###### find largest connected set and save metrics of bootstrap ######
    tt_boot[, connected.set := compfactor(list(f1=factor(tt_boot$OG_BEID), f2=factor(tt_boot$worker.ID)))]
    metric.matrix[bootstrap, 2] <- tt_boot[, uniqueN(connected.set)] # number of components
    tt_boot <- tt_boot[connected.set==1, !c("connected.set")]
    
    # check if largest component includes ref_firm
    ref_included <- tt_boot[, any(OG_BEID==ref_firm)]
  }
  metric.matrix[bootstrap, 3:5] <- tt_boot[, c(uniqueN(worker.ID), uniqueN(OG_BEID), .N)] # workers, companies, obs in larget component
  
  # reorder tt_boot to set ref_firm
  tt_boot <- rbindlist(list(tt_boot[OG_BEID==ref_firm, ], tt_boot[OG_BEID != ref_firm, ]))
  
  ###### Estimate worker fe interaction model through iterative procedure ######
  cat(paste(Sys.time(), "Estimating interaction model"), "\n")
  
  for (i in 1:maxiter){
    # print update
    cat(paste(Sys.time(), ":", "starting", i), "\n") 
    
    # store old regression and MSE
    if (i > 1){
      reg_before_coefs <- reg_new$coefficients
      # MSE_before <- MSE_new
      rm(reg_new)
    }
    
    # run new regression
    reg_new <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                           within_firm,
                                           within_firm_workerfe,
                                           within_firm_interact,
                                           within_firm_interact_workerfe,
                                           within_firm_job_workerfe,
                                           across_firm, 
                                           across_firm_workerfe,
                                           across_firm_interact,
                                           across_firm_interact_workerfe,
                                           controls,
                                           controls_workefe,
                                           "",
                                           "| ",
                                           # fixed effects
                                           "worker.ID ",
                                           "+ OG_BEID",
                                           ""
                                        )),
                                        vcov="iid",
                                        # mem.clean = TRUE, # save memory 
                                        lean = FALSE, # saves memory, cannot use because of fixef call
                                        nthreads=7, # use all but one core for this
                                        data=tt_boot)
 
    
    # start updating working fixed effects
    cat(paste(Sys.time(), ":", "retieving fes", i), "\n")  
    
    # first delete old worker FE estimates, but keep them somewhere for an easy MSE comparison
    tt_boot[, fe_workerID_before := fe_workerID] 
    tt_boot[, fe_workerID := NULL]
    # and retrieve new set of worker fes, add to reg.table
    fes <- fixef(reg_new, sorted=TRUE, nthreads=7, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
    tt_boot <- merge(tt_boot, data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID), by="worker.ID")
    # reorder tt_boot (keep reference firm for fe estimation the same)
    setorderv(tt_boot, cols="order")
    
    # clean up
    rm(fes)
    gc()
    
    # early exit criterion: coefficient distance < coef_dist
    if (i > 1){
      # store euclidean distance in coefficients
      eucdl <- dist(rbind(reg_before_coefs, reg_new$coefficients))
      cat(paste("Iteration", i, "with coef. distance of", eucdl, "to prev. interation"), "\n")
      
      # also store MSE of worker fes
      wfeMSE <- tt_boot[, sum((fe_workerID-fe_workerID_before)^2)]
      cat(paste("Iteration", i, "with MSE of worker fes of", wfeMSE, "to prev. interation"), "\n")
      
      
      # exit loop if possible
      if (i > 1 & eucdl <= coef_dist & wfeMSE <= worekrFE_dist) break
      
    } 

  }
  
  ###### Save result of bootstrap iteration ######
  
  ####### coefficients ########
  cat(paste(Sys.time(), "Saving coefficients"), "\n")
  
  saveRDS(list(reg_new$coefficients), file = paste0(map_output_here, "coefs_", bootstrap, ".rds"))
  
  ####### Retrieve fixed effects  ###### 
  
  cat(paste(Sys.time(), "retrieving final set of fixed effects"), "\n")
  
  fes <- fixef(reg_new, sorted=TRUE, nthreads=7, fixef.tol = .Machine$double.eps*10000)
  
  workerfes <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID)
  companyfes <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OG_BEID = fes$OG_BEID, reference = FALSE)
  companyfes[OG_BEID==ref_firm, reference := TRUE]
  
  ######## Save fixed effects #####
  saveRDS(list(workerfes=workerfes, companyfes=companyfes), file = paste0(map_output_here, "fes_", bootstrap, ".rds"))
  
  ###### clean up ####
  rm(reg_new, tt_boot, companyfes, workerfes, fes)
}
k <- reg.table[, c(1, 1, uniqueN(worker.ID), uniqueN(OG_BEID), .N)]
metric.matrix.relative <- t(apply(metric.matrix, MARGIN=1, function(x) x/k))
rm(k)

timer <- Sys.time()-timer
timer  # 5.95 days
saveRDS(list(metric.matrix=metric.matrix, metric.matrix.relative=metric.matrix.relative, timer=timer), file=paste0(map_output_here, "overview.rds"))
