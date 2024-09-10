##################################################################################
### Cluseter-Bootstrapped fixed effect estimates at the worker.ID level #####
# of main model, main model without company size control and 2 basic models (without size control and with size control)
# for bootstrapped standard errors in company fe regression (see 05B)
# and bootstrapped Combes et al 2012 method (needs to run in Stata, see 05D)

# This script runs the bootstrap models and saves the estimates 

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
if (!dir.exists(paste0(map_data_analysis, "step5/bootstrap_wo_workerfe/"))) dir.create(paste0(map_data_analysis, "step5/bootstrap_wo_workerfe/")) # to store regression object for further analysis

map_output_here <- paste0(map_data_analysis, "step5/bootstrap_wo_workerfe/") # for bootstrap outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 

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

##################################################################################
#### Set formula parts ####
across.firm.interactions <- FALSE
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
controls <- "+ i(job_no_trunc, ref=\"1\")" 
if (across.firm.interactions) {
  controls <- paste(controls, "+ i(company.type.1, i.job_no_trunc , ref=\"DOMESTIC\", ref2=\"1\")") ##"+ log(company.group.size)"
}  
## separate formula part for size model
sizecontrol <- "+ log(company.group.size)"

##################################################################################
#### Bootstrap samples ##### 

# idea: loop over bootstrap iterations. Within each bootstrap iteration prepare 3 data sets: main model, 2 basic models. Save the fixed effect estimates.
# for bootstrap: 
# 1. draw workers 
# 2. find connected set
# 3. fit model
# 4. save output for further analysis

# set seed 
set.seed(4939)

# number of bootstrap iterations
bootstraps <- 100

# store unique worker IDs (cluster level)
workerids <- reg.table[, unique(worker.ID)]

# empty matrix to track network size
metric.matrix <- matrix(NA, nrow=bootstraps, ncol=5)
colnames(metric.matrix) <- c("reps ref firm", "components", "b workers", "b firms", "b obs")


timer <- Sys.time()
for (bootstrap in 1:bootstraps){
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
  
  ###### loop through estimations ######
  for (model in c("basic", "size", "full", "fullwosize")){
    
    ####### estimate model ##### 
    cat(paste(Sys.time(), "estimating", model, "model"), "\n")
    
    
    reg_fe <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                          fifelse(model %in% c("full", "fullwosize"), within_firm, ""),
                                          fifelse(model %in% c("full", "fullwosize"), within_firm_interact, ""),
                                          fifelse(model %in% c("full", "fullwosize"), within_firm_job, ""),
                                          fifelse(model %in% c("full", "fullwosize"), across_firm, ""), 
                                          fifelse(model %in% c("full", "fullwosize"), across_firm_interact, ""),
                                          fifelse(model %in% c("full", "fullwosize"), controls, ""),
                                          fifelse(model %in% c("full", "size"), sizecontrol, ""),
                                          fifelse(model == "basic", "0", ""), # need to pass something to basic fe model without covariates
                                          "",
                                          "| ",
                                          # fixed effects
                                          "worker.ID ",
                                          "+ OG_BEID",
                                          ""
                                          )),
                                          vcov="iid",
                                          nthreads=7,
                                          data=tt_boot)
    
    ###### Retrieve fixed effects  ###### 
    cat(paste(Sys.time(), "retrieving fixed effects"), "\n")
    
    fes <- fixef(reg_fe, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000)
    
    workerfes <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID)
    companyfes <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OG_BEID = fes$OG_BEID, reference = FALSE)
    companyfes[OG_BEID==ref_firm, reference := TRUE]
    
    ####### Save fixed effects #####
    saveRDS(list(workerfes=workerfes, companyfes=companyfes), file = paste0(map_output_here, bootstrap, "_", model, ".rds"))
  }
  
}
k <- reg.table[, c(1, 1, uniqueN(worker.ID), uniqueN(OG_BEID), .N)]
metric.matrix.relative <- t(apply(metric.matrix, MARGIN=1, function(x) x/k))
rm(k)

timer <- Sys.time()-timer
timer # ca. 19 hours

saveRDS(list(metric.matrix=metric.matrix, metric.matrix.relative=metric.matrix.relative, timer=timer), file=paste0(map_output_here, "overview.rds"))
