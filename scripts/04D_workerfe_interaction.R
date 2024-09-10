##################################################################################
### Wage analysis with differential returns by worker fes: coefficients #####
# algorithm from De la Roca/Puga 17 Working in big cities --> add an interaction between (tenure and) experience and the worker fixed effects
# the idea is to loop through the estimations until convergence 
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")


if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")
if (!require("viridis")) install.packages("viridis"); library("viridis") # Color palette
if (packageVersion("viridis")!="0.6.2") warning("Analysis ran in viridis version 0.6.2 - consider up- or downgrading")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step4/"))) dir.create(paste0(map_data_analysis, "step4/")) # to store regression object for further analysis
if (!dir.exists(paste0(map_data_analysis, "step4/worker_fe/"))) dir.create(paste0(map_data_analysis, "step4/worker_fe/")) # to store regression object for further analysis

if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
if (!dir.exists(paste0(map_output, "analysis/wages/by_worker_fe/"))) dir.create(paste0(map_output, "analysis/wages/by_worker_fe/"))

map_output_here <- paste0(map_output, "analysis/wages/by_worker_fe/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) # general regression table
main_reg_model <- readRDS(paste0(map_data_analysis, "step4/main_model_wo_across.rds")) # worker fes from main regression (first run)

##################################################################################
##################################################################################
#### Prepare data ######
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

# save types for formula
types <- reg.table[, unique(company.type.1)]

##### Reorder data for fe estimation and adjust OG_BEID fixed effect by internationalisation status #####
# redorder data s.t. common ref firm is first
ref_firm <- reg.table[, .N, by=OG_BEID][, pick := N==max(N)][pick==TRUE, OG_BEID]
reg.table <- rbindlist(list(reg.table[OG_BEID==ref_firm, ], reg.table[OG_BEID != ref_firm, ]))

rm(ref_firm) 

# save order for below
reg.table[, order := 1:.N]

##### Add first set of worker fes to reg.table #####
reg.table <- merge(reg.table, main_reg_model$worker.fes.table[, c("worker.ID", "fe_workerID")], by="worker.ID")
rm(main_reg_model)
setorderv(reg.table, cols="order")


#### clean up to save memory ##### 
# only keep necesary vars in reg.table
reg.table <- reg.table[, c("order", "worker.ID", "OG_BEID", "Nemployer.year", "year", "company.group.size", 
              "company.type.1", "lhwage_detrended", "job_no_trunc",
              "exp_group_MNE_1", "exp_group_INT_COMP_1", "exp_group_DOMESTIC_1", "tenure_group_OGBEID_spell", "fe_workerID")]
##################################################################################
#### Analysis settings ####
##### Parameters of analysis ###### 
gc() # collect garbage

sample.use <- FALSE
sample.size <- 0.01*10^6
across.firm.interactions <- FALSE # note that plotting functions are not adjusted for this. 


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
#### Algorithm: worker fixed effect interaction #######
maxiter <- 30 # maximum iterations
coef_dist <- 1e-4 # early exit: max. euclidean distance between coef vecs in two successive iters (convergence #1)
worekrFE_dist <- 1e-3 # early exit: + max. MSE of worker fe vecs in two successive iters (convervenge #2)


# create an output matrix for MSEs and euclidean distances 
metrics <- matrix(NA, nrow=maxiter, ncol=3)
colnames(metrics) <- c("R2", "coef_dist", "MSE_workerfe")

# let's also store the coefficients in a list
coefs <- list()

timer <- Sys.time()
timer 

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
                          data=reg.table[subsample==TRUE,])
  
  
  # store R^2
  metrics[i, 1] <-  r2(reg_new, type="r2")
  # store coefficients 
  coefs[[i]] <- reg_new$coefficients
  
  # start updating working fixed effects
  cat(paste(Sys.time(), ":", "retieving fes", i), "\n")  
  
  # first delete old worker FE estimates, but keep them somewhere for an easy MSE comparison
  reg.table[, fe_workerID_before := fe_workerID] 
  reg.table[, fe_workerID := NULL]
  # and retrieve new set of worker fes, add to reg.table
  fes <- fixef(reg_new, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
  reg.table <- merge(reg.table, data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID), by="worker.ID")
  # reorder reg.table (keep reference firm for fe estimation the same)
  setorderv(reg.table, cols="order")

  # clean up
  rm(fes)
  gc()
  
  # early exit criterion: coefficient distance < coef_dist
  if (i > 1){
    # store euclidean distance in coefficients
    metrics[i, 2] <- dist(rbind(reg_before_coefs, reg_new$coefficients))
    cat(paste("Iteration", i, "with coef. distance of", metrics[i, 2], "to prev. interation"), "\n")
    
    # also store MSE of worker fes
    metrics[i, 3] <- reg.table[, sum((fe_workerID-fe_workerID_before)^2)]
    cat(paste("Iteration", i, "with MSE of worker fes of", metrics[i, 3], "to prev. interation"), "\n")
    
    if (i > 1 & metrics[i, 2] <= coef_dist & metrics[i, 3] <= worekrFE_dist) break
  }
  
  # exit loop if possible: you also move the entire check down here... in any case you want the worker fe's of the last iteration
}

timer <- Sys.time()-timer
timer 

#### Calculate final set of fixed effects ####

# retrieve fes
fes <- fixef(reg_new, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)

# company fes
firm.fes.table <- data.table(OG_BEID=as.integer(names(fes$OG_BEID)), fe_OG_BEID=fes$OG_BEID, reference=FALSE)
firm.fes.table[which(firm.fes.table$OG_BEID==names(fes$OG_BEID)[which(fes$OG_BEID==0)]), reference := TRUE]
firm.fes.table <- merge(firm.fes.table, 
                        reg.table[, .(company.type.1=unique(company.type.1), company.group.size=mean(company.group.size)), by=c("OG_BEID")], 
                        by=c("OG_BEID"))
firm.fes.table[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
# worker fes
worker.fes.table <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID)
worker.fes.panel <- merge(worker.fes.table, reg.table[, c("worker.ID", "year", "OG_BEID", "Nemployer.year", "company.type.1", "company.group.size")], by="worker.ID")
worker.fes.panel[, company.type.1.change := uniqueN(company.type.1)>1, by=OG_BEID]
worker.fes.panel[, reference := FALSE] # reference is in company fes
rm(fes)

#### Save model fit #####
reg_new$metrics <- metrics
reg_new$timer <- timer
reg_new$firm.fes.table <- firm.fes.table
reg_new$worker.fes.table <- worker.fes.table
reg_new$worker.fes.panel <- worker.fes.panel

saveRDS(reg_new, file=paste0(map_data_analysis, "step4/worker_fe/worker_FE_interaction_model.rds"), compress=TRUE)
saveRDS(coefs, file=paste0(map_data_analysis, "step4/worker_fe/workerFE_iter_coefs.rds"), compress=TRUE)

