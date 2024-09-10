##################################################################################
### This scripts combines the bootstrapped company/worker fe estimates in one table per model for Stata ##### 
# goal: table with MNE (ID), fe_OG_BEID (for method coefficients), fe_OG_BEID1 ... fe_OG_BEID100 (bootstrapped distributions) 
# --> You need this setup to use functionality of estquant in Stata (produce bootstrapped standard errors)
##################################################################################
#### packages ######
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("haven")) install.packages("haven"); library("haven") # save .dta (STATA) file
if (packageVersion("haven")!="2.5.0") warning("Analysis ran in haven version 2.5.0 - consider up- or downgrading")

#### dirs ##### 
if (!dir.exists(paste0(map_data_analysis, "step5/"))) dir.create(paste0(map_data_analysis, "step5/")) # to store regression object for further analysis
if (!dir.exists(paste0(map_data_analysis, "step5/Stata_files/"))) dir.create(paste0(map_data_analysis, "step5/Stata_files/")) # to store regression object for further analysis
map_output_here <- paste0(map_data_analysis, "step5/Stata_files/") # for bootstrap outputs

#### load data ####
# load non-bootstrapped data: This is already subsampled in an earlier script
companyfes <- as.data.table(read_dta(paste0(map_data_analysis, "step4/companyfes.dta")))
workerfes <- as.data.table(read_dta(paste0(map_data_analysis, "step4/workerfes.dta")))

##################################################################################
##################################################################################
#### Create .dta files for Combes et al method in Stata #####

# Note that selection of workers/firms for distribution is given by the step4/ ... .dta files!

# General setup: for company/worker fes go through different models,
# loop through bootstraps, read data, add bootstrap columns, save in one .dta for Stata

# loop through fixed effect types: note to enhance performance by move this to the lowest loop level
for (fetype in c("company", "worker")){
  # message
  cat(paste(Sys.time(), ": starting", fetype), "\n")
  
  # need ID names
  fetype.2 <- fifelse(fetype == "company", "OG_BEID", "workerID")
  
  # split into separate data.tables per estimation model
  basic <- get(paste0(fetype,"fes"))[, .(MNE, ID = get(fetype.2), fe = basic)]
  size <- get(paste0(fetype,"fes"))[, .(MNE, ID = get(fetype.2), fe = basicsize)]
  full <- get(paste0(fetype,"fes"))[, .(MNE, ID = get(fetype.2), fe = main)]
  interacted <- get(paste0(fetype,"fes"))[, .(MNE, ID = get(fetype.2), fe = interacted)]
  fullwosize <- get(paste0(fetype,"fes"))[, .(MNE, ID = get(fetype.2), fe = fullwosize)]
  
  # for worker fes also create table for starters
  if (fetype == "worker"){
    starter <-  get(paste0(fetype,"fes"))[, .(MNE, ID = get(fetype.2), fe = starter)]
    # only keep relevant rows
    starter <- starter[!is.na(fe),]
    # set models 
    models <- c("starter", "full", "fullwosize", "basic", "size", "interacted")
  } else models <- c("full", "fullwosize", "basic", "size", "interacted")
  
  # loop through desired data sets
  for (model in models){ 
    # message
    cat(paste(Sys.time(), ": starting", model), "\n")
    
    # extract relevant base table
    output <- get(model)
    
    # add progress bar
    pb <- txtProgressBar(min = 1, max = 100, initial = 1)
    
    # loop through bootstrapped data and add columns
    for (boot in 1:100){ 
      # update progress bar
      setTxtProgressBar(pb, boot)
      
      # load data set
      if (model == "interacted" | model ==  "starter"){
        current <- readRDS(paste0(map_data_analysis, "step5/bootstrap_workerfe/", "fes_", boot, ".rds"))
        
      }else{
        current <- readRDS(paste0(map_data_analysis, "step5/bootstrap_wo_workerfe/", boot, "_", model, ".rds"))
      }
      # subset to firm or company fes
      if (fetype == "company") current <- current$companyfes else current <- current$workerfes
      
      # setnames and clear
      if (fetype == "company") setnames(current, "fe_OG_BEID", paste0("fe", boot)) else setnames(current, c("fe_workerID", "worker.ID"), c(paste0("fe", boot), "workerID"))
      setnames(current, fetype.2, "ID")
      if (!is.null(current$reference)) current[, c("reference") := NULL]
      # append to model
      output <- merge(output, current, by="ID", all.x=TRUE)
    }
    rm(boot, current)
    # close progress bar
    close(pb)
    
    # remove ID to anonymize
    output[, ID := NULL]
    
    # save final output as .dta file
    write_dta(data = output, path = paste0(map_output_here, model, "_", fetype, ".dta"))
  }
}

