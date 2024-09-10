##################################################################################
### Translate RINPs ####
# This script assigns an integer for each RINP in the worker panel 
# That saves a lot of memory (ca 5 gb) when loading the full panel
##################################################################################

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))

##################################################################################
##################################################################################
####### Main part #######
# load RINPs from all years in one data.table
RINPs <- list()
k <- 0
for (current_year in 2006:2021){ 
  cat(paste(Sys.time(), ":", current_year), "\n")
  k <- k+1
  RINPs[[k]] <- as.data.table(readRDS(paste0(map_data_analysis, "step1/SPOLIS_yearly/RINPs_", current_year, ".rds"))) 
}
RINPs <- rbindlist(RINPs)

# only keep unique RINPs
RINPs <- RINPs[!duplicated(RINPs),]
RINPs[, .N] # number of unique workers
setnames(RINPs, 1, "RINP")

# translate string to integer
RINPs[, worker.ID := 1:.N]

# save results
saveRDS(RINPs, file=paste0(map_data_analysis, "step1/RINP_workerID_translation.rds"), compress = TRUE)
