##################################################################################
##### Careers in Multinational Enterprises, master script ########################
# This script loads global settings and
# sources all other scripts in the folder to 
# i) build the analysis dataset
# ii) run the analysis and robustness checks
# Setting "run.scripts" to TRUE will execute the full project.
##################################################################################
##################################################################################

# clean start 
rm(list=ls(all.names = TRUE))
gc() # return memory to system

# track memory usage
# gcinfo(FALSE)
# increase the memory limit to server maximum
memory.limit(size=55500)

# global packages
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")

# create folder structure and set paths
## SET YOUR MAIN FOLDER AND DATA SOURCE FOLDER HERE
main_path <- NA 
if (is.na(main_path)) warning("You need to set the main folder path.")

## set paths
map_data_source <- paste0(main_path, "source_data/")
map_scripts <- paste0(main_path, "scripts/")
map_data_analysis <- paste0(main_path, "data/")
map_output <-  paste0(main_path, "outputs/")

## create data and output paths if necessary
if (!dir.exists(map_data_analysis)) dir.create(map_data_analysis)
if (!dir.exists(map_output)) dir.create(map_output)


##################################################################################
### Call scripts to run analysis ####

run.scripts <- FALSE # set to TRUE to execute full project.
countert <- 0
ttimers <- list()

# small functions to wrap around script calls
start.part <- function(){
  # save and show timer
  countert <<- countert + 1 # update in global environment
  ttimers[[countert]] <<- Sys.time()
  cat(paste(ttimers[[countert]]), "\n")
}


end.part <- function(){
  # save and show timer
  ttimers[[countert]] <<- Sys.time()-ttimers[[countert]]
  print(ttimers[[countert]])
  # store list of objects to remove
  objs <<- ls(all.names = TRUE)[-which(ls(all.names = TRUE) %in% c("map_data_analysis", "map_data_source", "map_scripts", "map_output", "countert", "ttimers", "end.part", "start.part", "run.scripts"))]
}

if (run.scripts){
  ###########################################################################################################

  
  ### 01 Load and clean data ####
  
  #### Firm level data ####
  # retrieve firm level data from SQL server (Note: you may need to adjust this depending on your source for firm level data within CBS)
  # clean firm level data
  start.part()
  source(paste0(map_scripts, "01A_companydata.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Find largest connected set and identify experience measures ####
  start.part()
  source(paste0(map_scripts, "01B_cpi.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
    
  
  #### GBA (worker demographics) ####
  start.part()
  source(paste0(map_scripts, "01C_GBA.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### POLIS (matched employer-employee data) ####
  # load and clean yearly employer employee data (firm level)
  start.part()
  source(paste0(map_scripts, "01D_readPOLIS.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  # create numeric worker IDs to save memory
  start.part()
  source(paste0(map_scripts, "01E_translationRINPs.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  # append and clean yearly employer employee data (company group level)
  start.part()
  source(paste0(map_scripts, "02A_cleanPOLISappend.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Graduates ####
  # read and clean data on graduates
  start.part()
  source(paste0(map_scripts, "02B_graduates.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Identify company types for company groups in matched employer employee data ####
  start.part()
  source(paste0(map_scripts, "02C_identify_company_types.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  ### 02 Build analysis dataset ####

  #### Build matched employer-employee data of graduates ####
  start.part()
  source(paste0(map_scripts, "03A_POLIS_graduates.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Find largest connected set and identify experience measures ####
  start.part()
  source(paste0(map_scripts, "03B_experience_measures.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  ### 03 Run analyses ####
  
  #### Summary stats ####
  start.part()
  source(paste0(map_scripts, "04A_summarystats.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Main wage analysis ####
  start.part()
  source(paste0(map_scripts, "04B_main_wage_analysis.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Wage path plots ####
  start.part()
  source(paste0(map_scripts, "04C_main_wage_paths.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Interactions with worker fixed effects (main estimates) ####
  start.part()
  source(paste0(map_scripts, "04D_workerfe_interaction.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Distributions of worker and firm fixed effects for different specifications ####
  start.part()
  source(paste0(map_scripts, "04E_worker_company_fe_distributions.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Bootstrap around the main model (for company and worker fe distribution analysis and entry wage regressions) ####
  start.part()
  source(paste0(map_scripts, "05A_main_basic_models_bootstrap.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Bootstrapped linear regression on entry wages ####
  start.part()
  source(paste0(map_scripts, "05B_companyfe_regs_bootstrapped.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Bootstrapped around the model with worker fe interactions (for company and worker fe distribution analysis and entry wage regressions) ####
  start.part()
  source(paste0(map_scripts, "05C_workerfe_interaction_bootstrap.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Create Stata output file to run Combes method (for company and worker fe distribution analysis in Appendix B)  ####
  start.part()
  source(paste0(map_scripts, "05D_workerfe_sorting_prepare_Combes.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Output tables of Appendix B (need to run after 05D_workerfe_sorting_prepare_Combes.R is run) #### 
  # These run in Stata. See the scripts 99_Stata_run_Combes_company and 99_Stata_run_Combes_worker
  
  #### Run regressions and create graphs for worker fe interaction analysis with bootstrapped standard errors ####
  start.part()
  source(paste0(map_scripts, "05E_workerfe_interaction_analysis.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Mass layoff analysis with bootstrapped standard errors ####
  start.part()
  source(paste0(map_scripts, "05F_mass_layoffs.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Employment probability analysis ####
  start.part()
  source(paste0(map_scripts, "06A_empl_probability.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Employment probability analysis: worker fixed effect interactions ####
  start.part()
  source(paste0(map_scripts, "06B_empl_probability_workerfe_bootstrap.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Analysis of entrants to MNEs ####
  start.part()
  source(paste0(map_scripts, "06C_MNE_entries.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  ###########################################################################################################
  ###########################################################################################################

  ### 04 Robustness checks ####

  #### Base salary ####
  start.part()
  source(paste0(map_scripts, "07_rob_base_salary.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Excluding acquisitions ####
  start.part()
  source(paste0(map_scripts, "07_rob_excluding acquisitions.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Large firms ####
  start.part()
  source(paste0(map_scripts, "07_rob_large_firms.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Discounting experience ####
  start.part()
  source(paste0(map_scripts, "07_rob_discounting_experience.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### MNE vs industry experience ####
  start.part()
  source(paste0(map_scripts, "07_rob_industry_experience.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### MNE vs location-specific experience ####
  start.part()
  source(paste0(map_scripts, "07_rob_location_experience.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  #### Bargaining ####
  start.part()
  source(paste0(map_scripts, "07_rob_bargaining.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Firm-year fixed effects + Spell fixed effects ####
  start.part()
  source(paste0(map_scripts, "07_rob_firmyear_spell.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Foreign vs. domestic MNEs ####
  start.part()
  source(paste0(map_scripts, "07_rob_FMNE_DMNE.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Across firm returns by firm type ####
  start.part()
  source(paste0(map_scripts, "07_rob_across_by_firm_type.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Limited mobility bias ####
  start.part()
  source(paste0(map_scripts, "07_rob_limited_mobility.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  # save timers
  saveRDS(ttimers, file=paste0(map_data_analysis, "project_timers.rds"))
}
