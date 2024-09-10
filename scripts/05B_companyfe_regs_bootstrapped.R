##################################################################################
### This script runs bootstrapped regressions on the company fixed effects: Entry wage differences ####
##################################################################################
#### packages ######
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("haven")) install.packages("haven"); library("haven") # save .dta (STATA) file
if (packageVersion("haven")!="2.5.0") warning("Analysis ran in haven version 2.5.0 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

#### dirs ##### 
if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
if (!dir.exists(paste0(map_output, "analysis/wages/entry_wages/"))) dir.create(paste0(map_output, "analysis/wages/entry_wages/"))
map_output_here <- paste0(map_output, "analysis/wages/entry_wages/") # for final outputs

#### load data ####
# load non-bootstrapped data: This is already subsampled in an earlier script
companyfes <- as.data.table(read_dta(paste0(map_data_analysis, "step4/companyfes.dta")))
# main dataset
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 

##################################################################################
##################################################################################
#### Prepare data #####
# add mean employment to companyfes
companyfes <- merge(companyfes, reg.table[, .(employment = mean(company.group.size)), by=OG_BEID],
                    by = "OG_BEID")

##################################################################################
#### Run regressions with bootstrapped standard errors #####

##### split into separate data.tables per estimation model #####
basic <- companyfes[, .(MNE, OG_BEID, employment, fe_OG_BEID = basic)]
size <- companyfes[, .(MNE, OG_BEID, employment, fe_OG_BEID = basicsize)]
full <- companyfes[, .(MNE, OG_BEID, employment, fe_OG_BEID = main)]
interacted <- companyfes[, .(MNE, OG_BEID, employment, fe_OG_BEID = interacted)]
  
##### loop through models and run regression with bootstrapped standard errors #######
## list of output models and bootstrapped covariance matrix
companyfe_regs <- list()
bvcov <- list()

timer <- Sys.time()
for (model in c("basic", "size", "full", "interacted")){ 
  # message
  cat(paste(Sys.time(), ": starting", model), "\n")
  
  # Run main regression to get coefficients
  companyfe_regs[[model]] <- feols(fe_OG_BEID ~ MNE, 
                                   data=get(model),
                                   weights = get(model)[, employment])
  
  # bootstrapped standard errors 
  ## matrix to store bootstrapped coefficient estimates
  bcoefs <- matrix(NA, nrow = 100, ncol = length(companyfe_regs[[model]]$coefficients))
  # add progress bar
  pb <- txtProgressBar(min = 1, max = 100, initial = 1)
  for (boot in 1:100){
    # update progress bar
    setTxtProgressBar(pb, boot)
   
    # load data set
    if (model == "interacted"){
      current <- readRDS(paste0(map_data_paper2, "step5/bootstrap_workerfe/", "fes_", boot, ".rds"))
      
    }else{
      current <- readRDS(paste0(map_data_paper2, "step5/bootstrap_wo_workerfe/", boot, "_", model, ".rds"))
    }
    # subset to company fes
    current <- current$companyfes 
    
    # add employment weights and MNE identifier
    current <- merge(current, get(model)[, !c("fe_OG_BEID")],
                     by = "OG_BEID")
    
    # run regression and store coefficients
    reg <- feols(fe_OG_BEID  ~ MNE, 
                 data = current,
                 weights = current[, employment])
    
    # extract coefficient estimates
    bcoefs[boot,] <- reg$coefficients
  }
  # close progress bar
  close(pb)
  
  # add bootstrapped vcov matrix to output list
  bvcov[[model]] <- cov(bcoefs)
}
timer <- Sys.time()-timer
timer # 40 secs

##### create output table #####
etable(companyfe_regs, 
       vcov = bvcov,
       title="Company fixed effects (MNE vs Domestic)", 
       file=paste0(map_output_here, "reg_companyfe.tex"),
       label="tab:reg_companyfe", 
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

