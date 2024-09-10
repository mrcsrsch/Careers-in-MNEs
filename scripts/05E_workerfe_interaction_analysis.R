##################################################################################
### Analysis of workerfe interaction model with bootstrapped standard errors ####

##################################################################################

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
if (!dir.exists(paste0(map_output, "analysis/wages/by_worker_fe/"))) dir.create(paste0(map_output, "analysis/wages/by_worker_fe/"))
map_output_here <- paste0(map_output, "analysis/wages/by_worker_fe/") # for final outputs

#### load data  ####
ability_model <- readRDS(paste0(map_data_analysis, "step4/worker_fe/worker_FE_interaction_model.rds")) # coefficients
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) # main dataset
##################################################################################
##################################################################################
#### prepare data ######
##### Calculate fixed effect groups for analysis #######
# retrieve fes
fes <- fixef(ability_model, sorted=TRUE, nthreads=4, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
# extract worker fixed effects
worker.fes.table <- data.table(worker.ID=as.integer(names(fes$worker.ID)), fe_workerID = fes$worker.ID)
rm(fes)

##### Add bootstrapped coviarance matrix #####

# number of bootstraps
bootstraps <- 100

# create matrix with one row per bootstrap iteration
bcoefs <- matrix(NA, nrow=bootstraps, ncol=length(ability_model$coefficients))
colnames(bcoefs) <- names(ability_model$coefficients)

for (bootstrap in 1:bootstraps){
  current <- readRDS(paste0(map_data_paper2, "step5/bootstrap_workerfe/coefs_", bootstrap, ".rds"))
  bcoefs[bootstrap, ] <- as.vector(current[[1]])
}

# calculate bootstrapped variance-covariance matrix
bvcov <- cov(bcoefs)

##################################################################################
#### Create output table #####

etable(ability_model,
       vcov = bvcov,
       file=paste0(map_output_here, "reg_wages_worker_fe.tex"),  
       title="Wage profiles",
       label="tab:worker_fe_interaction", 
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
                `log(company.group.size)` = "log(company size)", job_no_trunc = "Employer number", fe_workerID = "worker fe", worker.ID = "worker", OG_BEID = "company"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       order = c("years in company", "Domestic Company experience", "International Company experience", "MNE experience"),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Bootstrapped standard errors clustered at worker level in parantheses."),
       replace = TRUE)

##################################################################################
#### Graphical analysis #####

###### identify worker fe groups #####
worker.fes.table[, fe_workerID_percentile := cut(fe_workerID, breaks=c(-Inf,quantile(fe_workerID, probs=c(1/4, 3/4)), Inf), labels=c("Low-ability", "Mid-ability", "High-ability"), include.lowest=TRUE)]
percentile_means <- worker.fes.table[, .(fe_workerID = mean(fe_workerID)), by=fe_workerID_percentile][order(fe_workerID_percentile)]
percentile_means[, cutoffs := c("<0.25", "0.25-0.75", ">0.75")]

###### Across firm ####
# get estimates
estimates <-  workerfe.estimates(ability_model, across=TRUE, 
                                 exp.types=reg.table[, unique(company.type.1)], 
                                 percentile_means, k=1,
                                 bvcov = bvcov)
# create plots
for (type in c("MNE", "INT_COMP")){
  pdf(file=paste0(map_output_here, "coefplot_across_workerfe_", type, ".pdf"), width=16*0.7, height=9*0.7)
  workerfe.plot(estimates, type=type, legend.add = TRUE, title = "", across=TRUE, graycolors=TRUE)
  dev.off()
}

# write estimates to file
sink(paste0(map_output_here, "coefs_across_workerfe.txt"))
estimates
sink()

###### Within firm ####
estimates <-  workerfe.estimates(ability_model, across=FALSE, 
                                 exp.types=reg.table[, unique(company.type.1)], 
                                 percentile_means, k=1,
                                 bvcov = bvcov)
# create plots
for (type in c("MNE", "INT_COMP")){
  pdf(file=paste0(map_output_here, "coefplot_within_workerfe_", type, ".pdf"), width=16*0.7, height=9*0.7)
  workerfe.plot(estimates, type=type, legend.add = TRUE, title = "", across=FALSE, graycolors=TRUE)
  dev.off()
}


