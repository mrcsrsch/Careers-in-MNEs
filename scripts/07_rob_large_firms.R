##################################################################################
### Robustness check: Minimum firm size > x #####
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")


#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_output, "robustness/"))) dir.create(paste0(map_output, "robustness/"))
if (!dir.exists(paste0(map_output, "robustness/wage_min_company_size/"))) dir.create(paste0(map_output, "robustness/wage_min_company_size/"))

map_output_here <- paste0(map_output, "robustness/wage_min_company_size/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
##################################################################################
##################################################################################
#### Prepare data ######
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

##################################################################################
#### Main wage analysis: settings ####
##### Parameters of analysis ###### 
gc() # collect garbage

sample.use <- FALSE
sample.size <- 0.001*10^6
across.firm.interactions <- FALSE
back.of.envelope <- TRUE # adds back of envelope calculations --> What is the "excess" contribution of MNEs to wages in NL? 
# back.of.envelope only implement for non-interacqion cae
if (across.firm.interactions) back.of.envelope <- FALSE

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
controls <- "+ log(company.group.size) + i(job_no_trunc, ref=\"1\")" 
if (across.firm.interactions) {
  controls <- paste(controls, "+ i(company.type.1, i.job_no_trunc , ref=\"DOMESTIC\", ref2=\"1\")") ##"+ log(company.group.size)"
}  


for (min_company_size in c(20, 50, 250)){
  # message
  cat(paste0(Sys.time(), " size class ", min_company_size))
  
  
  ##################################################################################
  #### Main regression analysis: Non-linear returns #######
  
  reg_main <- feols(fml=as.formula(paste0("lhwage_basic_detrended ~ ",
                                          within_firm,
                                          within_firm_interact,
                                          within_firm_job,
                                          across_firm, 
                                          across_firm_interact,
                                          controls,
                                          "",
                                          "| ",
                                          # fixed effects
                                          "worker.ID ",
                                          "+ OG_BEID",
                                          ""
  )),
  cluster="worker.ID",
  ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
  nthreads=7,
  data=reg.table[subsample==TRUE & company.group.size>= min_company_size])
  
  ##### Print table ####  
  etable(reg_main, #lapply(paste0("regs[[",length(regs):1, "]]"), function(x) eval(parse(text=x))), 
         file=paste0(map_output_here, "reg_wages_non_linear_across_min_company_size_",min_company_size, ".tex"),  
         title="Wage profiles",
         label="tab:main_wage", 
         depvar = TRUE,
         digits = "r4",
         fitstat = c("n", "r2"),
         digits.stats = "r4",
         fixef_sizes = TRUE,
         fixef_sizes.simplify = TRUE,
         powerBelow = -6,
         float = TRUE,
         dict = c(company.type.1 = "", MNE = "MNE", INT_COMP = "International firm", tenure_group_OGBEID_spell = "Within company tenure",
                  exp_group_MNE_1 = "MNE experience", exp_group_INT_COMP_1 = "International firm experience", exp_group_DOMESTIC_1 = "Domestic company experience",
                  `log(company.group.size.2)` = "log(company size)", job_no_trunc = "Employer number"),
         signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
         notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                   "Clustered standard errors (worker level) in parantheses."),
         replace = TRUE)
  
  ##### Run graphical analysis ####

  pdf(file=paste0(map_output_here, "coefplot_across_min_company_size_", min_company_size, ".pdf"), width=16*0.7, height=9*0.7)
  estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
  company.type.plot(estimates, type="Total", legend.add = TRUE, legend.names = c("MNE", "Int. firm"), only.legend = FALSE, title = "", graycolors = TRUE)
  dev.off()

  rm(estimates)
  
  ###### B) Stayer wage growth ###### 
  pdf(file=paste0(map_output_here, "coefplot_stayer_min_company_size_", min_company_size, ".pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", legend.names= c("MNE", "Int. firm"), graycolors=TRUE)
  dev.off()
    
  rm(estimates)
}