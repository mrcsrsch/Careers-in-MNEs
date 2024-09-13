##################################################################################
### Robustness check: Does the value of experience depend on the type of the current employer? #####
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
if (!dir.exists(paste0(map_output, "robustness/across_by_type/"))) dir.create(paste0(map_output, "robustness/across_by_type/"))

map_output_here <- paste0(map_output, "robustness/across_by_type/") # for final outputs

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
across.firm.interactions <- TRUE
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
  controls <- paste(controls, "+ i(company.type.1, factor(job_no_trunc), ref=\"DOMESTIC\", ref2=\"1\")") 
}  


##################################################################################
#### Main regression analysis: Non-linear returns #######

reg_main <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
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
data=reg.table[subsample==TRUE])

##### Print table ####
etable(reg_main, #lapply(paste0("regs[[",length(regs):1, "]]"), function(x) eval(parse(text=x))), 
       file=paste0(map_output_here, "reg_wages_non_linear_", ifelse(across.firm.interactions, "w_", "wo_"), "across", ".tex"),  
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
###### A) Mover wage growth #####

# Retrieve estimates at different types of outside firms
# at MNE 
estimates_MNE <- total.company.type(reg_main, company.type="MNE", exp.types=types, k=1)
# at Int Comp
estimates_Int <- total.company.type(reg_main, company.type="INT_COMP", exp.types=types, k=1)
# at domestic firm
estimates_DOM <- total.general.prem(reg_main, exp.types = types, k=1)
## put MNE estimates in one matrix for estimates and se
estimates <- list()

estimates$coefs <- matrix(NA, nrow=nrow(estimates_DOM$coefs), ncol=3)
rownames(estimates$coefs) <- rownames(estimates_DOM$coefs)
colnames(estimates$coefs) <- c("MNE", "Int. firm", "Domestic")
estimates$coefs[,1] <- estimates_MNE$coefs[,which(colnames(estimates_MNE$coefs)=="MNE")]
estimates$coefs[,2] <- estimates_Int$coefs[,which(colnames(estimates_Int$coefs)=="MNE")]
estimates$coefs[,3] <- estimates_DOM$coefs[,which(colnames(estimates_DOM$coefs)=="MNE")] 

estimates$sds <- matrix(NA, nrow=nrow(estimates_DOM$sds), ncol=3)
rownames(estimates$sds) <- rownames(estimates_DOM$sds)
colnames(estimates$sds) <- c("MNE", "Int. firm", "Domestic")
estimates$sds[,1] <- estimates_MNE$sds[,which(colnames(estimates_MNE$sds)=="MNE")]
estimates$sds[,2] <- estimates_Int$sds[,which(colnames(estimates_Int$sds)=="MNE")]
estimates$sds[,3] <- estimates_DOM$sds[,which(colnames(estimates_DOM$sds)=="MNE")] 

# black and white (paper)
pdf(file=paste0(map_output_here, "coefplot_across_firm_w_across_bw.pdf"), width=16*0.7, height=9*0.7)
company.type.plot(estimates, type="Total", legend.add = TRUE, 
                  legend.names = c("at MNE", "at Int. firm", "at domestic firm"), 
                  only.legend = FALSE, title = "", graycolors = TRUE, highlight.MNE = FALSE)
dev.off()
# in color (presentation)
pdf(file=paste0(map_output_here, "coefplot_across_firm_w_across_col.pdf"), width=16*0.7, height=9*0.7)
company.type.plot(estimates, type="Total", legend.add = TRUE, 
                  legend.names = c("at MNE", "at Int. firm", "at domestic firm"), 
                  only.legend = FALSE, title = "", graycolors = FALSE, highlight.MNE = FALSE)
dev.off()

# Also output estimates to a .txt file
sink(paste0(map_output_here, "coefs_across_firm_w_across.txt"))
estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
sink()

rm(estimates)

###### B) Stayer wage growth ###### 

  # black and white (paper)
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_w_across_bw.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                    legend.names= c("MNE", "Int. firm"), graycolors=TRUE)
  dev.off()
  
  # color (presentation)
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_w_across_col.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                    legend.names= c("MNE", "Int. firm"), graycolors=FALSE)
  dev.off()
  
  # Also output estimates to a .txt file
  sink(paste0(map_output_here, "coefs_stayer_wages_w_across.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
rm(estimates)