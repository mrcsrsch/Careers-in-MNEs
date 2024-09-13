##################################################################################
### Robustness check: Split by Foreign and Domestic MNEs #####

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
if (!dir.exists(paste0(map_output, "robustness/foreign_domestic_MNE/"))) dir.create(paste0(map_output, "robustness/foreign_domestic_MNE/"))

map_output_here <- paste0(map_output, "robustness/foreign_domestic_MNE/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
# load raw MNE data
MNEs <- readRDS(paste0(map_data_analysis, "step1/company/MNEs.rds"))
OG_SBEIDs <- readRDS(paste0(map_data_analysis, "step2/OGBEID_SBEID_translation.rds"))


##################################################################################
##################################################################################
#### Identify Foreign and Domestic MNEs ####
MNEs <- merge(MNEs, OG_SBEIDs[, c("SBEID", "year", "OG_BEID")], by=c("SBEID", "year"))
rm(OG_SBEIDs)

# aggregate to OG_BEID level
MNEs <- MNEs[, .(BUI_MUL=BUI_MUL[1], NED_MUL=NED_MUL[1]), by=c("OG_BEID", "year")]

# extract firm level panel from reg.table
firm_panel <- reg.table[, .(MNE = company.type.1[1]=="MNE"),by=c("OG_BEID", "year")]
# add foreign/domestic info
firm_panel <- merge(firm_panel, MNEs, by=c("OG_BEID", "year"), all.x=T)
firm_panel[is.na(BUI_MUL), BUI_MUL := F]
firm_panel[is.na(NED_MUL), NED_MUL := F]

# assign foreign or domestic based on majority vote
firm_panel[MNE==T, MNE_type := fifelse(sum(BUI_MUL)>=sum(NED_MUL), "Foreign_MNE", "Domestic_MNE"), by="OG_BEID"]

# add back to reg.table
firm_panel <- firm_panel[MNE==T, .(MNE_type=MNE_type[1]), by="OG_BEID"]

reg.table <- merge(reg.table, firm_panel, by="OG_BEID", all.x=T)

#### Create new experience profiles ####
reg.table[, company.type.2 := company.type.1]
reg.table[company.type.2=="MNE", company.type.2 := MNE_type]
reg.table[, MNE_type := NULL]

setorderv(reg.table, cols=c("worker.ID", "year", "Nemployer.year"))

types <-  reg.table[, unique(company.type.2)]
## loop through them
for (type in types){
  # message
  cat(paste0(Sys.time(), " running ", which(types==type), "/", length(types)), "\n")
  # calculate cumulative sum of years in company.type.2 and lag by one period
  reg.table[, paste0("exp_years_", type, "_", "2") := c(0, c(cumsum(fifelse(company.type.2==type, tenure_days_OGBEID_year,0))/365)[-.N]), by=worker.ID] 
  # per OG_BEID spell add first obs as experience measure (we look at across firm returns)
  reg.table[, paste0("exp_years_", type, "_", "2") := eval(as.symbol(paste0("exp_years_", type, "_", "2")))[1], by=c("worker.ID", "OG_BEID", "spell.id")]
  
  # cut tenure profile into splines
  reg.table[, paste0("exp_group_", type, "_", "2") := cut(eval(as.symbol(paste0("exp_years_", type, "_", "2"))), breaks=c(-Inf, seq(0,10,1), Inf), include.lowest=TRUE)]
}

##################################################################################
#### Prepare data for estimation ######
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
types <- reg.table[, unique(company.type.2)]
## empirical model formula parts

# within firm tenure (growth)
within_firm <- paste0(" + i(tenure_group_OGBEID_spell, ref=\"[-Inf,1]\")")
within_firm_interact <- paste0(" + i(company.type.2, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\")")

# within firm tenure by job number (general effect)
within_firm_job <- paste0(" + i(tenure_group_OGBEID_spell, i.job_no_trunc, ref=\"[-Inf,1]\", ref2=\"1\")")

# across firm experience returns
across_firm <- paste0(" + i(exp_group_", types, "_", "2", ", ref = \"[-Inf,0]\")", collapse = "")
if (across.firm.interactions){
  across_firm_interact <- paste0(" + i(company.type.2, exp_group_", types, "_", "2",  ", ref=\"DOMESTIC\"", ", ref2 = \"[-Inf,0]\")", collapse = "")
} else {
  across_firm_interact <- ""
}

# controls
controls <- "+ log(company.group.size) + i(job_no_trunc, ref=\"1\")" 
if (across.firm.interactions) {
  controls <- paste(controls, "+ i(company.type.2, i.job_no_trunc , ref=\"DOMESTIC\", ref2=\"1\")") ##"+ log(company.group.size)"
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
  pdf(file=paste0(map_output_here, "coefplot_across_firm_wo_across_foreign_domestic.pdf"), width=16*0.7, height=9*0.7)
  estimates <- total.general.prem(reg_main, exp.types = c("Foreign_MNE", "Domestic_MNE", "DOMESTIC"), k=2)
  company.type.plot(estimates, type="Total", legend.add = TRUE, legend.names = c("Foreign MNE", "Domestic MNE"), only.legend = FALSE, title = "", graycolors = TRUE)
  dev.off()
  
  # Also output estimates to a .txt file
  sink(paste0(map_output_here, "coefs_across_firm_wo_across.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()

rm(estimates)

###### B) Stayer wage growth ###### 

  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_wo_across_foreign_domestic.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("Foreign_MNE", "Domestic_MNE", "DOMESTIC"),  k=2)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=FALSE, title="", legend.names= c("Foreign MNE", "Domestic MNE"), graycolors=TRUE)
  dev.off()
  
  # Also output estimates to a .txt file
  sink(paste0(map_output_here, "coefs_stayer_wages_wo_across.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()

rm(estimates)