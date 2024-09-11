##################################################################################
##### Employment probability analysis w/o worker fe interactions ##### 
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")
require("car")
if (packageVersion("car")!="3.0.12") warning("Analysis ran in car version 3.0.12 - consider up- or downgrading")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/cohort/"))) dir.create(paste0(map_output, "analysis/cohort/"))
map_output_here <- paste0(map_output, "analysis/cohort/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) # main wage panel
##################################################################################
##################################################################################
#### clean data #####
# truncate job.number
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

# identify labor market entry cohort for each worker
reg.table[, entry_cohort := min(year), by=worker.ID]

###### Get years since labor market entry ##### 
reg.table[, labor_exp_years := year-entry_cohort]
reg.table[, labor_exp_group_2 := cut(labor_exp_years, breaks=c(-Inf, seq(0,10,1), Inf), include.lowest=TRUE)]


###### identify entrants/exiters. #####
# first order again to be sure
setorderv(reg.table, cols=c("worker.ID", "year", "Nemployer.year"))

# then lead relevant vars
reg.table[, c("OG_BEID.lead", "OG_BEID.lag") := shift(.(OG_BEID),
                                                      n=c(1,-1), type="lead"), by=worker.ID]


# Identify first and last obs of worker to correct entries and exits
reg.table[, n := 1:.N, by=worker.ID]
reg.table[, entry.obs := n==1]
reg.table[, exit.obs := n==max(n), by=worker.ID]
reg.table[, n := NULL]

####### Exiters #### 
# identify last year in an OG_BEID
reg.table[, exit := OG_BEID!=OG_BEID.lead]
# correct exits before end of panel 
reg.table[exit.obs==TRUE & year < 2021, exit := TRUE]
# correct for OG_BEID closures
reg.table[, OG_close := year==max(year), by=OG_BEID]
#reg.table[year==2021, OG_close := FALSE]
#reg.table[OG_close==TRUE, exit := FALSE]

####### Entrants #### 
reg.table[, entry := OG_BEID!=OG_BEID.lag]
reg.table[entry.obs==TRUE, entry := TRUE]

##### clean up ###### 
reg.table[, c("OG_BEID.lead", "OG_BEID.lag", "OG_close") := NULL]
gc()

##################################################################################
#### Data overview #####

# calculate share of workers in MNEs at labor market entry
## entries into MNEs 
reg.table[labor_exp_group_2=="[-Inf,0]" & entry.obs==TRUE, mean(company.type.1=="MNE")] # 0.31184
## entries into MNEs, adjusted for cohort sizes
reg.table[labor_exp_group_2=="[-Inf,0]" & entry.obs==TRUE, mean(company.type.1=="MNE"), by=entry_cohort][, mean(V1)] # 0.3151257
## exits out of MNEs
reg.table[labor_exp_group_2=="[-Inf,0]" & entry.obs==TRUE & exit.obs==FALSE, mean(company.type.1=="MNE" & exit)] # 0.04263

##################################################################################
#### Analysis settings #####
sample.use <- FALSE
sample.size <- 0.3*10^6

##### use full sample or subsample with size sample.use? ####
if (sample.use){
  # use a  random sample of workers for quicker calculations
  set.seed(7854)
  reg.table[, subsample := (worker.ID %in% sample(worker.ID, sample.size))]
} else{
  # full sample
  reg.table[, subsample := TRUE]
}


##### set formula parts #### 
frm <-  paste0("+ i(labor_exp_group_2, ref=\"[-Inf,0]\")",
               "+ log(company.group.size)",
               "+ i(job_no_trunc)")
fixedeffs <- paste0("|",
                    "+ worker.ID",
                    "+ OG_BEID",
                    "+ year^NACE_LETTER")

##################################################################################
#### Main regression analysis: MNE employment probability ####

regs <- list()

##### at MNE #####
regs[[1]] <- feols(as.formula(paste0("I(company.type.1==\"MNE\") ~ ",
                                    frm, 
                                    fixedeffs)),
                   cluster="worker.ID",
                   ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
                   nthreads=7,
                   data=reg.table[subsample==TRUE])

#### Save table ######
etable(regs, 
       title="MNE vs other companies",
       file=paste0(map_output_here, "reg_cohort.tex"),
       label="tab:cohort", 
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
                 "Clustered standard errors (worker level) in parantheses."),
       replace = TRUE)

#### Graphical analysis ######

## Prob to be in MNE (paper: black and white)
pdf(file=paste0(map_output_here, "coefplot_cohort_MNE_bw.pdf"), width=16*0.7, height=9*0.7)
estimates <- cohort.estimates(regs[[1]], workerfe_interaction = FALSE, percentile_means = NA)
cohort.plot(estimates, workerfe_interaction = FALSE, horizontal = TRUE, tit=NA, graycolors=TRUE)
dev.off()

## Prob to be in MNE (presentation: color)
pdf(file=paste0(map_output_here, "coefplot_cohort_MNE_col.pdf"), width=16*0.7, height=9*0.7)
estimates <- cohort.estimates(regs[[1]], workerfe_interaction = FALSE, percentile_means = NA)
cohort.plot(estimates, workerfe_interaction = FALSE, horizontal = TRUE, tit=NA, graycolors=FALSE)
dev.off()

# Also output estimates to a .txt file
sink(paste0(map_output_here, "coefs_cohort_MNE.txt"))
estimates.table(estimates, df = degrees_freedom(regs[[1]], type="t"))
sink()

###### Wald tests ###### 

# H0: Prob of MNE employment with labor market experience <= 6 years = Prob of MNE employment with labor market experience > 6 years

htest <- linearHypothesis(regs[[1]], c(paste0("+ labor_exp_group_2::(0,1]",
                                              "+ labor_exp_group_2::(1,2]", 
                                              "+ labor_exp_group_2::(2,3]",
                                              "+ labor_exp_group_2::(3,4]",
                                              "+ labor_exp_group_2::(4,5]",
                                              "+ labor_exp_group_2::(5,6]",
                                              "=",
                                              "+ labor_exp_group_2::(6,7]",
                                              "+ labor_exp_group_2::(7,8]",
                                              "+ labor_exp_group_2::(8,9]",
                                              "+ labor_exp_group_2::(9,10]",
                                              "+ labor_exp_group_2::(10, Inf]")),
                          vcov = vcov(regs[[1]]),
                          coef = regs[[1]]$coefficients,
                          test = "Chisq", verbose= TRUE)

# write output
sink(paste0(map_output_here, "Waldtest.txt"))
htest
sink()

