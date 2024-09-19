##################################################################################
### Robustness check: MNE vs. location-specific experience #####
# This script loads the company data from the KIO SQL servers and performs basic manipulations
# If you do not have access to this SQL server, I recommend that you skip the SQL calls and load the respective datasets from disc.
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")
if (!require("xtable")) install.packages("xtable"); library("xtable") # .tex of matrices/data.tables
if (packageVersion("xtable")!="1.8.4") warning("Analysis run in xtable version 1.8.4 - consider up- or downgrading")
if (!require("RODBC")) install.packages("RODBC"); library("RODBC")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_output, "robustness/"))) dir.create(paste0(map_output, "robustness/"))
if (!dir.exists(paste0(map_output, "robustness/location_exp/"))) dir.create(paste0(map_output, "robustness/location_exp/"))

map_output_here <- paste0(map_output, "robustness/location_exp/") # for final outputs


#### load data  ####
# main regression table
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
# Full wage date
#POLIS <- readRDS(paste0(map_data_analysis, "step2/POLIS.rds")) # POLIS
# MNE and nace industries on OG level
#OGs <-readRDS(paste0(map_data_analysis, "step2/OG_company_types.rds"))
# load OG_BEID, SBEID translation
OG_SBEIDs <- readRDS(paste0(map_data_analysis, "step2/OGBEID_SBEID_translation.rds"))

##################################################################################
##################################################################################
### get location data from regiobase ###

##### set SQL connection ###### 
server_id <- NA # specify server ID here
database_id <- NA # specify database ID here 

if (is.na(server_id) | is.na(database_id)) warning("You need to specify the server and database ids for this script to run. Alternatively, you could remove the SQL calls and load the respective datasets from disc.")

# open SQL connection
dbhandle <- odbcDriverConnect(paste0("driver={SQL Server};server=", server_id, ";database=", database_id, ";trusted_connection = yes"))

##### Get regiobase (ABR_regio) ##### 
regiobase <- list()
i <- 0
for (y in 2006:2021){
  # message
  cat(paste0(Sys.time(), ": ", y), "\n")
  i <- i + 1
  query <- paste0("
                SELECT 
                BE_ID AS SBEID, 
                Jaar as year, 
                PC6 as PC6,
                Verdeelperc_LBE,
                WPactueel
                FROM  [Data].[Regiobase_",y,"_DEF]")
  
  regiobase[[i]] <- sqlQuery(channel = dbhandle, query = query, as.is = TRUE)
}
regiobase <- rbindlist(regiobase) 

# add OG_BEIDs and remove firms that are not in data
regiobase <- merge(regiobase, OG_SBEIDs[, c("SBEID", "year", "OG_BEID")], by=c("SBEID", "year"))

# calculate number of employees per LBE (local entity at PC6 level)
regiobase[, local_workers := WPactueel*as.numeric(Verdeelperc_LBE/100)]

# remove missings
regiobase <- regiobase[!is.na(local_workers), !c("WPactueel", "Verdeelperc_LBE")]

# remove observations with missing PC6
regiobase <- regiobase[!is.na(PC6),]

# add NUTS2 regions, drop non-matched regions (NUTS2 is province)
pc6_nuts2 <- fread(paste0(map_data_source, "LOCATION/pc2020_NL_NUTS-2021_v1.0.csv"))
# extract NUTS2 and postcodes
pc6_nuts2[, NUTS2 := substr(NUTS3,2,5)]
pc6_nuts2[, PC6 := paste0(substr(CODE, 2, 5), paste0(substr(CODE, 7, 8)))]
pc6_nuts2 <- pc6_nuts2[, c("PC6", "NUTS2")]
# add to regiobase
regiobase[, .N]
regiobase <- merge(regiobase, pc6_nuts2, by="PC6")
regiobase[, .N]

# aggregate to OG_BEID-year-NUTS2 level (count workers)
regiobase <- regiobase[, .(local_workers = sum(local_workers)), by=c("OG_BEID", "year", "NUTS2")]

# identify OG_BEIDs that are in multiple regions
regiobase[, multi_region := .N>1, by=c("OG_BEID", "year")]

# what share of firm-years are in multiple regions? 
regiobase[, sum(multi_region)/.N*100]

# for multi-region firms, assign NUTS2 based on highest employment share
regiobase[multi_region==T, share := local_workers/sum(local_workers), by=c("OG_BEID", "year")]
# flag region with highest employment share
regiobase[multi_region==F, keep := T]
regiobase[multi_region==T, keep := share==max(share), by=c("OG_BEID", "year")]
# keep only those
regiobase <- regiobase[keep==T, ]
# check if any OG_BEID-years are still double
regiobase[multi_region==T, check := .N>1, by=c("OG_BEID", "year")]
regiobase[multi_region==T, sum(check)/.N] # ca. 20% of multiregion firms have no employment center
# for remaining duplicates, keep a random one
regiobase[check==T, keep := c(T, rep(F,.N-1)), by=c("OG_BEID", "year")]
# keep only those
regiobase <- regiobase[keep==T, ]
# check again
regiobase[, check:=NULL]
regiobase[, check := .N>1, by=c("OG_BEID", "year")]
regiobase[check==T,] #none

# add to reg.table
reg.table <- merge(reg.table, regiobase[, c("OG_BEID", "year", "NUTS2")], by=c("OG_BEID", "year"), all.x=T)
reg.table[is.na(NUTS2), NUTS2 := "unknown"] # less than <1 percent of obs 
##################################################################################
# NUTS2 division by firm type 

firm_location <- reg.table[NUTS2!="unknown", .(NUTS2=NUTS2[1]), by=.(OG_BEID, company.type.1)]
# add province labels
a <- paste0("NL",c(11, 12, 
                  13, 21,
                  22, 23,
                  31, 32,
                  33, 34,
                  41, 42))
b <- c("Groningen", "Friesland", 
               "Drenthe", "Overijssel",
       "Gelderland",       "Flevoland",
       "Utrecht",   "Nort Holland", 
       "South Holland",     "Zeeland", 
       "North Brabant", "Limburg"  )
ab <- data.table(NUTS2 = a, NUTS2_label = b)
rm(a)
firm_location <- merge(firm_location, ab, by="NUTS2")
## Calculate division over labels
firm_location <- firm_location[, sapply(b, function(x) uniqueN(OG_BEID[NUTS2_label==x])/uniqueN(OG_BEID)*100, simplify = F), by=company.type.1]


## apply rounding
vars <- names(firm_location)[-1]
firm_location[, paste0(vars) := lapply(.SD, function(x) round(x, 2)), .SDcols = vars]

## transpose and save
firm_location <- t(firm_location)

print(xtable(firm_location[, c(2,3,1)], caption="Summary Stats (firm level)", label="tab:firm_location",
             digits=2),
      # include.rownames = TRUE,
      file = paste0(map_output, "analysis/", "firm_location_stats.tex"))


##################################################################################

#### calculate previous years of experience in same location as current one ######
setorderv(reg.table, c("worker.ID", "year", "Nemployer.year"))

regions <- reg.table[, unique(NUTS2)]
for (region in regions){
  # message
  cat(paste0(Sys.time(), " running ", which(regions==region), "/", length(regions)), "\n")
  # calculate cumulative sum of years in NUTS2_numeric
  reg.table[, paste0("exp_years_", region) := c(0, c(cumsum(fifelse(NUTS2==region, tenure_days_OGBEID_year,0))/365)[-.N]), by=worker.ID] 
  # cut tenure profile into splines
  reg.table[, paste0("exp_group_", region) := cut(eval(as.symbol(paste0("exp_years_", region))), breaks=c(-Inf, seq(0,10,1), Inf), include.lowest=TRUE)]
}

#### other manipulations ######
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


##################################################################################
#### Main regression analysis: Non-linear returns #######

reg_main <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                        within_firm,
                                        within_firm_interact,
                                        within_firm_job,
                                        across_firm,
                                        across_firm_interact,
                                        paste0("+", paste0("exp_years_", regions, collapse = "+")),
                                        controls,
                                        "",
                                        "| ",
                                        # fixed effects
                                        "worker.ID ",
                                        "+ OG_BEID",
                                       #"+ location_exp_group^NUTS2_numeric", # add location specific experience
                                       #paste0("+", paste0("exp_group_", regions, collapse = "+")), # add all location-specific exp profiles 
                                        ""
)),
cluster="worker.ID",
ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
nthreads=7,
data=reg.table[subsample==TRUE])

##### Print table ####
etable(reg_main, #lapply(paste0("regs[[",length(regs):1, "]]"), function(x) eval(parse(text=x))), 
       file=paste0(map_output_here, "reg_wages_non_linear_", ifelse(across.firm.interactions, "w_", "wo_"), "across", "_location.tex"),  
       title="Wage profiles",
       label="tab:location_exp", 
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
if (across.firm.interactions){
  pdf(file=paste0(map_output_here, "coefplot_across_firm_w_across_location.pdf"), width=2.10*5.5, height=2.97*5.5)
  
  par(mfrow=c(length(types),2))
  
  # within domestic firm
  # total
  estimates <- total.general.prem(reg_main, exp.types = types, k=1)
  company.type.plot(estimates, type="Total", legend.add = FALSE, only.legend = FALSE, highlight.MNE = TRUE)
  
  # empty plot with legend
  company.type.plot(estimates, type="Total", legend.add = FALSE, only.legend = TRUE, highlight.MNE = TRUE)
  
  # within other types than domestic
  for (type in types){
    if (type=="DOMESTIC") next
    # Total
    estimates <- total.company.type(reg_main, company.type= type, exp.types=types, k=1)
    company.type.plot(estimates, type="Total", legend.add = FALSE, only.legend = FALSE, highlight.MNE = TRUE)
    
    # Also output estimates to a _location.txt file
    sink(paste0(map_output_here, "coefs_across_firm_w_across_", type, "_total_location.txt"))
    estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
    sink()
    
    
    # Excess
    estimates <- excess.company.type(reg_main, company.type = type, exp.types=types, k=1)
    company.type.plot(estimates, type="Excess", legend.add = FALSE, only.legend = FALSE, highlight.MNE = TRUE)
    
    # Also output estimates to a _location.txt file
    sink(paste0(map_output_here, "coefs_across_firm_w_across_", type, "_excess_location.txt"))
    estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
    sink()
  }
  
  dev.off()
} else{
  # black and white (paper)
  pdf(file=paste0(map_output_here, "coefplot_across_firm_wo_across_bw_location.pdf"), width=16*0.7, height=9*0.7)
  estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
  company.type.plot(estimates, type="Total", legend.add = TRUE, 
                    legend.names = c("MNE", "Int. firm"), 
                    only.legend = FALSE, title = "", graycolors = TRUE)
  dev.off()
  
  # in color (presentation)
  pdf(file=paste0(map_output_here, "coefplot_across_firm_wo_across_col_location.pdf"), width=16*0.7, height=9*0.7)
  estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
  company.type.plot(estimates, type="Total", legend.add = TRUE, 
                    legend.names = c("MNE", "Int. firm"), 
                    only.legend = FALSE, title = "", graycolors = FALSE, highlight.MNE = TRUE)
  dev.off()
  
  # Also output estimates to a _location.txt file
  sink(paste0(map_output_here, "coefs_across_firm_wo_across_location.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
  
}

rm(estimates)

###### B) Stayer wage growth ###### 
if (across.firm.interactions){
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_w_across_location.pdf"), width=4*3.2, height=3*3.2)
  estimates <- stayer.wages.estimates(reg_main, company.types=types, k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE)
  dev.off()
  
  # Also output estimates to a _location.txt file
  sink(paste0(map_output_here, "coefs_stayer_wages_w_across_location.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
  
} else{
  # black and white (paper)
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_wo_across_bw_location.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                    legend.names= c("MNE", "Int. firm"), graycolors=TRUE)
  dev.off()
  
  # color (presentation)
  pdf(file=paste0(map_output_here, "coefplot_stayer_wages_wo_across_col_location.pdf"), width=16*0.7, height=9*0.7)
  estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
  stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                    legend.names= c("MNE", "Int. firm"), graycolors=FALSE)
  dev.off()
  
  # Also output estimates to a _location.txt file
  sink(paste0(map_output_here, "coefs_stayer_wages_wo_across_location.txt"))
  estimates.table(estimates, df = degrees_freedom(reg_main, type="t"))
  sink()
}
rm(estimates)