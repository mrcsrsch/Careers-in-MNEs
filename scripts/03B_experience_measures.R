##################################################################################
##### This script subsets to the largest connected set 
##### and calculates the accumulated days of experience for the main analysis #####
##################################################################################

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis run in data.table version 1.14.2 - consider up- or downgrading")
if (!require("xtable")) install.packages("xtable"); library("xtable") # .tex of matrices/data.tables
if (packageVersion("xtable")!="1.8.4") warning("Analysis run in xtable version 1.8.4 - consider up- or downgrading")
if (!require("lfe")) install.packages("lfe"); library("lfe") # to find connected set
if (packageVersion("lfe")!="2.8.7.1") warning("Analysis run in lfe version 2.8.7.1 - consider up- or downgrading")

#### output dir #####
if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
map_output_here <- paste0(map_output, "analysis/")

#### load data  ####
POLIS_graduates <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates.rds"))

##################################################################################
##################################################################################
#### find largest connected OG_BEID-RINP set in POLIS_graduates (for fixed effect estimation (AKM 02)) #####

connected <- compfactor(list(f1=factor(POLIS_graduates$OG_BEID), f2=factor(POLIS_graduates$worker.ID)))
POLIS_graduates[, connected.set := connected] 
POLIS_graduates[, .(companies=uniqueN(OG_BEID), workers=uniqueN(worker.ID), obs=.N),by=(connected.set==1)]

POLIS_graduates <- POLIS_graduates[connected.set==1, !c("connected.set")]
rm(connected)
##################################################################################

# loop through 3 different company  type definitions
for  (ex in c("company.type", "company.type.1", "company.type.perm")){
  #### Create summary stats #####
  ##### Simple overview table #####
  tt <- POLIS_graduates[, .(companies=uniqueN(OG_BEID), workers=uniqueN(worker.ID), obs=.N),by=eval(paste0(ex))]
  tt2 <- POLIS_graduates[, .(companies=uniqueN(OG_BEID), workers=uniqueN(worker.ID), obs=.N)]
  tt <- rbindlist(list(tt, tt2), fill=TRUE)
  rm(tt2)
  
  print(xtable(tt, caption="Data overview", label="tab:data_overview",
               digits=0),
        include.rownames = FALSE,
        file = paste0(map_output_here, "table_data_overview_", ex, ".tex"))
}

##################################################################################
#### Tenure & experience measures ####
# first take all jobs, calculate number of tenure days per company.type.1 (experience measure)
## order POLIS_graduates
setorderv(POLIS_graduates, cols=c("worker.ID", "year", "Nemployer.year"))

## which types?
types <-  POLIS_graduates[, unique(company.type.1)]


## loop through them
for (type in types){
  # message
  cat(paste0(Sys.time(), " running ", which(types==type), "/", length(types)), "\n")
  # calculate cumulative sum of years in company.type.1 and lag by one period
  POLIS_graduates[, paste0("exp_years_", type, "_", "1") := c(0, c(cumsum(fifelse(company.type.1==type, tenure_days_OGBEID_year,0))/365)[-.N]), by=worker.ID] 
  # per OG_BEID spell add first obs as experience measure (we look at across firm returns)
  POLIS_graduates[, paste0("exp_years_", type, "_", "1") := eval(as.symbol(paste0("exp_years_", type, "_", "1")))[1], by=c("worker.ID", "OG_BEID", "spell.id")]
  
  # cut tenure profile into splines
  POLIS_graduates[, paste0("exp_group_", type, "_", "1") := cut(eval(as.symbol(paste0("exp_years_", type, "_", "1"))), breaks=c(-Inf, seq(0,10,1), Inf), include.lowest=TRUE)]
}


# tenure within the company (OG_BEID) 
setorderv(POLIS_graduates, cols=c("worker.ID", "year", "Nemployer.year")) #reorder to be save
POLIS_graduates[, tenure_years_OGBEID_spell := cumsum(tenure_days_OGBEID_year)/365, by=c("worker.ID", "OG_BEID", "spell.id")]
POLIS_graduates[, tenure_group_OGBEID_spell := cut(tenure_years_OGBEID_spell, breaks=c(-Inf, seq(1,10,1), Inf), include.lowest=TRUE) ]


##################################################################################
#### save result and clean up ####
# save result over results of 03A_combine_POLIS_graduates
saveRDS(POLIS_graduates, file=paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds"), compress = TRUE)
