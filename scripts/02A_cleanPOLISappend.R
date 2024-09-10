##################################################################################
### Append and clean yearly POLIS (OG_BEID level) #### 
# This script appends and cleans the yearly POLIS data by identifying OG_BEID spells
# It also aggregates to the OG_BEID level and imputes missing OG_BEIDs. 
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (!require("DescTools")) install.packages("DescTools"); library("DescTools") # to find overlapping date ranges

#### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step2/"))) dir.create(paste0(map_data_analysis, "step2/"))
##################################################################################
##################################################################################

#### load POLIS #######

POLIS <- list()
k <- 0
for (current_year in 2006:2021){ 
  cat(paste(Sys.time(), ":", current_year), "\n")
  k <- k +1
  POLIS[[k]] <- readRDS(paste0(map_data_analysis, "step1/SPOLIS_yearly/POLIS_", current_year, ".rds"))
}
rm(current_year, k)

POLIS <- rbindlist(POLIS)
gc()


##### merge with numeric worker ID #########
RINPtra <- readRDS(paste0(map_data_analysis, "step1/RINP_workerID_translation.rds"))

POLIS[, .N]
POLIS <- merge(POLIS, RINPtra, by="RINP")
POLIS[, .N]
POLIS[, RINP := NULL]
rm(RINPtra)
gc()

##### add OG_BEID (company group) identifier and control for gaps #####
beog <- readRDS(paste0(map_data_analysis, "step1/company/beog.rds"))
POLIS <- merge(POLIS, beog, by=c("SBEID", "year"), all.x=TRUE)

##################################################################################
#### Control for gaps and assign missing OG BEIDs  #####
## (some SBEIDs have a missing OG_BEID in some years, but the same OG_BEID in others. That OG_BEID sometimes only contains one firm. E.g. SBEID==66683866)
## also check: SBEIDs[SBEID==SBEIDs[is.na(OG_BEID),sample(SBEID, 1)],]
## extract SBEID-level table
SBEIDs <- POLIS[, .(OG_BEID=OG_BEID[1]), by=c("SBEID", "year")]
setorderv(SBEIDs, cols=c("SBEID", "year"))
# SBEIDs[, sum(is.na(OG_BEID))/.N] --> 1.2% have missing OG_BEID
### add number of SBEIDs per OG_BEID year --> seems to mostly occur at edges
SBEIDs[, OG_BEID_no_SBEIDs := .N, by=c("OG_BEID", "year")]
### find firms with missing OG_BEID and same OG_BEID for all other observations. Replace OG_BEID there
#### the approach below will change company groups somewhat. I do this because OG_BEIDs seem to be somewhat inconsistent, where firms drop out for single years (beginning/end of firm) for no apparent reason.
#### As the tenure measures rely on OG_BEIDs and so do job moves, this reduces potential false job moves in the data. 
#### Check e.g. OG_BEID==41662032, with SBEID==11545836 and SBEID===11545836 in 2010 (SBEID survives one 1 year with regular number of employees, but has no OG_BEID there)
SBEIDs[, c("candidate", "replacement") := {# find candidate positions
                                           candidate <- .N>1 & is.na(OG_BEID) & uniqueN(OG_BEID[!is.na(OG_BEID)==1])
                                           # store OG_BEID vector
                                           replacement <- OG_BEID
                                           if (sum(candidate)==1){
                                             # if only one missing, just replace without further check.
                                             replacement[which(candidate==TRUE)] <- replacement[!candidate][1]
                                           } else if (sum(candidate) > 1){
                                             # if more than one missing, check if OG_BEID consists of only 1 firm
                                             if (SBEIDs[OG_BEID==replacement[!candidate][1], all(OG_BEID_no_SBEIDs==1)]) replacement[which(candidate==TRUE)] <- replacement[!candidate][1]
                                           }
                                            
                                           list(candidate, replacement)}, by=SBEID]
##### check with there are cases left
# SBEIDs[SBEID %in% SBEIDs[, any(is.na(replacement)), by=SBEID][V1==TRUE, SBEID], .(V2=uniqueN(OG_BEID)==1), by=SBEID][, all(V2)] # --> no cases left
##### replace 
SBEIDs[, imputed.OG_BEID := is.na(OG_BEID) & !is.na(replacement)]
SBEIDs[, OG_BEID := replacement]

### assign new OG_BEID where missing
start <- SBEIDs[, max(OG_BEID, na.rm=TRUE)]+10^8
new <- SBEIDs[is.na(OG_BEID), .(SBEID=unique(SBEID))][, .(SBEID, OG_BEID_new=start:(start-1+.N))]
SBEIDs <- merge(SBEIDs, new, by=c("SBEID"), all.x=TRUE)
SBEIDs[is.na(OG_BEID), OG_BEID := OG_BEID_new]
# update SBEIDs per OG_BEID
SBEIDs[, SBEIDs_in_OG := .N, by=c("OG_BEID", "year")]

#### merge back with POLIS
POLIS[, OG_BEID := NULL]
POLIS <- merge(POLIS, SBEIDs[, c("SBEID", "year", "OG_BEID", "SBEIDs_in_OG")], by=c("SBEID", "year"), all.x=TRUE)
POLIS[, sum(is.na(OG_BEID))]==0

#### save translation table with imputation dummy (to correct MNE status)
saveRDS(SBEIDs[, c("SBEID", "year", "OG_BEID", "SBEIDs_in_OG", "imputed.OG_BEID")], paste0(map_data_analysis, "step2/OGBEID_SBEID_translation.rds"), compress=TRUE)
rm(start, new, SBEIDs)
##################################################################################
#### Aggregate to OG_BEID level #######
# Essentially I am now better controlling for jobs that switch within OG_BEIDs (check some workers from example above)
# But this means there are redundant SBEID observations, so let's aggregate again
POLIS[, obs := .N, by=c("worker.ID", "OG_BEID", "year")]
POLIS[, sum(obs>1)/.N] #3% of observations
## only need to aggregate for obs > 1
POLIS[obs>1, `:=` (job.entry = min(job.entry),
                       job.exit = max(job.exit),
                       SCONTRACTSOORT = min(SCONTRACTSOORT),
                       SBASISLOON = sum(SBASISLOON),
                       SBIJZONDEREBELONING = sum(SBIJZONDEREBELONING),
                       SLNOWRK = sum(SLNOWRK),
                       SOVERWERKUREN = sum(SOVERWERKUREN),
                       SREGULIEREUREN = sum(SREGULIEREUREN),
                       female = female[1],
                       age = age[1],
                       SBEIDs_in_OG = SBEIDs_in_OG[1]
                       ), by=c("worker.ID", "OG_BEID", "year")]

# remove unnecesary cols
POLIS[, c("SBEID", "tenure.days.year", "obs") := NULL]

# remove SBEID and keep unique
POLIS <- unique(POLIS)

# recalculate tenure days
# Note that here I assume away gaps between jobs within an OG_BEID. 
POLIS[, tenure_days_OGBEID_year :=  as.integer(difftime(job.exit, job.entry, units="days"))+1]

###### calculate OG BEID characteristics #####
# firm size based on full sample (number of fulltime (fte > 0.7) employees, aged 18-65) 
POLIS[, company.group.size := .N, by=c("OG_BEID", "year")]
##################################################################################
#### find employment spells (OG_BEID) ######
# note that at this point job entries and exits lie within the same year
setorderv(POLIS, cols = c("worker.ID", "OG_BEID", "job.entry")) 

# set job entry and exit across years

# first find those without breaks in employment history (vast majority of observations)
# compare evolution of tenure days within OG_BEID. This will identify tenures without break since first entry 
# repeat this process until each adjacent spell has a spell id 
spell <- 0
POLIS[, spell.id := NA_integer_]

while (POLIS[is.na(spell.id), .N] > 0 & spell < 17) {
  # update spell
  spell <- spell + 1
  # message
  cat(paste(Sys.time(), ": loop", spell), "\n")
  
  # calculate two measures of cumulative tenure days for worker-firm matches without spell id
  POLIS[is.na(spell.id), c("total1", "total2") := .(cumsum(tenure_days_OGBEID_year),  
                                     as.integer(difftime(job.exit, job.entry[1], units="days"))+1),  by = c("worker.ID", "OG_BEID")]
  # reassign job.entry and exit vars, calculate tenure per row, assign spell id
  # allow for a margin of error of 182 days (won't count those days as tenure, but links jobs)
  POLIS[is.na(spell.id) & (abs(total1-total2)<183), c("job.entry", "job.exit", 
                          "tenure_days_OGBEID_spell", "spell.id") := .(job.entry[1], job.exit[.N],
                                              total1, spell), by = c("worker.ID", "OG_BEID")]
  POLIS[,  c("total1", "total2") := NULL]
} 

# just to be safe, delete all workers without a spell.id. These should be none. 
POLIS[is.na(spell.id), .N]
POLIS <- POLIS[!(worker.ID %in% POLIS[is.na(spell.id), unique(worker.ID)]), ]

###### remove spells < 6 months ##### 
# let's remove job spells short than 6 months (above and below I control for gaps using a similar cutoff)
POLIS[, remove := max(tenure_days_OGBEID_spell)<183, by=c("worker.ID", "OG_BEID", "spell.id")]
POLIS[, .N, by=remove]
POLIS <- POLIS[remove==FALSE, !c("remove")]

##################################################################################
#### pick main job #####
# this takes around 30 mins when using overlap intervals

# find workers with overlapping employment spells
## these must have > 1 obs in any year
POLIS[, obs := .N, by=c("worker.ID", "year")]
## for worker-years with obs >1 get unique combinations of job.entry and job.exit
tt <- unique(POLIS[obs>1, c("worker.ID", "OG_BEID", "job.entry", "job.exit")]) 
tt[, obs := .N, by=worker.ID] # only need workers with > 1 OG_BEID-level job
tt <- tt[obs>1, !c("obs")]
setorderv(tt, cols=c("worker.ID", "job.entry"))
## find workers with overlapping job.entry and job.exit, allow for a maximum overlap of 182 days
overl <- function(entry, exit){
  for (i in 1:length(entry)){
    res <- any( Overlap(c(entry[i], exit[i]),cbind(entry[-i], exit[-i])) > 182)
    if (res==TRUE) break
  }
  return(res)
}

tt[, overlaps := overl(entry=job.entry, exit=job.exit), by=worker.ID]

## get worker.IDs with overlaps
ids <- tt[overlaps==TRUE, unique(worker.ID)]
POLIS[, uniqueN(worker.ID[worker.ID %in% ids])/uniqueN(worker.ID)] 

## delete these workers
POLIS <- POLIS[!(worker.ID %in% ids), ]
POLIS[, .N]

# clean up
rm(tt, ids, overl)
POLIS[, obs := NULL]

##################################################################################
### adjust tenure days in leap years #### 
POLIS[tenure_days_OGBEID_year>365, tenure_days_OGBEID_year := 365]

##################################################################################
#### assign ids within year #######
# assign ID in panel: c(worker.ID, year, Nemployer.year)
# jobs are unique at any point in time, but there may be >1 employer/year for job movers

# first order again
setorderv(POLIS, cols = c("worker.ID", "year", "job.entry")) 
# assign ID
POLIS[, Nemployer.year := 1:.N, by=c("worker.ID", "year")]
# set as key
setkeyv(POLIS, cols=c("worker.ID", "year", "Nemployer.year"))

#### calculate nominal hourly wages #####
POLIS[, hwage := (SBASISLOON+SBIJZONDEREBELONING+SLNOWRK)/(SOVERWERKUREN + SREGULIEREUREN)]
POLIS[, hwage_basic := SBASISLOON/SREGULIEREUREN] # an issue with hwage might be unreported over time that is compensated through bonuses (e.g. managers)

##################################################################################
#### save result ####

# only keep some of the vars (need job.entry to identify Graduates, delete later because memory consuming)
POLIS <- POLIS[, c("worker.ID", "year", "Nemployer.year", "job.entry", "OG_BEID", "spell.id",
                   "age", "female",
                   "hwage", "hwage_basic",
                   "SBASISLOON", "SBIJZONDEREBELONING", "SLNOWRK", "SREGULIEREUREN", "SOVERWERKUREN", "deeltijdfactor",
                   "SCONTRACTSOORT", 
                   "tenure_days_OGBEID_year",
                   "company.group.size")]

                                               
# save 
saveRDS(POLIS, paste0(map_data_analysis, "step2/POLIS.rds"), compress=TRUE)

