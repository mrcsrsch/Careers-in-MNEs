##################################################################################
### IDENTIFY GRADUATES IN POLIS #### 
# this script detrends wages & identifies graduates in POLIS
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (!require("fixest")) install.packages("fixest"); library("fixest")

#### output dir #####
if (!dir.exists(paste0(map_data_analysis, "step3/"))) dir.create(paste0(map_data_analysis, "step3/"))

#### load data  ####
POLIS <- readRDS(paste0(map_data_analysis, "step2/POLIS.rds")) # POLIS
DIPLOMA <- readRDS(paste0(map_data_analysis, "step2/DIPLOMA.rds")) # graduations
OGs <- readRDS(paste0(map_data_analysis, "step2/OG_company_types.rds")) # company types and NACE LETTERS 
##################################################################################
##################################################################################
#### POLIS manipulations ####
POLIS <- merge(POLIS, OGs, by=c("OG_BEID", "year"), all.x=TRUE)

###### detrend log(hwage) and log(lhwage_basic) over whole dataset ####

# by industry-year
for (i in c("hwage", "hwage_basic")){
  detrend <- feols(fml=as.formula(paste0("log(", i, ") ~ ",
                                         "1 | NACE_LETTER^year ")),
  data=POLIS)
  POLIS[, paste0("l", i, "_detrended") := detrend$residuals]
}

# clean up
rm(detrend) 

##################################################################################
#### create POLIS_graduates ######
# merge POLIS and DIPLOMA, remove all workers without diploma records
# rename POLIS to avoid confusion
POLIS_graduates <- merge(POLIS, DIPLOMA, by="worker.ID")
rm(POLIS, DIPLOMA)

##### initial data processing #####

# number the jobs a graduate holds
# order POLIS_graduates
setorderv(POLIS_graduates, cols=c("worker.ID", "year", "Nemployer.year"))
# add a OGBEID-spell.id column, use this to find order of job spells, delete further down
POLIS_graduates[, OG_BEID_spell := paste(OG_BEID, "-", spell.id)]
# ...as position of unique ID in that column
POLIS_graduates[, job.number := match(OG_BEID_spell, unique(OG_BEID_spell)), by=worker.ID]

###### select graduates #####
# Identify workers that within 2 years of graduation start a fulltime job where they stay for at least 6 months (365/2 days) 
# Idea is that this removes jobs at the local restaurant while searching for other employment
# Identify candidate observations
# calculate max tenure per OGBEID spell.id
setorderv(POLIS_graduates, cols=c("worker.ID", "year", "Nemployer.year")) # to be sure, order again
POLIS_graduates[, tenure_days_OGBEID_spell := cumsum(tenure_days_OGBEID_year), by=c("worker.ID", "OG_BEID", "spell.id")]
POLIS_graduates[, candidate := max(tenure_days_OGBEID_spell)>=182, by=c("worker.ID", "OG_BEID", "spell.id")] #just to be sure, given 2A these are all the spells
# only keep workers with at least 1 employment spell of > 6 months
POLIS_graduates[, keep := any(candidate), by=worker.ID]
POLIS_graduates[, .(OG_BEID = uniqueN(OG_BEID), RINP=uniqueN(worker.ID), obs=.N), by=keep]
POLIS_graduates <- POLIS_graduates[keep ==TRUE, !c("keep")]

# add years since graduation for each observation (as a crude measure)
POLIS_graduates[, years_since_graduation := year-degree.year]
# calculate distance in month between each job.entry (spell-level) and degree date
POLIS_graduates[, distance_days := as.integer(difftime(job.entry, degree.date, units="days"))+1]

# if job.entry is 01-01-2006 (beginning of panel), set distance days to a very large number --> cannot reasonably identify when job actually started
POLIS_graduates[job.entry=="2006-01-01", distance_days := 99*365]

# find job spells that start within [0.0, 3.0] years of graduation 
# jobs < 0.0 (cluster of entries) might be theses internships
# jobs > 2.0 (cluster of entries) 
# check:
#plot(POLIS_graduates[degree.year>=2009 & candidate==TRUE, round(min(distance_days)/365,1), by=worker.ID][V1>-2 & V1<5, .N, by=V1]) --> min dist could be 0
#plot(POLIS_graduates[degree.year>=2009 & candidate==TRUE, round(min(distance_days)/365,1), by=worker.ID][V1>-1 & V1<2, .N, by=V1]) --> higher entries are further away --> set 0 as min dist
#plot(POLIS_graduates[candidate==TRUE, round(min(distance_days)/365,1), by=worker.ID][V1>-1 & V1<4, .N, by=V1]) --> looks similar
# plot(POLIS_graduates[candidate==TRUE & degree.year>=2006, round(min(distance_days)/365,1), by=worker.ID][V1>=0 & V1<4, .N, by=V1])
# plot(POLIS_graduates[candidate==TRUE & degree.year>=2006 & degree.highest==0, round(min(distance_days)/365,1), by=worker.ID][V1>=0 & V1<4, .N, by=V1])
# plot(POLIS_graduates[candidate==TRUE & degree.year>=2006 & degree.highest==2, round(min(distance_days)/365,1), by=worker.ID][V1>=0 & V1<4, .N, by=V1])
# plot(POLIS_graduates[candidate==TRUE & degree.year>=2006 & degree.highest==3, round(min(distance_days)/365,1), by=worker.ID][V1>=0 & V1<4, .N, by=V1])
# --> >1 & <3 years could be e.g. masters abroad. Let's keep these workers. 

POLIS_graduates[candidate==TRUE, first_job := all(distance_days>=0) & all(distance_days<=3*365),  by=c("worker.ID", "OG_BEID", "spell.id")]  # distance days identifies distance to entry in OG_BEID
POLIS_graduates[, c("job.entry", "degree.date", "degree.year", "distance_days") := NULL]

# find first job out of candidate jobs
POLIS_graduates[first_job==TRUE, first_job := job.number == min(job.number), by=worker.ID]
POLIS_graduates[is.na(first_job), first_job := FALSE]
# remove workers without a first long job
POLIS_graduates[, keep := any(first_job), by=worker.ID]
POLIS_graduates[, .(OG_BEID = uniqueN(OG_BEID), RINP=uniqueN(worker.ID), obs=.N), by=keep]
POLIS_graduates <- POLIS_graduates[keep ==TRUE, !c("keep", "candidate")]

##################################################################################
#### remove earlier jobs than graduate entry ######
# add identifier whether worker held job before first_job
# check if first_job is in position 1
# if this gives an error or warning, something is wrong above: >1 job.number assigned to first_job
POLIS_graduates[, graduate.prev.experience := unique(job.number[first_job])!=1 , by=worker.ID]

# remove observations before first_job
POLIS_graduates[, keep := job.number >= job.number[first_job][1], by=worker.ID]
POLIS_graduates <- POLIS_graduates[keep==TRUE, !c("keep")]

# reassign job.number 
setorderv(POLIS_graduates, cols=c("worker.ID", "year", "Nemployer.year"))
POLIS_graduates[, job.number := NULL]
POLIS_graduates[, job.number := match(OG_BEID_spell, unique(OG_BEID_spell)), by=worker.ID]
POLIS_graduates[, OG_BEID_spell := NULL]

# reassign Nemployer.year
setorderv(POLIS_graduates, cols=c("worker.ID", "year", "Nemployer.year"))
POLIS_graduates[, Nemployer.year := NULL]
POLIS_graduates[, Nemployer.year := 1:.N, by=c("worker.ID", "year")]


#### remove single observation workers ######

POLIS_graduates[, keep := .N>1, by=worker.ID]
POLIS_graduates[, .(OG_BEID = uniqueN(OG_BEID), RINP=uniqueN(worker.ID), obs=.N), by=keep]
POLIS_graduates <- POLIS_graduates[keep==TRUE, !c("keep")]

##################################################################################
#### save result ####
# save result
saveRDS(POLIS_graduates, file=paste0(map_data_analysis, "step3/POLIS_graduates.rds"), compress = TRUE)
