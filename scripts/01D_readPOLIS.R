##################################################################################
#### Prepare Polis ####
# output: yearly panel (2006 - 2021 seperate), worker-SBEID-level, 
# ages 18-65, full time employment (> 0.7 fte), non-stagairs, wage/hour components, gender
##################################################################################

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")


#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))
if (!dir.exists(paste0(map_data_analysis, "step1/SPOLIS_yearly/"))) dir.create(paste0(map_data_analysis, "step1/SPOLIS_yearly/"))

#### functions #####
# set fread paths
get.path <- function(current_year){
  if (current_year < 2010) {
    return(paste0(map_data_source, "POLIS/POLISBUS", current_year, "BUSV1.csv"))}
  else return(paste0(map_data_source, "POLIS/SPOLISBUS", current_year, "BUSV1.csv"))
}

get.types <- function(current_year){
  if (current_year < 2010){
    return(c(BEID = "character", RINPERSOONS = "character", RINPERSOON = "character",  
             AANVSSBBUS = "character", EINDSSBBUS = "character",
             BASISLOON = "integer", LNOWRK = "integer", BIJZONDEREBELONING = "integer", 
             REGULIEREUREN = "numeric", OVERWERKUREN = "numeric",
             CONTRACTSOORT = "character",
             DATUMAANVANGIKV  = "character",
             DATUMEINDEIKV = "character",
             VOLTIJDDAGEN = "numeric",
             SOORTBAAN = "integer"))
  } else return(c(SBEID = "character", RINPERSOONS = "character", RINPERSOON = "character",  
                  AANVSSBBUS = "character", EINDSSBBUS = "character",
                  SBASISLOON = "integer", SLNOWRK = "integer", SBIJZONDEREBELONING = "integer", 
                  SREGULIEREUREN = "numeric", SOVERWERKUREN = "numeric",
                  SCONTRACTSOORT = "character",
                  SDATUMAANVANGIKO = "character",
                  SDATUMEINDEIKO = "character",
                  SVOLTIJDDAGEN = "numeric",
                  SSOORTBAAN = "integer"))
}
##################################################################################
##################################################################################
####### Main part #######
#### read in gba here (saves time, costs memory)
gba <- readRDS(paste0(map_data_analysis, "step1/gba.rds"))

#### loop through years to create separate POLIS files #####
for (current_year in 2006:2021){
  cat(paste(Sys.time(), ":", current_year), "\n")
  
  # read POLIS
  POLIS <- fread(get.path(current_year), dec=",", sep=";", header = TRUE, showProgress = TRUE, data.table = TRUE, nThread = 4,
                 logical01 = TRUE, 
                 select = get.types(current_year))
  
  # adjust names
  if (current_year < 2010) setnames(POLIS, 1:15, c("SBEID", "RINPERSOONS", "RINPERSOON", 
                                                   "AANVSSBBUS", "EINDSSBBUS",
                                                   "SBASISLOON", "SLNOWRK", "SBIJZONDEREBELONING", 
                                                   "SREGULIEREUREN", "SOVERWERKUREN",
                                                   "SCONTRACTSOORT",
                                                   "SDATUMAANVANGIKO", "SDATUMEINDEIKO", "SVOLTIJDDAGEN", "SSOORTBAAN"))
  
  # note: AANVSSBBUS captures date within month of salary 
  # and SDATUMAANVANGIKO date when contract actually began 
  # I use the first to find fulltime jobs per month. 
  # I use the second to calc. tenure after combining IKOs
  
  ##### basic adjustments #####  
  
  # adjust to date format
  POLIS[, c("AANVSSBBUS", "EINDSSBBUS", "SDATUMAANVANGIKO", "SDATUMEINDEIKO") := .(as.Date(AANVSSBBUS, format="%Y%m%d"),
                                                                                   as.Date(EINDSSBBUS, format="%Y%m%d"),
                                                                                   as.Date(SDATUMAANVANGIKO, format="%Y%m%d"),
                                                                                   as.Date(SDATUMEINDEIKO, format="%Y%m%d"))]
  POLIS[, month := month(AANVSSBBUS)]
  
  
  # paste RINP key
  POLIS[, RINP := paste0(RINPERSOONS, RINPERSOON)]
  POLIS[, c("RINPERSOONS", "RINPERSOON") := NULL]
  
  # change contractsoort to numeric 
  POLIS[SCONTRACTSOORT == "O" | SCONTRACTSOORT == "o", SCONTRACTSOORT := "1"]
  POLIS[SCONTRACTSOORT == "B" | SCONTRACTSOORT == "b", SCONTRACTSOORT := "2"]
  POLIS[SCONTRACTSOORT == "N" | SCONTRACTSOORT == "n", SCONTRACTSOORT := "9"]
  POLIS[, SCONTRACTSOORT := as.integer(SCONTRACTSOORT)] 
  
  
  ##### Main data manipulations #####
  
  # remove fake BEIDs (short-lived, no firm-level data available)
  POLIS <- POLIS[substr(SBEID,1,1) %in% c(1:9),] 
  ## change SBEID to numeric (all character ID are removed)
  POLIS[, SBEID := as.integer(SBEID)]
  
  # remove internships (stagaires). These are mandatory internships (see polis manual)
  POLIS <- POLIS[! SSOORTBAAN %in% c(2), ]

  
  # delete jobs with non-positive hours/wage: these e.g. occur because of remaining holidays or paybacks 
  # not relevant for experience measures
  POLIS <- POLIS[SREGULIEREUREN > 0 & SBASISLOON > 0 & SBIJZONDEREBELONING >= 0 & SLNOWRK >= 0 & SOVERWERKUREN >= 0 ,]
  
  # aggregate jobs at same employer
  
  # find job days per contract (I use this to aggregate the deeltijdfactor)
  # use AANVSBBUS because that lies within same month for all year (for older POLIS SDATUMEINDEIKO is different)
  POLIS[, job.days := as.integer(difftime(EINDSSBBUS, AANVSSBBUS, units="days"))+1]
  
  # aggregate to year level (over SBEIDs)
  setorderv(POLIS, cols=c("RINP", "SBEID", "month"))
  
  POLIS <- POLIS[, `:=`(SBASISLOON = sum(SBASISLOON),
                       SBIJZONDEREBELONING = sum(SBIJZONDEREBELONING),
                       SLNOWRK = sum(SLNOWRK),
                       SOVERWERKUREN = sum(SOVERWERKUREN),
                       SREGULIEREUREN = sum(SREGULIEREUREN),
                       SCONTRACTSOORT = min(SCONTRACTSOORT),
                       job.entry = min(SDATUMAANVANGIKO),  #I'm assuming here that jobs feature no "breaks" throughout the year.
                       job.exit = max(SDATUMEINDEIKO),  #I'm assuming here that jobs feature no "breaks" throughout the year.
                       job.days = sum(job.days), # this var has no meaning, just used to find fulltime employees
                       SVOLTIJDDAGEN = sum(SVOLTIJDDAGEN)), # this var has no meaning, just used to find fulltime employees
                 by=c("SBEID", "RINP")]
  # only keep those vars and remove duplicates
  POLIS <- POLIS[, c("RINP", "SBEID", "SBASISLOON", "SBIJZONDEREBELONING", 
                     "SLNOWRK", "SOVERWERKUREN", "SREGULIEREUREN", "SCONTRACTSOORT",
                     "job.entry", "job.exit", "job.days", "SVOLTIJDDAGEN")]
  POLIS <- unique(POLIS)
  
  # add year and calculate tenure within year
  POLIS[, year := as.integer(current_year)]
  POLIS[, tenure.days.year :=  as.integer(difftime(job.exit, job.entry, units="days"))+1]
  
  
  ##### Main data manipulations: find fulltime employees ####
  # only keep full-time jobs (see SPOLIS manual)
  ######## small sanity check on deeltijd factor #####
  # # full time hours per day, adjusted for that we observe weekends as well (very rough estimates)
  # POLIS[, deeltijd.2 := (SREGULIEREUREN/job.days)/(7.5*21.62/30.41)] # 7.5 hours per day, 21.62 average workingg days per month, 30.41 days in a month
  # POLIS[, cor(deeltijdfactor, deeltijd.2)] # 0.75, seems reasonable
  # # compare distribution
  # cbind(POLIS[, quantile(deeltijd.2, probs= seq(0,1,0.01))], POLIS[, quantile(deeltijdfactor, probs= seq(0,1,0.01))])
  # # compare wage schedules
  # POLIS[deeltijd.2 <= 1, .(mean(SBASISLOON/SREGULIEREUREN), .N), by=cut(deeltijd.2, breaks=20)][order(cut), plot(cut, V1)]
  # POLIS[, .(mean(SBASISLOON/SREGULIEREUREN), .N), by=cut(deeltijdfactor, breaks=20)][order(cut), plot(cut, V1)]
  # # all pretty similar, so conclude that deeltijdfactor is decent approximation
  # POLIS[ deeltijd.2 := NULL]
  ########
  
  # I keep RINPs at SBEIDs in jobs where deeltijdfactor >= 0.7 (roughly working >= 3.5 days per week) to ensure roughly equal time spent in firm
  # this removes around 42% of observations, check POLIS[, quantile(deeltijdfactor, probs= seq(0,1,0.01))]
  
  ## calc. deeltijdfactor (use this to select fulltime employees)
  POLIS[, deeltijdfactor := SVOLTIJDDAGEN / job.days]
  POLIS <- POLIS[deeltijdfactor >= 0.7, ] # Polis documentation says 0.95 for fulltime but I will use 0.7
  POLIS[, c("job.days", "SVOLTIJDDAGEN") := NULL] # let's keep the deeltijdfactor for robustness checks
  
  ##### subset and add gba #####
  # merge with gba, deletes all workers without gba record (should be none)
  POLIS <- merge(POLIS, gba, by="RINP")
  
  # calculate age
  POLIS[, age := year - birthyear]
  POLIS[, birthyear := NULL]
  
  ###### sanity check: How many fulltime employees do we have? #####
  #POLIS[age>=15 & age<=75, uniqueN(RINP)]  # should be around 4.5 mio in 2021
  
  # subset POLIS to workers aged 18 to 65
  POLIS <- POLIS[age >= 18 & age <= 65, ] 
  
  # save result
  saveRDS(POLIS, file=paste0(map_data_analysis, "step1/SPOLIS_yearly/POLIS_", current_year, ".rds"), 
          compress=FALSE)
  
  # store RINPs to transpose to numeric later (saves a lot of memory)
  RINPs <- unique(POLIS$RINP)
  saveRDS(RINPs, file=paste0(map_data_analysis, "step1/SPOLIS_yearly/RINPs_", current_year, ".rds"), 
          compress=FALSE)
  
  gc()
  
} 