##################################################################################
##### Read in and prepare graduates data ####
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step2/"))) dir.create(paste0(map_data_analysis, "step2/"))

#### load data #######
# load RINP translation table
RINPs <- readRDS(paste0(map_data_analysis, "step1/RINP_workerID_translation.rds"))
##################################################################################
##################################################################################

#### MBO data #######
# in DIPLOMAMBOTAB

# read in data
DIPLOMAMBO <- list()
k <- 0
for (current_year in 2004:2020){
  cat(paste0(Sys.time(), ": year ", current_year), "\n")
  k <-  k+1
  DIPLOMAMBO[[k]] <- fread(paste0(map_data_source, "ONDERWIJS/DIPLOMAMBOTAB/DIPLOMAMBO", current_year, "1299ANAV1.csv"))
}
DIPLOMAMBO <- rbindlist(DIPLOMAMBO)


# basic manipulations
DIPLOMAMBO[, RINP:= paste0(RINPERSOONS, RINPERSOON)]
DIPLOMAMBO[, c("RINPERSOONS", "RINPERSOON") := NULL]

# adjust type of some cols and find years
# Note that "jaar" is the schoolyear jaar/jaar+1, so use grad.date to find actual calendar year
DIPLOMAMBO[, grad.date := as.Date(paste0(EXAMENDATUMMBO), format = "%Y%m%d")]
DIPLOMAMBO[, year := year(grad.date)]
DIPLOMAMBO[, c("EXAMENDATUMMBO", "jaar") := NULL]

DIPLOMAMBO[, c("SELECTIEHOOFDDPLMBO",  "OPLNR", 
               "SELECTIEHOOGSTEDPLMBO",
               "TYPEMBODPL",
               "year") := .(as.integer(SELECTIEHOOFDDPLMBO), 
                            as.integer(OPLNR), 
                            as.integer(SELECTIEHOOGSTEDPLMBO),
                            as.integer(TYPEMBODPL ),
                            as.integer(year))]

# Variables:
# SELECTIEHOOFDDPLMBO : identifies hoofd (1)  and even diplomas (2) within year
# SELECTIEHOOGSTEDPLMBO : identified hoogstbehaalde (1) and even diplomas (2) within year
# TYPEMBODPL: type of MBO education, I will only keep == 1 here (berops opleidende leerweg, voltijd - these are fulltime students)

# keep only relevant degrees (full time MBO)
DIPLOMAMBO <- DIPLOMAMBO[TYPEMBODPL==1, !c("TYPEMBODPL")]

# other vars are now irrelevant as I should only have fulltime students up to graduation
DIPLOMAMBO[, c("SELECTIEHOOFDDPLMBO",
               "SELECTIEHOOGSTEDPLMBO") := NULL]

# add degree var
DIPLOMAMBO[, degree.level := 0]

###### change RINP to integer #####
# merge, remove all unobserved RINPs (they are not in the POLIS)
DIPLOMAMBO <- merge(DIPLOMAMBO, RINPs, by="RINP")  
DIPLOMAMBO[, RINP := NULL]

##################################################################################
#### WO / HBO  Data #####
##################################################################################
##### Code lists ######

# NOTE: 
# Via het opleidingsnummer kunnen met behulp van een koppelbaar bestand (OPLEIDINGSNRREFVV) allerlei gegevens die gerelateerd zijn aan het opleidingsnummer achterhaald worden.
# Note that some diplomas have missing months 

# BRIN = ONDERWEIJSINSTELLING
# OPLNR = Opleidingsnummer
# SOORTDIPLSOORTHO = Niveau van het in het hoger onderwijs behaalde diploma 
# Codes:  see below
# STUDIEFASEDIPL: see below for Codes
# VORMOPLEIDINGHODIP = vorm van de opleiding (- onbekend, 1 voltijd, 2 deeltijd, 3 duaal onderwijs)
# KOPPELDIPLOMAHOJJJJTABVV

### STUDIEFASEDIPL (observed Codes)
# A	associate degree
# B	bachelor
# D	propedeuse bachelor --- REMOVE
# I	initi?le opleiding (voornamelijk betreft dit ongedeelde opleidingen) ---- Note: These are studies witout Bachelor/Master. REMOVE.
# M	master
# P	propedeuse (voornamelijk betreft dit ongedeelde opleidingen) ---- REMOVE
# Z	beroepsfase artsen-, tandartsen-, dierenartsen- en apothekersopleidingen ---- REMOVE

### SOORTDIPLSOORTHO (observed Codes)
# 03	Hoofd-bachelor-diploma
# 04	Neven-bachelor-diploma
# 05	Hoofd-master-diploma
# 06	Neven-master-diploma
# 07	Hoofd-doctoraal-diploma ---- Not observed after STUDIEFASEDIPL clean
# 08	Neven-doctoraal-diploma ---- Not observed after STUDIEFASEDIPL clean
# 09	Hoofddiploma beroepsfase/postmaster ---- all of these have STUDIEFASEDIPL==M, random check yields this as only master degree
# 10	Nevendiploma beroepsfase/postmaster ---- all of these have STUDIEFASEDIPL==M, random check yields this as only master degree
##################################################################################


# in DIPLOMAHOTAB

#### read in data #####
DIPLOMAHO <- list()
k <- 0
for (current_year in 2004:2020){
  cat(paste0(Sys.time(), ": year ", current_year), "\n")
  k <-  k+1
  DIPLOMAHO[[k]] <- fread(paste0(map_data_source, "ONDERWIJS/DIPLOMAHOTAB/DIPLOMAHO", current_year, "1299ANAV1.csv"))
}
DIPLOMAHO <- rbindlist(DIPLOMAHO)

# basic manipulations
setnames(DIPLOMAHO, c("jaar", "EXAMENMAAND"), c("year", "month"))
DIPLOMAHO[, RINP:= paste0(RINPERSOONS, RINPERSOON)]
DIPLOMAHO[, c("RINPERSOONS", "RINPERSOON") := NULL]
DIPLOMAHO[, c("EXAMENDATUMDIPLHO", "HOBRINVESTDIPLHO", "SOORTDIPLHO") := NULL ] # don't observe these for every year (only >= 2013)
DIPLOMAHO[, c("KOPPELDIPLOMAHOJJJJTABVV") := NULL] # all entries are = 1

# adjust type of some cols
DIPLOMAHO[, c("month",  "OPLNR", 
              "SOORTDIPLSOORTHO",
              "VORMOPLEIDINGHODIP",
              "year") := .(as.integer(month), 
                           as.integer(OPLNR), 
                           as.integer(SOORTDIPLSOORTHO),
                           as.integer(VORMOPLEIDINGHODIP),
                           as.integer(year))]
DIPLOMAHO[, inexact := is.na(month)]
DIPLOMAHO[is.na(month), month := 9] # set to beginning of academic year

# add date
# unfortunatelty EXAMENDATUMDIPLHO not observed, so we have to construct it as the year column is the start year of an academic year
DIPLOMAHO[, year := fifelse(month>=9, year, year-1)]
DIPLOMAHO[, grad.date := as.Date(paste0(year, "-", month, "-", "1"), format = "%Y-%m-%d")]

# Could not find information on SOORTDIPLSOORTHO > 10 (unique gives 11, 12 but those are not in the code list)
# These are very few diplomas relative to the total (0.6%). So, I just remove them here. Should be special degrees that are not very relevant.
DIPLOMAHO <- DIPLOMAHO[SOORTDIPLSOORTHO<=10,]

# only keep relevant degrees (remove propedeuse etc.)
DIPLOMAHO <- DIPLOMAHO[STUDIEFASEDIPL %in% c("A", "B", "M"), ]

# turn studiephase to numeric and rename to degree.level
DIPLOMAHO[, degree.level := fifelse(STUDIEFASEDIPL=="A", 1, fifelse(STUDIEFASEDIPL=="B", 2, 3))]
DIPLOMAHO[, degree.level := as.integer(degree.level)]
DIPLOMAHO[, STUDIEFASEDIPL := NULL]

# only consider full-time degrees (otherwise higher chance of earlier relevant work experience in firm)
DIPLOMAHO <- DIPLOMAHO[VORMOPLEIDINGHODIP==1, ]

# remove unnecessary vars (might use them for later project though)
DIPLOMAHO[, c("month", "SOORTDIPLSOORTHO", "VORMOPLEIDINGHODIP") := NULL]


#### change RINP to integer #####
# merge, remove all unobserved RINPs (they are not in the POLIS)
DIPLOMAHO <- merge(DIPLOMAHO, RINPs, by="RINP")  
DIPLOMAHO[, RINP := NULL]
rm(RINPs)

##################################################################################
#### Find highest achieved degree + graduation year #####
# append DIPLOMAHO and DIPLOMAMBO
DIPLOMAMBO[, inexact := FALSE]
DIPLOMA <- rbindlist(list(DIPLOMAMBO, DIPLOMAHO), use.names=TRUE)
rm(DIPLOMAMBO,DIPLOMAHO)

# identify highest degree
DIPLOMA[, degree.highest := max(degree.level), by=worker.ID]

# add identifier whether worker has > 1 of highest degree
DIPLOMA[, degree.highest.multiple := sum(degree.highest==degree.level)>1, by=worker.ID]

# keep only latest degree (using years first is considerably faster)
DIPLOMA[, keep := year==max(year), by=worker.ID]
DIPLOMA <- DIPLOMA[keep==TRUE, !c("keep")]
# for workers >1 obs identify latest graduation date
DIPLOMA[worker.ID %in% DIPLOMA[, .N, by=worker.ID][N>1, worker.ID], keep := grad.date==max(grad.date), by=worker.ID]
DIPLOMA[is.na(keep), keep := TRUE]
DIPLOMA <- DIPLOMA[keep==TRUE, !c("keep")]
# remove inexact dates here (removes workers whose latest degree had month missing - 56)
DIPLOMA <- DIPLOMA[inexact==FALSE, !c("inexact")]
# keep one line per RINP and drop unused cols
# Note: without adjusting the above, it makes no sense to keep these, as the latest degree may not be the highest degree (or both occur in same year), and there may be multiple highest degrees
DIPLOMA[, c("BRIN", "OPLNR", "degree.level") := NULL]

# remove duplicated (grad on same date)
DIPLOMA  <- DIPLOMA[!duplicated(DIPLOMA), ]
DIPLOMA[, .N, by=worker.ID][, max(N)]==1 # quick check

# setnames
setnames(DIPLOMA, "year", "degree.year")
setnames(DIPLOMA, "grad.date", "degree.date")
##################################################################################
#### save result ##### 

# save results
saveRDS(DIPLOMA, file=paste0(map_data_analysis, "step2/DIPLOMA.rds"), compress = TRUE)


