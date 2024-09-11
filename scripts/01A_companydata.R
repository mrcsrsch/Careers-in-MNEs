##################################################################################
### SQL company data #####
# This script loads the company data from the KIO SQL servers and performs basic manipulations
# If you do not have access to this SQL server, I recommend that you skip the SQL calls and load the respective datasets from disc.
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (!require("RODBC")) install.packages("RODBC"); library("RODBC")

#### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))
if (!dir.exists(paste0(map_data_analysis, "step1/company/"))) dir.create(paste0(map_data_analysis, "step1/company/"))

##### set SQL connection ###### 
server_id <- NA # specify server ID here
database_id <- NA # specify database ID here 

if (is.na(server_id) | is.na(database_id)) warning("You need to specify the server and database ids for this script to run. Alternatively, you could remove the SQL calls and load the respective datasets from disc.")

# open SQL connection
dbhandle <- odbcDriverConnect(paste0("driver={SQL Server};server=", server_id, ";database=", database_id, ";trusted_connection = yes"))

##################################################################################
##################################################################################
##### kenmerken ondernemingen (nace code) ###### 
# retrieve NACE (SBI 08) and size class of firm IDs 

query <- paste0("
                SELECT p.Vbe_Identificatie, p.jaar, p.kwartaal, p.gk_strat, sbi08pub_kw
				FROM pab15_jaar p
                ")

beidkenmerken <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 

beidkenmerken <- beidkenmerken[nchar(Vbe_Identificatie) == 8]

setnames(beidkenmerken, 1:5, c("SBEID", "year", "quarter", "size_class", "sbi08pub_kw"))

# keep latest industry classfication per SBEID, year
setorderv(beidkenmerken, cols=c("SBEID", "year", "quarter"))
beidkenmerken <- beidkenmerken[year>=2006 & year <= 2021, .(nace_sbi08 = sbi08pub_kw[.N]), by=c("SBEID", "year")]

saveRDS(beidkenmerken, file=paste0(map_data_analysis, "step1/company/nace.rds"))

rm(beidkenmerken)
##################################################################################
##### company groups BEIDs (Note: this is sometimes also called OND_ID instead of OG_ID) ######
# retrieve company groups of firm IDs

query <- paste0(
  "SELECT [JAAR] as year
  ,[BEID] as SBEID
  ,[OGID] as OG_BEID
  FROM [dbo].[LBRU_KoppelingBeOgNormaal]
  WHERE JAAR >= 2006 AND JAAR <= 2021")

beog <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))
saveRDS(beog, file=paste0(map_data_analysis, "step1/company/beog.rds"))
# need this one below again
#rm(beog)
##################################################################################
#####  Foreign & Dutch MNEs ##### 
# identify Dutch and foreign MNEs

# Foreign MNEs can easily be identified for 2006-2020 via UCI (better for business economy) --> impute 2021 based on 2020
# Dutch MNEs: 2020/2021 is currently based on 2019 identification ("heel voorlopig"); 2006-2009 missing --> impute below


###### Foreign MNEs #####
# Identify these based on UCI, impute for 2021 (missing year)

# retrieve UCI source (2006-2020)
query <- paste0("
                SELECT *
                FROM [dbo].[UCI]
                ")

uci <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 
uci <- uci[, .(SBEID=as.integer(BEID), OG_BEID=as.integer(OGID), year=as.integer(JAAR), UCI=UCI)]

# remove missing UCIs 
uci <- uci[!is.na(UCI), ]

# Identify foreign MNEs: I define these as firm that contain a UCI other than NL (e.g. "NL/UK" is a foreign MNE)
# uci[, .N, by=UCI][order(UCI), UCI] --> non-foreign-MNEs are all firms with UCI %in% c("NL", "nl", "failliet", "uitgeschre") -- last 2 not in UCI list on DSC-web
foreign <- uci[!UCI%in% c("NL", "nl", "failliet", "uitgeschre"), ]
rm(uci)
foreign[, c("UCI", "BUI_MUL") := .(NULL, TRUE)]

# add 2021: all firms with foreign in 2019 and 2020 are also foreign in 2021. --> note: in a later script I aggregate to the OG_BEID level and take the max of BUI_MUL for all firms
foreign2021 <- foreign[year %in% c(2019,2020), .(OG_BEID=OG_BEID, year=2021, check=.N==2, BUI_MUL=TRUE), by=SBEID][check==TRUE, !c("check")]
foreign <- rbindlist(list(foreign, foreign2021))

#  remove OG_BEID (I add this in a later script)
foreign[, c("OG_BEID") := NULL]

###### Domestic MNEs ######
# Impute values for 2006-2009. Already imputed for 2020-2021 in MNEs data set. 

# get list of MNEs from KIO teams database (2010-2021)
query <- paste0("
                SELECT *
                FROM [Data].[Multinationals]
                ")
MNEs <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 
MNEs <- MNEs[, .(SBEID = as.integer(BEID), year=as.integer(jaar), BUI_MUL=as.logical(BUI_MUL), NED_MUL=as.logical(as.integer(NED_MUL)))]

# select Dutch MNEs
dutch <- MNEs[NED_MUL==TRUE, !c("BUI_MUL")]
rm(MNEs)

## Impute years 2006-2009
# For large firms (in SFGO), we can use the SFGO (2006 - 2009)
SFGO <- list()
k <- 0
for (year in 2006:2009){
  k <- k+1
  query <- paste0("
                SELECT 
                [OND_ID] as OG_BEID 
                , [JAAR] as year
                , [Bula_Dochters] as [Bula_Dochters]
                FROM [IOH_DATA_ANA].[Data].[SFGO_",year,"]
                ")
  SFGO[[k]] <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 
}
SFGO <- rbindlist(SFGO)
# add SBEIDs to this and remove OG_BEID (I add this in a later script)
SFGO <- merge(SFGO, beog[year>=2006 & year < 2010], by=c("OG_BEID","year"), all.x=TRUE) 
SFGO <- SFGO[!is.na(SBEID), !c("OG_BEID")]
# identify potential Dutch MNEs (later will overwrite this with BUI_MUL)
SFGO[, NED_MUL := Bula_Dochters=="J"]
SFGO[, Bula_Dochters := NULL]
SFGO_dutch <- SFGO[NED_MUL==TRUE, ]

# for smaller firms need to impute --> Dutch MNE in 2006-2009 if Dutch MNE in 2010-2012
## find firms that are not in SFGO 2006-2009
dutch[year %in% 2010:2012, SFGO := any(SBEID %in% SFGO$SBEID), by=SBEID]
rm(SFGO)
## for firms with SFGO==FALSE, impute NED_MUL
dutch20062009 <- dutch[year %in% 2010:2012 & SFGO==FALSE, .(year=2006:2009, NED_MUL=(.N==3)), by=SBEID][NED_MUL==TRUE,]

### add everything to Dutch
dutch <- rbindlist(list(dutch[, !c("SFGO")], dutch20062009, SFGO_dutch), use.names=TRUE)
rm(SFGO_dutch, dutch20062009)

###### Combine and set BUI_MUL as dominant ######
MNEs <- rbindlist(list(dutch, foreign), fill=TRUE)
MNEs[is.na(NED_MUL), NED_MUL := FALSE]
MNEs[is.na(BUI_MUL), BUI_MUL := FALSE]

MNEs <- MNEs[, .(NED_MUL = as.logical(max(NED_MUL)), BUI_MUL = as.logical(max(BUI_MUL))), by=c("SBEID", "year")]
MNEs[BUI_MUL==TRUE, NED_MUL := FALSE]

setorderv(MNEs, cols=c("SBEID",  "year"))

# save result 
saveRDS(MNEs, file=paste0(map_data_analysis, "step1/company/MNEs.rds"))

##################################################################################
#### Exporters / Importers ########
# get IHG to country data set

query <- paste0("
                select * from [dbo].[IHG_NaarLand]
                ")

ihg <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))

setDT(ihg)

# change types
ihg[, c("SBEID", "year", "export", "reexport", "import") := .(as.integer(BEID),
                                        as.integer(JAAR),
                                        as.numeric(EXPORT),
                                        as.numeric(WEDERUITVOER),
                                        as.numeric(IMPORT))]
# keep subset of vars and data
ihg <- ihg[!is.na(SBEID), c("SBEID", "year", "export", "reexport", "import")]

# set missing values to 0
ihg[, c("export", "reexport", "import") := lapply(.SD, nafill, fill=0), .SDcols=c("export", "reexport", "import")]

# aggregate per SBEID-year
ihg[, c("export", "reexport", "import") := lapply(.SD, sum), by=c("SBEID", "year"), .SDcols=c("export", "reexport", "import")]
# remove duplicates 
ihg <- unique(ihg)

# we're missing entries for 2006, so we will just impute values from 2007
ihg <- rbindlist(list(ihg, ihg[year==2007, .(SBEID, year=2006, export, reexport, import)]))

# save result
saveRDS(ihg, file=paste0(map_data_analysis, "step1/company/exporters.rds"))