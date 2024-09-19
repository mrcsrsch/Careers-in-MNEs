##################################################################################
# prepare GBA/NIETGBA
# output: RINP, birthyear, gender
##################################################################################

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")

#### output dir #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))
##################################################################################
##################################################################################
##### read in data and prepare #####
gba <- fread(paste0(map_data_source,"GBA_NIETGBA/GBAPERSOON2021.csv"), sep=";",
             select = c(RINPERSOONS = "character", RINPERSOON = "character", GBAGEBOORTEJAAR = "integer", GBAGESLACHT = "integer"))
gba[, RINP := paste0(RINPERSOONS, RINPERSOON)]
setnames(gba, 3, "birthyear")
gba[, female := (GBAGESLACHT == 2)]
gba[, c("RINPERSOONS", "RINPERSOON", "GBAGESLACHT") := NULL]

nietgba <- fread(paste0(map_data_source,"GBA_NIETGBA/NIET_GBAPERSOON.csv"), sep=";", 
                 select = c(RINPERSOONS = "character", RINPERSOON = "character", NIETGBAGEBJAAR = "integer", NIETGBAGESLACHT = "integer"))
nietgba[, RINP := paste0(RINPERSOONS, RINPERSOON)]
nietgba[, birthyear := as.integer(NIETGBAGEBJAAR)]
nietgba[, female := (NIETGBAGESLACHT == 2)]
nietgba[, c("RINPERSOONS", "RINPERSOON", "NIETGBAGEBJAAR", "NIETGBAGESLACHT") := NULL]
## remove rows with missing values 
nietgba <- nietgba[complete.cases(nietgba), ]

#### merge to one file #### 
gba <- merge(gba, nietgba, by="RINP", all.x = TRUE, all.y = TRUE)
rm(nietgba)
# remove duplicates, keep information from gba
gba[!is.na(birthyear.x), birthyear := birthyear.x]
gba[is.na(birthyear.x) & !is.na(birthyear.y), birthyear := birthyear.y]

gba[!is.na(female.x), female := female.x]
gba[is.na(female.x) & !is.na(female.y), female := female.y]

gba[, c("birthyear.x", "female.x", "birthyear.y", "female.y") := NULL]

#### save output #### 
saveRDS(gba, paste0(map_data_analysis, "step1/gba.rds"), compress=TRUE)
