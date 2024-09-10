##################################################################################
### Calculate consumer price index deflator #####
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")
if (!require("cbsodataR")) install.packages("cbsodataR"); library("cbsodataR")

### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))
map_output_here <- paste0(map_data_analysis, "step1/")

##################################################################################
### load CPI index #####
cpi <- setDT(get_data('83131ned'))
cpi <- cpi[substr(Perioden,1,4) %in% seq(2000,2022,1) & substr(Perioden, 6, 6)=="" &
             Bestedingscategorieen == '000000 Alle bestedingen', 
           .(Perioden, CPIAfgeleid_2)]
cpi[, CPIAfgeleid_2 := as.numeric(CPIAfgeleid_2)]

# define base year and factors 
base_factor <- cpi[Perioden=="2021", CPIAfgeleid_2]
cpi[, factor := base_factor/CPIAfgeleid_2]
cpi[, CPIAfgeleid_2:=NULL]
rm(base_factor)

# some formatting
cpi[, year:= as.integer(paste0(Perioden))]
cpi[, Perioden := NULL]

# save
saveRDS(cpi, file=paste0(map_output_here, "cpi_year.rds"))     
