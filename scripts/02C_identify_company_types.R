##################################################################################
##### This script identifies discrete firm classes at the OG_BEID level #####

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis run in data.table version 1.14.2 - consider up- or downgrading")

#### output dir #####
if (!dir.exists(paste0(map_data_analysis, "step2/"))) dir.create(paste0(map_data_analysis, "step2/"))

#### load data  ####
OG_SBEIDs <- readRDS(paste0(map_data_analysis, "step2/OGBEID_SBEID_translation.rds"))
MNEs <- readRDS(paste0(map_data_analysis, "step1/company/MNEs.rds"))
exportsimports <- readRDS(paste0(map_data_analysis, "step1/company/exporters.rds"))
nace <- readRDS(paste0(map_data_analysis, "step1/company/nace.rds"))

##################################################################################
##################################################################################

#### Add international firm metrics and NACE to OG_SBEIDs ####
OG_SBEIDs <- merge(OG_SBEIDs, MNEs, by=c("SBEID", "year"), all.x=TRUE)
OG_SBEIDs <- merge(OG_SBEIDs, exportsimports, by=c("SBEID", "year"), all.x=TRUE)
OG_SBEIDs <- merge(OG_SBEIDs, nace, by=c("SBEID", "year"), all.x=TRUE)

rm(MNEs, exportsimports, nace)

setorderv(OG_SBEIDs, cols=c("OG_BEID", "year", "SBEID"))

#### prepare vars for firm.type identification
## replace NAs
OG_SBEIDs[is.na(NED_MUL) & is.na(BUI_MUL), c("NED_MUL", "BUI_MUL") := .(FALSE, FALSE)]
OG_SBEIDs[, c("export", "reexport", "import") := lapply(.SD, nafill, fill=0), .SDcols=c("export", "reexport", "import")]

#### Identify main NACE letter of OG_BEID #####
# add 1_digit NACE (by reference)
### Reference table 
NACEcodes <-      c(paste0("0", 1:3, "=A", "=1"),
                    paste0("0", 5:9, "=B", "=2"),
                    paste0(10:33, "=C", "=3"),
                    "35=D=4",
                    paste0(36:39, "=E", "=5"),
                    paste0(41:43, "=F", "=6"),
                    paste0(45:47, "=G", "=7"),
                    paste0(49:53, "=H", "=8"),
                    paste0(55:56, "=I", "=9"),
                    paste0(58:63, "=J", "=10"),
                    paste0(64:66, "=K", "=11"),
                    "68=L=12",
                    paste0(69:75, "=M", "=13"),
                    paste0(77:82, "=N", "=14"),
                    "84=O=15",
                    "85=P=16",
                    paste0(86:88, "=Q", "=17"),
                    paste0(90:93, "=R", "=18"),
                    paste0(94:96, "=S", "=19"),
                    paste0(97:98, "=T", "=20"),
                    "99=U=21")
NACEcodes <- tstrsplit(NACEcodes, split="=")
NACEcodes[[3]] <- as.numeric(NACEcodes[[3]])

# go through OG_SBEIDs and add OG_SBEIDs[[3]] as NACE_LETTER
OG_SBEIDs[, nace_2digit := substr(nace_sbi08, 1, 2)]
OG_SBEIDs[!is.na(nace_2digit), NACE_LETTER := NACEcodes[[3]][which(NACEcodes[[1]]==nace_2digit[1])], by=nace_2digit]
#OG_SBEIDs[, nace_2digit := NULL]
# for all firms with missing nace_sbi08, assign NACE_LETTER === 999
OG_SBEIDs[is.na(NACE_LETTER), NACE_LETTER := 999] # missing NACE codes 
rm(NACEcodes)

##################################################################################
#### Aggregate to OG_BEID level #####
setorderv(OG_SBEIDs, cols=c("OG_BEID", "year", "SBEID"))

# pick main NACE LETTER per OG_BEID year
# as most common nace LETTER, same for 2digit nace
mode.calc <- function(x){
  uniques <- unique(x)
  return(uniques[which.max(tabulate(match(x,uniques)))])
}


OGs <- OG_SBEIDs[, .(NED_MUL=as.logical(max(NED_MUL)),
                     BUI_MUL=as.logical(max(BUI_MUL)),
                     export=sum(export),
                     import=sum(import),
                     reexport=sum(reexport),
                     NACE_LETTER = mode.calc(NACE_LETTER),
                     NACE_2digit = mode.calc(nace_2digit),
                     imputed.OG_BEID = as.logical(max(imputed.OG_BEID))), by=c("OG_BEID", "year")]
# where BUI_MUL and NED_MUL are both TRUE, set NED_MUL to false (in my definition BUI_MUL dominates, see company data script)
OGs[BUI_MUL==TRUE & NED_MUL==TRUE, NED_MUL:=FALSE]
rm(OG_SBEIDs)

##################################################################################
#### control for gaps in MNE status ####
# fix MNE vectors for gaps, if one year gap between two TRUE, impute TRUE
impute_MNE <- function(bui_mul){
  # check for changes in bui_mul vector
  gap <- diff(bui_mul, lag = 1)
  
  # can stop here if no changes
  if (all(gap==0)) return(bui_mul)
  
  # set MNE == TRUE if year in between two MNE == TRUE observations is FALSE
  gap <- which(diff(gap, lag = 1)==2)+1
  
  # return adjusted bui_mul vector and identifier
  bui_mul[gap] <- TRUE
  replaced <- rep(FALSE, length(bui_mul))
  replaced[gap] <- TRUE
  
  return(list(bui_mul, replaced))
}
setorderv(OGs, cols=c("OG_BEID", "year"))
OGs[, c("BUI_MUL", "replaced.bui") := impute_MNE(BUI_MUL), by=OG_BEID]
OGs[, c("NED_MUL", "replaced.ned") := impute_MNE(NED_MUL), by=OG_BEID]

# where overwritten, change other type
OGs[replaced.bui==TRUE, NED_MUL := FALSE]
OGs[replaced.ned==TRUE, BUI_MUL := FALSE]
OGs[, c("replaced.bui", "replaced.ned") := NULL]

# fix imputed OG_BEIDs --> imputed MNEs often lie before or after an observed OG_BEID link (see POLIS script)
# impute MNE status if MNE in next or previous year for these
OGs[, c("BUI_MUL.lag", "BUI_MUL.lead", "NED_MUL.lag", "NED_MUL.lead") := shift(.(BUI_MUL, NED_MUL), n=c(1,-1), type="lag", fill=FALSE), by=OG_BEID]
OGs[imputed.OG_BEID==TRUE, BUI_MUL := fifelse(NED_MUL==FALSE & (BUI_MUL.lead==TRUE | BUI_MUL.lag==TRUE), TRUE, BUI_MUL)]
OGs[imputed.OG_BEID==TRUE, NED_MUL := fifelse(BUI_MUL==FALSE & (NED_MUL.lead==TRUE | NED_MUL.lag==TRUE), TRUE, NED_MUL)]
OGs[, c("BUI_MUL.lag", "BUI_MUL.lead", "NED_MUL.lag", "NED_MUL.lead") := NULL]


##################################################################################
#### Identify company group types #####
##### 1. identify MNEs #####

# logic is: firm.type captures mutually exclusive firm class, following a top-down approach
OGs[, company.type := fifelse(BUI_MUL | NED_MUL, "MNE", "UNASSIGNED")]
# remove single observation MNEs --> these are uninteresting (as a one year change likely has no impact on experience) or falsely identified
OGs[, change := sum(company.type=="MNE")==1, by=OG_BEID]
OGs[change==TRUE, company.type:="UNASSIGNED"]
OGs[, change := NULL]

##### 2.  Other international companies (Exporter, Importer, Reexporter) #####

# remaining firms are either domestic or intl. firm across whole panel, depending on PERMANENT cutoff
## draw company-level data --> mean total import+export
OGs_aggregated <- OGs[, .(mean_value = mean((import+export)), export_years = sum((import+export)>0), MNE_years = sum(company.type=="MNE"), total_years = .N), by=OG_BEID]

# to  do: remove loop when creating final  script (only keep accepted definition)
cutoff <- c(1*10^4) # let's consider different cutoffs for permanent assignments (except MNE)
k <- 1
OGs_aggregated[, paste0("company.type.", k) := fifelse(mean_value>cutoff, "INT_COMP", "DOMESTIC")]
# merge back to OGs and identify company types
tt <- OGs_aggregated[, .(OG_BEID, get(paste0("company.type.", k)))]
setnames(tt, 2, paste0("company.type.", k))
OGs <- merge(OGs, tt, by=c("OG_BEID"))
# identify types in OGs
OGs[, paste0("company.type.", k) := fifelse(company.type=="MNE", "MNE", get(paste0("company.type.", k)))]
# clean up
OGs_aggregated[, paste0("company.type.", k) := NULL]


# identify "permanent" MNEs 
OGs_aggregated[, permanent_MNE := (MNE_years/total_years >= 12/16)]
OGs <- merge(OGs, OGs_aggregated[, c("OG_BEID", "permanent_MNE")], by=c("OG_BEID"))
OGs[, company.type.perm := fifelse(permanent_MNE, "MNE", company.type.1) , by=OG_BEID]
OGs[permanent_MNE==FALSE, company.type.perm :=  company.type.1[which(company.type.1!="MNE")[1]], by=OG_BEID]

##### Identify switchers (from MNE) ######
OGs[, company.type.change := uniqueN(company.type)>1, by=OG_BEID]

###### clean up ######
OGs[, c("NED_MUL", "BUI_MUL") := NULL]

##################################################################################
#### save result and clean up ####
saveRDS(OGs, file=paste0(map_data_analysis, "step2/OG_company_types.rds"), compress = TRUE)
