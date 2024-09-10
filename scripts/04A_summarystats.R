##################################################################################
#### Create table of summary statistics #####
##################################################################################

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis run in data.table version 1.14.2 - consider up- or downgrading")
if (!require("xtable")) install.packages("xtable"); library("xtable") # .tex of matrices/data.tables
if (packageVersion("xtable")!="1.8.4") warning("Analysis run in xtable version 1.8.4 - consider up- or downgrading")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") 
if (packageVersion("ggplot2")!="3.4.2") warning("Analysis run in ggplot2 version 3.4.2 - consider up- or downgrading")

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
cpi <- readRDS(paste0(map_data_analysis, "step1/cpi_year.rds")) # base year is 2021


#### output dir ####
if (!dir.exists(paste0(map_output, "summary_stats/"))) dir.create(paste0(map_output, "summary_stats/"))
map_output_here <- paste0(map_output, "summary_stats/")

##################################################################################
##################################################################################
### Create summary stats (firm level) #####

#### firm size distribution ###### 
# calculate average company group size 
firm_size <- reg.table[, .(company.group.size=mean(company.group.size)), by=.(OG_BEID, company.type.1)]
#firm_size[, company.type.1.change := .N>1, by=OG_BEID]
# plot distribution by type
firm_size[, group := cut(company.group.size, breaks = c(0,20,50,100,250,1000, 5000,Inf), 
                         labels = c("0 - 19", "20 - 49", "50 - 99", "100 - 249", "250 - 999", "1000 - 4999", "> 4999"))]
firm_size[company.type.1=="INT_COMP", company.type.1:="INTERNATIONAL"]

# show size distribution
ggplot(firm_size[,], aes(x=group)) +
  geom_bar(position="stack", color="black", size = 0.5) +
  xlab("Employmen size group") + ylab("Count") + 
  theme_bw()  +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank()) 

ggsave(filename=paste0(map_output_here, "firm_size_group.pdf"), last_plot(),
       width=16, height=9, scale=0.4, dpi=300)

# show distribution of firm types
percent_format <- function(x){
  paste0(x*100, "%")
}

ggplot(firm_size[,], aes(x=group, fill=company.type.1)) +
  geom_bar(position="fill", color="black", size = 0.5) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(labels = percent_format) +
  xlab("Employmen size group") + ylab("Share") + 
  theme_bw()  +
  theme(legend.position="bottom", 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank()) 


ggsave(filename=paste0(map_output_here, "firm_size_group_dist.pdf"), last_plot(),
       width=16, height=9, scale=0.4, dpi=300)
rm(firm_size)
gc()

#### Summary stats table (firm level) #####
# extract firm panel
vars <- c("company.type.1", "company.group.size", "export", "import", "reexport")
firm_stats <- reg.table[, lapply(.SD, function(x) x[1]), by=.(OG_BEID, year), .SDcols = vars]
# add real values
firm_stats <- merge(firm_stats, cpi, by="year")
vars <- c("export", "import", "reexport")
firm_stats[, c("export", "import", "reexport") := .(export*factor, import*factor, reexport*factor)]

# average over firms
vars <- c("company.group.size", "export", "import", "reexport")
firm_stats <- firm_stats[, lapply(.SD, function(x) mean(x, na.rm=T)), by=.(OG_BEID, company.type.1), .SDcols = vars]
# per worker information
vars <- c("export", "import", "reexport")
firm_stats[, paste0(vars) := lapply(.SD, function(x) x/company.group.size), .SDcols = vars]


# display standard deviations to complete output table
firm_stats[, lapply(.SD, function(x) sd(x, na.rm=T)), by=.(company.type.1), .SDcols = vars]

# average over type and save
firm_stats <- firm_stats[, lapply(.SD, function(x) mean(x, na.rm=T)), by=.(company.type.1), .SDcols = vars]
# add other information
firm_stats <- merge(reg.table[, .(firms=uniqueN(OG_BEID), workers=uniqueN(worker.ID), obs=.N), by=company.type.1],firm_stats,
                    by="company.type.1")

rm(vars)

# add industry division
## extract firm table and add NACE labels
firm_industry <- reg.table[, .(NACE_LETTER=NACE_LETTER[1]), by=.(OG_BEID, company.type.1)]
NACE_numeric <- c(3, 6, 
                  7, 8,
                  9, 10,
                  13, 14,
                  11, 17)
NACE_text <- c("Manufacturing", "Construction", 
               "Wholesale and Retail Trade", "Transportation and Storage",
               "Accommodation and Food Service", "Information and Communication",
               "Prof., Scientific and Technical Activities", "Administrative and Support Activities",
               "Financial and Insurance Activities", "Human Health and Social Work Activities")
NACE_industries <- data.table(NACE_LETTER = NACE_numeric, NACE_label = NACE_text)
firm_industry <- merge(firm_industry, NACE_industries, by="NACE_LETTER", all.x=T)
firm_industry[is.na(NACE_label), NACE_label := "Other"]
## Calculate division over labels
NACE_text <- c(NACE_text, "Other")
firm_industry <- firm_industry[, sapply(NACE_text, function(x) uniqueN(OG_BEID[NACE_label==x])/uniqueN(OG_BEID)*100, simplify = F), by=company.type.1]
rm(NACE_text, NACE_industries, NACE_numeric)

## add to summary stats
firm_stats <- merge(firm_stats, firm_industry, by="company.type.1")

## apply rounding
vars <- names(firm_stats)[-1]
firm_stats[, paste0(vars) := lapply(.SD, function(x) round(x, 2)), .SDcols = vars]

## transpose and save
print(xtable(t(firm_stats), caption="Summary Stats (firm level)", label="tab:firm_summary_stats",
             digits=2),
     # include.rownames = TRUE,
      file = paste0(map_output, "analysis/", "firm_summary_stats.tex"))

### Mover stats by career stage (worker level) #####
# find job to job moves
reg.table[, c("OG_BEID.lead", "company.type.1.lead") := shift(.(OG_BEID, company.type.1), type="lead", n=1), by=c("worker.ID")]
reg.table[, move := is.na(OG_BEID.lead) & year!=2021] # final worker obs, if not in 2021
reg.table[move==F, move := OG_BEID != OG_BEID.lead & !is.na(OG_BEID.lead)]
reg.table[move==T & is.na(company.type.1.lead), company.type.1.lead := "Leave panel"] 
reg.table[move == T, move := company.type.1.lead != "Leave panel"] # final worker obs, if not in 2021

# career stage
reg.table[, entry_cohort := min(year), by=worker.ID]
reg.table[, labor_exp_years := year-entry_cohort]

# create early career mover matrix (<= 5 on labor market)
mover_matrix_early <- reg.table[move==T & labor_exp_years <= 5, table(company.type.1, company.type.1.lead)]
rownames(mover_matrix_early) <- c("DOMESTIC", "INTERNATIONAL", "MNE")
colnames(mover_matrix_early) <- c("DOMESTIC", "INTERNATIONAL", "MNE")
mover_matrix_early_scaled <- mover_matrix_early/rowSums(mover_matrix_early)*100
mover_matrix_early_scaled <- cbind(mover_matrix_early_scaled, rowSums(mover_matrix_early))
colnames(mover_matrix_early_scaled) <- c("DOMESTIC", "INTERNATIONAL", "MNE", "Sum movers")

# print table to .tex
print(xtable(mover_matrix_early, caption="Mover matrix", label="tab:mover_matrix_early",
             digits=0),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mover_matrix_early.tex"))

# print table to .tex
print(xtable(mover_matrix_early_scaled, caption="Mover matrix", label="tab:mover_matrix_early_scaled",
             digits=0),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mover_matrix_early_scaled.tex"))


# create mid career mover matrix (5-10 on labor market)
mover_matrix_mid <- reg.table[move==T & labor_exp_years > 5 & labor_exp_years <= 10, table(company.type.1, company.type.1.lead)]
rownames(mover_matrix_mid) <- c("DOMESTIC", "INTERNATIONAL", "MNE")
colnames(mover_matrix_mid) <- c("DOMESTIC", "INTERNATIONAL", "MNE")
mover_matrix_mid_scaled <- mover_matrix_mid/rowSums(mover_matrix_mid)*100
mover_matrix_mid_scaled <- cbind(mover_matrix_mid_scaled, rowSums(mover_matrix_mid))
colnames(mover_matrix_mid_scaled) <- c("DOMESTIC", "INTERNATIONAL", "MNE", "Sum movers")


# print table to .tex
print(xtable(mover_matrix_mid, caption="Mover matrix", label="tab:mover_matrix_mid",
             digits=0),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mover_matrix_mid.tex"))


# print table to .tex
print(xtable(mover_matrix_mid_scaled, caption="Mover matrix", label="tab:mover_matrix_mid_scaled",
             digits=0),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mover_matrix_mid_scaled.tex"))



# create later career mover matrix (>10 years on labor market)
mover_matrix_late <- reg.table[move==T & labor_exp_years > 10, table(company.type.1, company.type.1.lead)]
rownames(mover_matrix_late) <- c("DOMESTIC", "INTERNATIONAL", "MNE")
colnames(mover_matrix_late) <- c("DOMESTIC", "INTERNATIONAL", "MNE")
mover_matrix_late_scaled <- mover_matrix_late/rowSums(mover_matrix_late)*100
mover_matrix_late_scaled <- cbind(mover_matrix_late_scaled, rowSums(mover_matrix_late))
colnames(mover_matrix_late_scaled) <- c("DOMESTIC", "INTERNATIONAL", "MNE", "Sum movers")


# print table to .tex
print(xtable(mover_matrix_late, caption="Mover matrix", label="tab:mover_matrix_late",
             digits=0),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mover_matrix_late.tex"))


# print table to .tex
print(xtable(mover_matrix_late_scaled, caption="Mover matrix", label="tab:mover_matrix_late_scaled",
             digits=0),
      include.rownames = TRUE,
      file = paste0(map_output_here, "mover_matrix_late_scaled.tex"))


#### Create summary stat table (worker level) #####
# order data
setorderv(reg.table, c("worker.ID", "year", "Nemployer.year"))
#  take worker-level averages
cohort_stats <- reg.table[, .(entry=min(year), 
                              entry_age = min(age), 
                              jobs = max(job.number),
                              First_MNE = company.type.1[1]=="MNE",
                              First_INT_COMP = company.type.1[1]=="INT_COMP",
                              First_DOMESTIC = company.type.1[1]=="DOMESTIC",
                              exp_years_MNE_1 = max(exp_years_MNE_1), 
                              exp_years_INT_COMP_1 = max(exp_years_INT_COMP_1), 
                              exp_years_DOMESTIC_1 = max(exp_years_DOMESTIC_1),
                              tenure_MNE = max(c(tenure_days_OGBEID_spell[company.type.1=="MNE"], 0))/365,
                              tenure_INT_COMP = max(c(tenure_days_OGBEID_spell[company.type.1=="INT_COMP"], 0))/365,
                              tenure_DOMESTIC = max(c(tenure_days_OGBEID_spell[company.type.1=="DOMESTIC"], 0))/365,
                              wage_MNE = mean(hwage[company.type.1=="MNE"]),
                              wage_INT_COMP = mean(hwage[company.type.1=="INT_COMP"]),
                              wage_DOMESTIC = mean(hwage[company.type.1=="DOMESTIC"]),
                              wage_exp_MNE  = mean(hwage[exp_years_MNE_1 > 0]),
                              wage_exp_INT_COMP  = mean(hwage[exp_years_INT_COMP_1 > 0]),
                              wage_exp_DOMESTIC  = mean(hwage[exp_years_DOMESTIC_1 > 0])), by=worker.ID]

# Create summary statistics matrix 
summarystats <- matrix(NA, ncol=4, nrow = length(colnames(cohort_stats))-2) 
rownames(summarystats) <- colnames(cohort_stats)[c(-1, -2)]
colnames(summarystats) <- c("Overall_Mean", "Overall_SD", "2006_Mean", "2006_SD")

# add means and sds
summarystats[, 1] <- as.matrix(cohort_stats[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), .SDcols = colnames(cohort_stats)[c(-1, -2)]])
summarystats[, 2] <- as.matrix(cohort_stats[, lapply(.SD, function(x) sd(x, na.rm=TRUE)), .SDcols = colnames(cohort_stats)[c(-1, -2)]])
summarystats[, 3] <- as.matrix(cohort_stats[entry == 2006, lapply(.SD, function(x) mean(x, na.rm=TRUE)), .SDcols = colnames(cohort_stats)[c(-1, -2)]])
summarystats[, 4] <- as.matrix(cohort_stats[entry == 2006, lapply(.SD, function(x) sd(x, na.rm=TRUE)), .SDcols = colnames(cohort_stats)[c(-1, -2)]])

# print table to .tex
print(xtable(summarystats, caption="Summary statistics", label="tab:summary_stats",
             digits=2),
      include.rownames = TRUE,
      file = paste0(map_output_here, "summary_stats.tex"))