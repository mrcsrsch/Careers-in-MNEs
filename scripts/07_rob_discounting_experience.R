##################################################################################
#### Robustness check: Discouting experience #### 
# This script calculates the optimal discount factor of across-firm MNE experience 
# and also performs an encompassing test 

##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")

#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_data_analysis, "step4/"))) dir.create(paste0(map_data_analysis, "step4/")) # to store regression object for further analysis

if (!dir.exists(paste0(map_output, "robustness/"))) dir.create(paste0(map_output, "robutness/"))
if (!dir.exists(paste0(map_output, "robustness/discounting/"))) dir.create(paste0(map_output, "robutness/discounting/"))

map_output_here <- paste0(map_output, "robustness/discounting/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 

##################################################################################
##################################################################################
#### prepare reg.table #####
setorderv(reg.table, cols=c("worker.ID", "year", "Nemployer.year"))

# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

# add columns for current year experience per company.type
types <- reg.table[, unique(company.type.1)]
reg.table[, paste0("exp_",types,"_year" ) := lapply(types, function(x) fifelse(company.type.1==x, tenure_days_OGBEID_year/365, 0))]

# find entry positions in OG_BEID, after first job
reg.table[, obs := 1:.N, by=worker.ID]
reg.table[, entry := obs==min(obs) , by=c("worker.ID", "OG_BEID", "spell.id")] # find entry positions
reg.table[obs==1, entry := FALSE] # adjust for first observation per worker


##################################################################################
#### Parameters of analysis ###### 
gc() # collect garbage and free up memory

sample.use <- FALSE
sample.size <- 0.1*10^6

##### Use full sample or subsample with worker size sample.use? ####
if (sample.use){
  # use a  random sample of workers for quicker calculations
  set.seed(7854)
  reg.table[, subsample := (worker.ID %in% sample(worker.ID, sample.size))]
} else{
  # full sample
  reg.table[, subsample := TRUE]
}

##### Set formula parts ####
# save types for formula
types <- reg.table[, unique(company.type.1)]
## empirical model formula parts

# without discounting
within_firm <- paste0(" + i(tenure_group_OGBEID_spell, ref=\"[-Inf,1]\")")
within_firm_interact <- paste0(" + i(company.type.1, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\")")

# without discounting 
within_firm_job <- paste0(" + i(tenure_group_OGBEID_spell, i.job_no_trunc, ref=\"[-Inf,1]\", ref2=\"1\")")

# across firm experience returns, with discounting
across_firm <- paste0(" + i(exp_group_", types, "_", "1", ", ref = \"[-Inf,0]\") : discount_", types, "_across - discount_", types, "_across", collapse = "")

# controls
controls <- "+ log(company.group.size) + i(job_no_trunc, ref=\"1\")"

##################################################################################
#### Find optimal discounting rate ######
# discounts to consider
deltas <- c(seq(0.8,0.95,0.05), seq(0.96, 1, 0.01))

# matrix of results
delta.mat <- matrix(NA, nrow=length(deltas), ncol=4)
colnames(delta.mat) <- c("delta", "R2", "R2within", "RMSE")

##### loop through deltas and store R2s  ######
timer <- Sys.time()
timer

#for (i in 1:length(deltas)){
for (i in 1:nrow(delta.mat)){
  
  # message 
  cat(paste0(Sys.time(), ": starting iteration ", i, "/", length(deltas)), "\n")

  
  ##################################################################################
  ###### Calculate within and across discount factor #####
  # reorder reg.table
  setorderv(reg.table, cols=c("worker.ID", "year", "Nemployer.year"))
  
  
  # set new delta
  delta_across <- deltas[i]

  # delete old columns if they exist
  if (any(c("discount_DOMESTIC_across", "discount_MNE_across", "discount_INT_COMP_across") %in% names(reg.table))){
    reg.table[, c("discount_DOMESTIC_across", "discount_MNE_across", "discount_INT_COMP_across") := NULL]
  }
  
  
  # Experience only accumulates across firms. Discounting happens within firms as well. Tenure is discounted within firms .
  # across firm: calculate discounted experience years per type, only for entry positions
  
  ### Discount factor across companies: discount_`type`_across 
  #### calculated as weighted average of discounts:  ( delta^0*days_0 + delta^1 * days_1 + delta^2 * days_2 ) / (sum_i days_i)
  #### first calculate for entry positions into firms:
  types <- reg.table[, unique(company.type.1)]
  reg.table[subsample==TRUE, 
            paste0("discount_", types,"_across") := lapply(types, function(x) {
              exps <- eval(as.symbol(paste0("exp_", x, "_year")))
              res <- rep(0, .N) # result vector: set to zero for all positions
              all_entry_obs <- obs[entry]  # extract entry positions
              # loop through entry positions and calculate mean discount value
              for (entry_obs in all_entry_obs){
                res[entry_obs] <- sum(exps[obs<entry_obs]*(delta_across^(year[entry_obs]-year[obs<entry_obs])))
                # if res[entry_obs] is different from zero, divide by sum(exps[obs<entry_obs])
                if (res[entry_obs]!=0) res[entry_obs] <- res[entry_obs]/sum(exps[obs<entry_obs])
              }
              res
            }), 
            by=worker.ID]
  #### within firm: within each worker.ID, OG_BEID, spell.id match discount initial discounted experience per year in the firm
  reg.table[subsample==TRUE, paste0("discount_", types,"_across") := lapply(types, function(x) eval(as.symbol(paste0("discount_", x,"_across")))[1]*delta_across^(year-year[1])), by=c("worker.ID", "OG_BEID", "spell.id")]
  
  ###### Run regression #####
  
  reg_current <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                             within_firm,
                                             within_firm_interact,
                                             within_firm_job,
                                             across_firm, 
                                             controls,
                                             "",
                                             "| ",
                                             # fixed effects
                                             "worker.ID ",
                                             "+ OG_BEID",
                                             ""
  )),
  vcov="iid",
  nthreads=7,
  data=reg.table[subsample==TRUE])
  
  ###### Store metrics ######
  delta.mat[i, 1] <- delta_across
  delta.mat[i, 2] <- r2(reg_current, type="r2") # R2
  delta.mat[i, 3] <- r2(reg_current, type="wr2") # R2 within
  delta.mat[i, 4] <- as.numeric(fitstat(reg_current, type="rmse"))

  cat(paste0("delta = ", delta.mat[i,1], " with RMSE = ", delta.mat[i, 4]), "\n")
  
  rm(reg_current)
}
timer <- Sys.time()-timer
timer


##################################################################################
#### Optimal discounting value #####

##### plots ######
ggplot(data=as.data.table(delta.mat), aes(x=delta)) + 
  geom_point(aes(y=RMSE), color="black") + 
  scale_x_continuous(breaks=deltas) +
  xlab("Discount factor") + ylab("Root Mean Squared Error") +
  theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_rect(colour="black", size=1.2),
                     plot.margin = unit(c(t = 1.27, r = 0.82, b = 0.25, l = 0.82), "inches"))
ggsave(filename = paste0(map_output_here, "RMSE_graph.pdf"), dpi=300,  width=20.29354/2.5, height=13.41437/2.5)


ggplot(data=as.data.table(delta.mat), aes(x=delta)) + 
  geom_point(aes(y=R2within), color="blue") + 
  scale_x_continuous(breaks=seq(min(delta.mat[,1]), max(delta.mat[,1]), 0.02)) +
  xlab("Discount factor") + ylab(bquote(R^2-within)) +
  theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(filename = paste0(map_output_here, "R2_within_graph.pdf"), dpi=300,  width=2.10*3, height=2.97*3)

###### save optimal delta #####
delta <- delta.mat[which.min(delta.mat[,4]), 1]

saveRDS(list(delta.mat, delta, timer), file=paste0(map_output_here, "delta.rds"))

##################################################################################
#### Rerun main wage regression analysis with optimal discounting value #####
##################################################################################

##### new delta #####
delta <- delta.mat[which.min(delta.mat[,4]), 1]

##### recalculate discounts #####
# first delete old ones
reg.table[, c("discount_DOMESTIC_across", "discount_MNE_across", "discount_INT_COMP_across") := NULL]


### Discount factor across companies: discount_`type`_across 
#### calculated as weighted average of discounts:  ( delta^0*days_0 + delta^1 * days_1 + delta^2 * days_2 ) / (sum_i days_i)
#### first calculate for entry positions into firms:
types <- reg.table[, unique(company.type.1)]
reg.table[subsample==TRUE, 
          paste0("discount_", types,"_across") := lapply(types, function(x) {
            exps <- eval(as.symbol(paste0("exp_", x, "_year")))
            res <- rep(0, .N) # result vector: set to zero for all positions
            all_entry_obs <- obs[entry]  # extract entry positions
            # loop through entry positions and calculate mean discount value
            for (entry_obs in all_entry_obs){
              res[entry_obs] <- sum(exps[obs<entry_obs]*(delta^(year[entry_obs]-year[obs<entry_obs])))
              # if res[entry_obs] is different from zero, divide by sum(exps[obs<entry_obs])
              if (res[entry_obs]!=0) res[entry_obs] <- res[entry_obs]/sum(exps[obs<entry_obs])
            }
            res
          }), 
          by=worker.ID]
#### within firm: within each worker.ID, OG_BEID, spell.id match discount initial discounted experience per year in the firm
reg.table[subsample==TRUE, paste0("discount_", types,"_across") := lapply(types, function(x) eval(as.symbol(paste0("discount_", x,"_across")))[1]*delta^(year-year[1])), by=c("worker.ID", "OG_BEID", "spell.id")]

##### Run regression ######
reg_main <- feols(fml=as.formula(paste0("lhwage_detrended ~ ",
                                        within_firm,
                                        within_firm_interact,
                                        within_firm_job,
                                        across_firm, 
                                        controls,
                                        "",
                                        "| ",
                                        # fixed effects
                                        "worker.ID ",
                                        "+ OG_BEID",
                                        ""
)),
cluster="worker.ID",
ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
data=reg.table[subsample==TRUE])

etable(reg_main, #lapply(paste0("regs[[",length(regs):1, "]]"), function(x) eval(parse(text=x))), 
       file=paste0(map_output_here, "reg_disounted_model_", ifelse(across.firm.interactions, "w_", "wo_"), "across", ".tex"),  
       title="Wage profiles",
       label="tab:main_wage", 
       depvar = TRUE,
       digits = "r4",
       fitstat = c("n", "r2"),
       digits.stats = "r4",
       fixef_sizes = TRUE,
       fixef_sizes.simplify = TRUE,
       powerBelow = -6,
       float = TRUE,
       dict = c(company.type.1 = "", MNE = "MNE", INT_COMP = "International firm", tenure_group_OGBEID_spell = "Within company tenure",
                exp_group_MNE_1 = "MNE experience", exp_group_INT_COMP_1 = "International firm experience", exp_group_DOMESTIC_1 = "Domestic company experience",
                `log(company.group.size.2)` = "log(company size)", job_no_trunc = "Employer number"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (worker level) in parantheses."),
       replace = TRUE)

##### Run graphical analysis ####

# adjust covariate names
names(reg_main$coefficients) <- gsub(":discount.*$", "", names(reg_main$coefficients))
names(reg_main$coefficients) <- gsub("^discount.*:c", "c", names(reg_main$coefficients))
names(reg_main$se) <- gsub(":discount.*$", "", names(reg_main$se))
names(reg_main$se) <- gsub("^discount.*:c", "c", names(reg_main$se))
###### A) Mover wage growth #####
pdf(file=paste0(map_output_here, "coefplot_across_firm_wo_across_discounted.pdf"), width=16*0.7, height=9*0.7)
estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
company.type.plot(estimates, type="Total", legend.add = TRUE, 
                  legend.names = c("MNE", "Int. firm"), 
                  only.legend = FALSE, title = "", graycolors = TRUE)
mtext(paste0("Discount factor = ", delta), side=3, adj=0)
dev.off()


rm(estimates)

###### B) Stayer wage growth ###### 

pdf(file=paste0(map_output_here, "coefplot_stayer_wages_wo_across_discounted.pdf"), width=16*0.7, height=9*0.7)
estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                  legend.names= c("MNE", "Int. firm"), graycolors=TRUE)
dev.off()

rm(estimates)