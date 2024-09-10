##################################################################################
#### Wage paths ##### 
#### This script uses the main wage regression model (w or w/o across firm interactions)
#### to draw wage path plots, for pre-specified worker paths
#### It allows for a two-way split
# Settings
across.firm.interactions <- FALSE # load main model or general path model? (see setting in 04B)

##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")
if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra") # Plotting
if (packageVersion("gridExtra")!="2.3") warning("Analysis ran in gridExtra version 2.3 - consider up- or downgrading")

#### output dirs #####
if (!dir.exists(paste0(map_output, "analysis/"))) dir.create(paste0(map_output, "analysis/"))
if (!dir.exists(paste0(map_output, "analysis/wages/"))) dir.create(paste0(map_output, "analysis/wages/"))
if (!dir.exists(paste0(map_output, "analysis/wages/wage_paths/"))) dir.create(paste0(map_output, "analysis/wages/wage_paths/"))

map_output_here <- paste0(map_output, "analysis/wages/wage_paths/") # for final outputs

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 

if (across.firm.interactions){
  # load main wage model: with interactions
  main_reg_model <- readRDS(paste0(map_data_analysis, "step4/main_model_w_across.rds"))
} else{
  # load main wage model: without interactions
  main_reg_model <- readRDS(paste0(map_data_analysis, "step4/main_model_wo_across.rds"))
}


##################################################################################
#### Functions for wage path plots ####
## function for simple wage paths (stayer, one time mover)

create.path <- function(counter_factual=FALSE, stayer=TRUE, first_type="DOMESTIC", first_years=10, second_years=NA, second_type=NA,
                        first_size = 1, second_size = 1,
                        types, 
                        breaks.tenure=c(-Inf, seq(1,10,1), Inf),
                        breaks.experience=c(-Inf, seq(0,10,1), Inf)){
  require(data.table)
  # if stayer
  if (stayer) {
    tt  <- data.table(path=paste0(first_type, "-", first_years),
                      counter_factual = counter_factual,
                      year=1:(first_years),
                      company.type.1=rep(first_type, first_years),
                      job_no_trunc = rep(1, first_years),
                      company.group.size = rep(first_size, first_years),
                      
                      tenure_years_OGBEID_spell = 1:first_years)
    # add experience years
    tt[, paste0("tenure_years_", types) := 0]
  }  else{
    tt <- data.table(path = rep(paste0(first_type, "-", first_years, "_", second_type, "-", second_years), first_years+second_years),
                     counter_factual = counter_factual,
                     year = 1:(first_years+second_years),
                     company.type.1=c(rep(first_type, first_years), rep(second_type, second_years)),
                     job_no_trunc = c(rep(1, first_years), rep(2, second_years)),
                     company.group.size = c(rep(first_size, first_years), rep(second_size, second_years)),
                     tenure_years_OGBEID_spell = c(1:first_years, 1:second_years))
    # add experience years 
    tt[, paste0("tenure_years_",types) := lapply(types, function(x) c(rep(0, first_years), rep(fifelse(first_type==x, first_years, 0), second_years)))]
  }
  # cut tenure measures into same splines as before
  tt[, tenure_group_OGBEID_spell := cut(tenure_years_OGBEID_spell, breaks=breaks.tenure, include.lowest=TRUE) ]
  tt[, paste0("exp_group_",types, "_", "1") := lapply(.SD, function(x) cut(x, breaks=breaks.experience, include.lowest=TRUE)), .SDcols=  paste0("tenure_years_",types)]
  
  # add years_since_graduation for nicer x-axis in plots
  tt[, years_since_graduation := year-1]
  
  # clean up
  tt[, paste0("tenure_years_",types) := NULL]
  
  return(tt)
}

# create a data.table containing different paths
create.table <- function(required.paths, breaks.tenure=c(-Inf, seq(1,10,1), Inf), breaks.experience=c(-Inf, seq(0,10,1), Inf), include_company_size = FALSE, company_sizes=NA){
  require("data.table")
  if (missing("required.paths")) stop("specify path data.table")
  
  # add company sizes if required --> these come in the form of another data.table with specific names
  if (include_company_size){
    required.paths[, first_size := company_sizes$company.group.size[which(company_sizes$company.type.1==first_type)], by=seq_len(nrow(required.paths))]
    required.paths[, second_size := company_sizes$company.group.size[which(company_sizes$company.type.1==second_type)], by=seq_len(nrow(required.paths))]
    if (any(required.paths[, is.na(first_size)]) | any(required.paths[stayer==FALSE, is.na(second_size)])) stop("Company sizes not correctly specified.")
    if (any(required.paths[, first_size<1]) | any(required.paths[stayer==FALSE, second_size<1])) stop("Company sizes need to be >=1")
  } else {
    required.paths[, first_size := 1]
    required.paths[, second_size := 1]
  }
  
  
  # get experience types
  types <- unique(required.paths[, c(first_type, second_type)])
  types <- types[!is.na(types)]
  # adjust time frames in required.paths
  required.paths[, second_years:= total_years-first_years]
  
  # create first row
  tt <- create.path(counter_factual=required.paths[1, counter_factual], stayer = required.paths[1, stayer],
                    first_type=required.paths[1, first_type], first_years=required.paths[1, first_years],
                    second_type = required.paths[1, second_type], second_years=required.paths[1, second_years],
                    first_size = required.paths[1, first_size], second_size = required.paths[1, second_size], 
                    types = types,
                    breaks.tenure=breaks.tenure,
                    breaks.experience=breaks.experience)
  # loop through rows of required.paths and add to tt
  if (nrow(required.paths)>1){
    for (i in 2:nrow(required.paths)){
      tt <-  rbindlist(list(tt,
                            create.path(counter_factual=required.paths[i, counter_factual], stayer = required.paths[i, stayer],
                                        first_type=required.paths[i, first_type], first_years=required.paths[i, first_years],
                                        second_type = required.paths[i, second_type], second_years=required.paths[i, second_years],
                                        first_size = required.paths[i, first_size], second_size = required.paths[i, second_size], 
                                        types = types,
                                        breaks.tenure=breaks.tenure,
                                        breaks.experience=breaks.experience)))
    }
  }
  
  return(tt)
}

# predict paths
predict.paths <- function(reg_form, tt, reg.table, coefs, OG_fes){
  # an issue is that model.matrix needs each interaction case to be fulfilled, otherwise it returns an error
  # so let's take some real data and append it to tt
  cols <- names(tt)[names(tt) %in% names(reg.table)] # take overlap in col names
  tt2 <- reg.table[samp==TRUE, eval(cols), with=FALSE]
  tt <- rbindlist(list(tt, tt2), use.names = TRUE, fill=TRUE) # impute NA for cols only in tt
  rm(tt2, cols)
  
  # create model.matrix table  from this: this works because model.matrix only creates a matrix with the cols in tt
  X <- model.matrix(reg_form, data=tt, subset=TRUE)
  # extract part of tt from model.matrix (these are the wage paths with is.na(path)==FALSE) and subset tt again
  rows <- tt[!is.na(path), which=TRUE]
  X <- X[rows,]
  tt <- tt[rows,]
  
  # make sure ordering is right and extract relevant coefficients from coefs and X: this can remove company.group.size if not in tt
  orders <- match(colnames(X), names(coefs))
  orders <- orders[!is.na(orders)]
  coefs <- coefs[orders]
  orders <- match(names(coefs), colnames(X))
  X <- X[, orders]
  
  # add predictions to tt
  tt[, pred := X%*%coefs]
  
  # if OG_BEID fe specified, add to pred
  if (exists("OG_fes")){
    # move through company.type.1s in OG_fes and add to pred
    types <- OG_fes[, unique(company.type.1)]
    for (i in types){
      tt[company.type.1==i, c("pred", "OG_BEID_fe") := .(pred+OG_fes[company.type.1==i,fe_OG_BEID_mean], OG_fes[company.type.1==i,fe_OG_BEID_mean])]
    }
  }
  
  # add exponential version of prediction
  tt[, pred.exp := exp(pred)]
  
  # in the end, I want to look at accumulated income
  
  tt[, pred.cum := cumsum(pred), by=path]
  tt[, pred.cum.exp := cumsum(pred.exp), by=path]

  # return  the predictions
  return(list(tt$pred, tt$pred.cum, tt$pred.exp, tt$pred.cum.exp))
}
##################################################################################
##################################################################################
#### Prepare reg.table ####
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

##################################################################################
#### Required wage paths ####
required_paths_hourly <- data.table(total_years = rep(10, 5),
                                    counter_factual=c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                    stayer = c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                    first_type=c("DOMESTIC", "MNE", "MNE", "MNE", "MNE"),
                                    first_years=c(10, 10, 8, 6, 4),
                                    second_type =c(NA, NA, "DOMESTIC", "DOMESTIC", "DOMESTIC"))
##################################################################################
#### Initializing #####
##### create new fixest object ####
###### draw sample, keep dummy for model.matrix creation ######
set.seed(490)
reg.table[, samp := (worker.ID %in% sample(worker.ID, 0.5*10^5))]

###### create fixest object ######

# create object --> can just leave out the fes here. With subset=TRUE in model.matrix, can ignore the intercept here
reg_form <- feols(fml=as.formula(main_reg_model$fml),
                  data=reg.table[samp==TRUE,],
                  vcov="iid")


##################################################################################
#### Loop over creating paths with and without company size included #######

##### Company size #####
for (include.company.size in c(TRUE, FALSE)){
  ##### Calculate company sizes #####
  if (include.company.size){
    # first per company.type.1
    company_sizes <- main_reg_model$firm.fes.table[company.type.1.change==FALSE, .(company.group.size=mean(company.group.size)), by=company.type.1]
    # add total average
    company_sizes <- rbindlist(list(company_sizes,
                                    main_reg_model$firm.fes.table[company.type.1.change==FALSE, .(company.type.1="OUTSIDE_COMPANY", company.group.size=mean(company.group.size))]))
  }
  
  ##### extract relevant coefficients ######
  # get all coefficients
  coefs <-  main_reg_model$coefs
  ## extract static wage premia
  static <-  grep("company.type.1::", names(coefs))
  ## extract tenure and exp groups
  main <- grep("(tenure_group|exp_group)", names(coefs))
  ## extract relevant controls
  controls <- grep("job_no_trunc", names(coefs))
  controls <- c(controls, ifelse(include.company.size, grep("company.group.size", names(coefs)), numeric(0))) 
  ### subset coefs
  coefs <-  coefs[unique(c(static, main, controls))]
  
  ##################################################################################
  ##### create data.table for wage path prediction #####
  # create tables
  tt_hourly <- create.table(required_paths_hourly, breaks.tenure=c(-Inf, seq(1,10,1), Inf), breaks.experience=c(-Inf, seq(0,10,1), Inf), include_company_size = include.company.size, company_sizes=company_sizes)
  
  #### predict paths (using average OG_BEID fe & potentially average company sizes) ######
  tt_hourly[, c("pred", "pred.cum",  "pred.exp", "pred.cum.exp") := predict.paths(reg_form, tt_hourly, reg.table, coefs, OG_fes=main_reg_model$firm.fes.means)]
  
  #####################################################
  ####  Plots of paths #####
  
  #### change path names for plots #####
  tt_hourly[, path := gsub("-", " ", path)]
  tt_hourly[, path := gsub("_", " & ", path)]
  
  #### plot paths: log hourly wage ####
  accepted_linetypes <- c("dashed", "dotted", "dotdash", "longdash", "twodash")
  gg_hourly <- ggplot(tt_hourly[counter_factual==FALSE, c("years_since_graduation", "pred",  "path")], aes(years_since_graduation, pred, group=path)) +
    geom_line(data=tt_hourly[counter_factual==TRUE, c("years_since_graduation", "pred",  "path")], aes(years_since_graduation, pred, color=path),
              size=1) +
    geom_line(size=1, aes(linetype=path)) +
    scale_color_grey(end = 0, start = 0.65) +
    scale_linetype_manual(values = accepted_linetypes) +
    scale_x_continuous(name="Years of experience", breaks=0:tt_hourly[,max(years_since_graduation)], expand = c(0.01, 0.01)) +
    ylab("Residual log hourly wage") +
    theme_linedraw()  +
    theme(legend.position=c(0.1,0.8), 
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          panel.background = element_blank(), panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
    labs(linetype=guide_legend(title="Movers (years)", order = 2), color=guide_legend(title="Stayers (years)", order = 1))

  ggsave(filename=paste0(map_output_here, "plot_path_hourly_", ifelse(include.company.size, "size_", "no_size_"), fifelse(across.firm.interactions, "w_", "wo_"), "across", ".pdf"), 
         gg_hourly,
         width=16*0.7, height=9*0.7)
}

