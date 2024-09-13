##################################################################################
### Robustness: using k-means clustering #####
# This script applies k-means clustering to the firm fixed effects used in the dynamic and static model
# It also runs a bootstrap to calculate standard erros for a t-test
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table") # Data manipulations
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # Fixed effect estimations
if (packageVersion("fixest")!="0.10.4") warning("Analysis ran in fixest version 0.10.4 - consider up- or downgrading")

if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # Plotting
if (packageVersion("ggplot2")!="3.3.5") warning("Analysis ran in ggplot2 version 3.3.5 - consider up- or downgrading")

if (!require("igraph")) install.packages("igraph"); library("igraph") # calculate Jochmans measure
if (packageVersion("igraph")!="1.4.2") warning("Analysis ran in igraph version 1.4.2 - consider up- or downgrading")

if (!require("xtable")) install.packages("xtable"); library("xtable") # .tex of matrices/data.tables
if (packageVersion("xtable")!="1.8.4") warning("Analysis run in xtable version 1.8.4 - consider up- or downgrading")


#### sourcing scripts #####
source(paste0(map_scripts, "00A_analysis_functions.R"), echo=T) 

#### output dirs #####
if (!dir.exists(paste0(map_output, "robustness/"))) dir.create(paste0(map_output, "robustness/"))
if (!dir.exists(paste0(map_output, "robustness/clustered_fe/"))) dir.create(paste0(map_output, "robustness/clustered_fe/"))
if (!dir.exists(paste0(map_output, "robustness/clustered_fe/static/"))) dir.create(paste0(map_output, "robustness/clustered_fe/static/"))
if (!dir.exists(paste0(map_output, "robustness/clustered_fe/dynamic_wo_size/"))) dir.create(paste0(map_output, "robustness/clustered_fe/dynamic_wo_size/"))
if (!dir.exists(paste0(map_output, "robustness/clustered_fe/dynamic/"))) dir.create(paste0(map_output, "robustness/clustered_fe/dynamic/"))

map_output_here_static <- paste0(map_output, "robustness/clustered_fe/static/") # for final outputs
map_output_here_dynamic_wo_size <- paste0(map_output, "robustness/clustered_fe/dynamic_wo_size/") # for final outputs
map_output_here_dynamic <- paste0(map_output, "robustness/clustered_fe/dynamic/") # for final outputs
map_output_here <- c(map_output_here_static, map_output_here_dynamic_wo_size, map_output_here_dynamic)

if (!dir.exists(paste0(map_data_analysis, "step7/"))) dir.create(paste0(map_data_analysis, "step7/"))
map_data_output_here <- paste0(map_data_analysis, "step7/")

#### load data  ####
reg.table <- readRDS(paste0(map_data_analysis, "step3/POLIS_graduates_connected.rds")) 
##################################################################################
##################################################################################
#### Settings k-means  + bootstrap #####
boots <- 101 #(bootstraps + 1 )  
k_firms <- c(10,20,50) # how many clusters?
set.seed(445646) # set a seed

#### Prepare data ######
# truncate job number 
reg.table[, job_no_trunc := fifelse(job.number>3, 4, job.number)]

##################################################################################
#### Calculate Jochmans (first) ######
# create vector to store Jochmans measure
jwconnect <- rep(0, length(k_firms)+1)
names(jwconnect) <- c("full", paste0("k=", k_firms))

# extract edge list
edgelist <- reg.table[, c("worker.ID", "OG_BEID"), with=FALSE] 
setnames(edgelist, c("V1", "V2"))
edgelist[, V1 := V1+max(V2)+1] # create non-overlapping IDs

## create a graph (not bipartite yet)
connect <- graph.data.frame(edgelist, directed=FALSE)
## turn into bipartite graph
cat(is.bipartite(connect), "\n")
V(connect)$type <- V(connect)$name %in% edgelist[, unique(V2)] # True are firm IDs 
cat(is.bipartite(connect), "\n")
## clean up 
rm(edgelist)
gc()

# project bipartite graph (firms as edges, number of movers between group-years as weights in E(connect)$weights)
cat(paste0(Sys.time(), " projecting graph (workers on firms)"), "\n")
connect <- bipartite_projection(connect, multiplicity = TRUE, which="true", remove.type=TRUE)
gc()

# find components in connect_new
comps <- components(connect)
if (comps$no!=1) warning("More than one connected set in graph") # should be 1
rm(comps)

# calculate connectivity #
# message
cat(paste0(Sys.time(), " calculating Jochmans measure"), "\n")

# this is the smallest non-zero eigenvalue of the laplacian matrix of the network
eigenvals <- embed_laplacian_matrix(connect, no=2, which="sa", type="I-DAD", scaled = TRUE, options=list(tol=1E-3, maxiter=100000)) #, options=list(tol=1E-2), maxiter=10000

# print eigenval and save in vec
cat(paste("Connectivity = ", eigenvals$D[2]), "\n")
jwconnect[1] <- eigenvals$D[2]
rm(connect, eigenvals)


##################################################################################
##### Set formula parts ####
static <- "lhwage_detrended ~ 0" # static model has no controls 
dynamic_wo_size <- "lhwage_detrended ~  + i(tenure_group_OGBEID_spell, ref=\"[-Inf,1]\") + i(company.type.1, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\") + i(tenure_group_OGBEID_spell, i.job_no_trunc, ref=\"[-Inf,1]\", ref2=\"1\") + i(exp_group_MNE_1, ref = \"[-Inf,0]\") + i(exp_group_INT_COMP_1, ref = \"[-Inf,0]\") + i(exp_group_DOMESTIC_1, ref = \"[-Inf,0]\")+ i(job_no_trunc, ref=\"1\")"
dynamic <- "lhwage_detrended ~  + i(tenure_group_OGBEID_spell, ref=\"[-Inf,1]\") + i(company.type.1, tenure_group_OGBEID_spell, ref=\"DOMESTIC\", ref2=\"[-Inf,1]\") + i(tenure_group_OGBEID_spell, i.job_no_trunc, ref=\"[-Inf,1]\", ref2=\"1\") + i(exp_group_MNE_1, ref = \"[-Inf,0]\") + i(exp_group_INT_COMP_1, ref = \"[-Inf,0]\") + i(exp_group_DOMESTIC_1, ref = \"[-Inf,0]\")+ log(company.group.size) + i(job_no_trunc, ref=\"1\")"

forms <- c(static=static, dynamic_wo_size=dynamic_wo_size, dynamic=dynamic)

##################################################################################
#### Kmeans clutersting: extract empirical wage distirbution ######

# create outputs 
## store matrices of difference estimates in nested list structure 
grouped_fe_diff <- vector(mode='list', length=length(forms))
names(grouped_fe_diff) <- names(forms)
### create matrices to store estimates 
for (p in 1:length(forms)){
  grouped_fe_diff[[p]] <- vector(mode='list', length=length(k_firms))
  names(grouped_fe_diff[[p]]) <- paste0(k_firms)
  for (k in 1:length(k_firms)){
    grouped_fe_diff[[p]][[k]]$weighted_prem$MNE <- matrix(c(rep(1, boots), rep(NA, boots)), nrow=boots, ncol=2)
    colnames(grouped_fe_diff[[p]][[k]]$weighted_prem$MNE) <- c("MNE", "grouped_fe")
    grouped_fe_diff[[p]][[k]]$weighted_prem$DOM <- matrix(c(rep(1, boots), rep(NA, boots)), nrow=boots, ncol=2)
    colnames(grouped_fe_diff[[p]][[k]]$weighted_prem$DOM) <- c("MNE", "grouped_fe")
    grouped_fe_diff[[p]][[k]]$unweighted_prem$MNE <- matrix(c(rep(1, boots), rep(NA, boots)), nrow=boots, ncol=2)
    colnames(grouped_fe_diff[[p]][[k]]$unweighted_prem$MNE) <- c("MNE", "grouped_fe")
    grouped_fe_diff[[p]][[k]]$unweighted_prem$DOM <- matrix(c(rep(1, boots), rep(NA, boots)), nrow=boots, ncol=2)
    colnames(grouped_fe_diff[[p]][[k]]$unweighted_prem$DOM) <- c("MNE", "grouped_fe")
  }
}
rm(p,k)


#### Loop over bootstrap iterations (first is no bootstrap) ######


timer <- Sys.time()
timer

# save worker IDs and set bootstrap sample size
workerids <- reg.table[, unique(worker.ID)]
bsize <- length(workerids)


for (boot in 1:boots){
  # message
  cat(paste(Sys.time(), "boot =", boot, "/", boots), "\n")
  
  ##### Create boot strap sample (first no bootstrap) ##### 
  if (any(names(reg.table)=="bootsample")) reg.table[, bootsample := NULL]
  if (boot == 1){
    reg.table[, bootsample := TRUE]
    boot.table <- reg.table
  } else { # block-bootstrap
    #reg.table[, bootsample := worker.ID %in% sample(workerids, size = bsize, replace = F)]
    sids <- sample(workerids, size = bsize, replace = TRUE)
    boot.table <- reg.table[worker.ID %in% unique(sids),]
    ## logic is to repeatedly add reg.table to itself until all occurrences of worker.ID in sids are fulfilled
    sids <- sids[duplicated(sids)]
    while (sum(duplicated(sids)) > 0){
      boot.table <- rbindlist(list(boot.table, reg.table[worker.ID %in% unique(sids),]))
      sids <- sids[duplicated(sids)]
    }
    ## create boot var 
    boot.table[, bootsample := TRUE]
  }
  
  
  ##### extract empirical wage distribution of firms ######
  
  
  #boot.table[bootsample == T, paste0("firm_", seq(0.05, 0.95, 0.05)) := as.list(quantile(lhwage_detrended, probs = seq(0.05, 0.95, 0.05))), by=c("OG_BEID")]
  # non-weighted K-means (considerably faster)
  ## by within-firm wage distribution
  firms <- boot.table[bootsample == T, as.list(quantile(lhwage_detrended, probs = seq(0, 1, 0.1))), by=c("OG_BEID")] #seq(0.05, 0.95, 0.05))
  # by within-firm wage growth of stayers. Note that 39019 out of 8055023 obs drop out because firm has no stayers
  #firms <- boot.table[bootsample == T & stayer==T, as.list(quantile(lhwage_detrended_growth, probs = seq(0, 1, 0.1))), by=c("OG_BEID")]
  
  ##### find groups of firms and workers using (observation weighted) kmeans  ##### 
  k <- 0
  for (k_firm in k_firms){
    k <- k+1
    # message
    cat(paste(Sys.time(), "k = ", k_firm), "\n")
    
    # Apply non-weighted K-means (considerably faster than weighted k-means)
    clusters <- kmeans(firms[, !c("OG_BEID")], centers = k_firm, iter.max=1000, algorithm = "Lloyd", nstart=20)
    ## add firm grouping to boot.table
    if ("grouped_firm" %in% names(boot.table)){
      boot.table[, grouped_firm := NULL] # clean up
    } 
    if ("grouped_firm" %in% names(firms)){
      firms[, grouped_firm := NULL] # clean up
    }
    
    #boot.table[bootsample == T, "grouped_firm" := clusters$cluster]
    # non-weighted K-means (considerably faster)
    firms[, "grouped_firm" := clusters$cluster]
    boot.table <- merge(boot.table, firms[,c("OG_BEID", "grouped_firm")], by="OG_BEID", all.x=T) # need to keep all observations here
    
    
    ###### Calculate weights for wage premia ###### 
    ## calculate weights per group (employment_weighted)
    firm_fes_1 <- boot.table[bootsample == T, sapply(c("MNE", "DOMESTIC"), function(x) sum(company.type.1==x), simplify = F, USE.NAMES = T), by="grouped_firm"]
    firm_fes_1[, c("MNE", "DOMESTIC") := .(MNE/sum(MNE), DOMESTIC/sum(DOMESTIC))]
    # calculate weighs per group (firm weighted) (more similar to distribution comparisons in paper) What is the average wage premium of MNEs vs domestic firms (not employment weighted)?
    firm_fes_2 <- boot.table[bootsample == T, sapply(c("MNE", "DOMESTIC"), function(x) uniqueN(OG_BEID[company.type.1==x]), simplify = F, USE.NAMES = T), by="grouped_firm"]
    firm_fes_2[, c("MNE", "DOMESTIC") := .(MNE/sum(MNE), DOMESTIC/sum(DOMESTIC))]
    
    ##### Calculate Jochmans for grouped firms ###### 
    if (boot == 1){
      # extract edge list
      edgelist <- boot.table[, c("worker.ID", "grouped_firm"), with=FALSE] 
      setnames(edgelist, c("V1", "V2"))
      edgelist[, V1 := V1+max(V2)+1] # create non-overlapping IDs
      
      ## create a graph (not bipartite yet)
      connect <- graph.data.frame(edgelist, directed=FALSE)
      ## turn into bipartite graph
      cat(is.bipartite(connect), "\n")
      V(connect)$type <- V(connect)$name %in% edgelist[, unique(V2)] # True are firm IDs 
      cat(is.bipartite(connect), "\n")
      ## clean up 
      rm(edgelist)
      gc()
      
      # project bipartite graph (firms as edges, number of movers between group-years as weights in E(connect)$weights)
      cat(paste0(Sys.time(), " projecting graph (workers on firms)"), "\n")
      connect <- bipartite_projection(connect, multiplicity = TRUE, which="true", remove.type=TRUE)
      gc()
      
      # find components in connect_new
      comps <- components(connect)
      if (comps$no!=1) warning("More than one connected set in graph") # should be 1
      rm(comps)
      
      # calculate connectivity #
      # message
      cat(paste0(Sys.time(), " calculating Jochmans measure"), "\n")
      
      # this is the smallest non-zero eigenvalue of the laplacian matrix of the network
      eigenvals <- embed_laplacian_matrix(connect, no=2, which="sa", type="I-DAD", scaled = TRUE, options=list(tol=1E-3, maxiter=100000)) #, options=list(tol=1E-2), maxiter=10000
      
      # print eigenval and save in vec
      cat(paste("Connectivity = ", eigenvals$D[2]), "\n")
      jwconnect[k+1] <- eigenvals$D[2]
      rm(connect, eigenvals)
    }
    
    
    ##### Run regressions with clustered firm fixed effects: static, dynamic w/o size, dynamic ##### 
    t <- 0
    for (form in forms){
      t <- t+1
      # message
      cat(paste(Sys.time(), names(forms)[t]), "\n")
      
      # calculate current model 
      if (boot == 1){
        reg_main <- feols(fml=as.formula(paste0(form,
                                                "| ",
                                                # fixed effects
                                                "worker.ID ",
                                                "+ grouped_firm",
                                                ""
        )),
        cluster="worker.ID",
        ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"),
        nthreads=7,
        subset = boot.table$bootsample,
        data=boot.table)
        
        ##### Create outputs #####
        etable(reg_main, 
               file=paste0(map_output_here[t], "reg_wages_grouped_", names(forms)[t], "_k", k_firm, ".tex"),  
               title="Wage profiles",
               label= paste0("tab:rob_grouped_", names(forms)[t]), 
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
        
        # save output plots only for dynamic model
        if (names(forms)[t]=="dynamic"){
          # mover wage growth
          pdf(file=paste0(map_output_here[t], "coefplot_across_grouped_", names(forms)[t], "_k", k_firm, ".pdf"), width=16*0.7, height=9*0.7)
          estimates <- total.general.prem(reg_main, exp.types = c("MNE", "INT_COMP", "DOMESTIC"), k=1)
          company.type.plot(estimates, type="Total", legend.add = TRUE, 
                            legend.names = c("MNE", "Int. firm"), 
                            only.legend = FALSE, title = "", graycolors = TRUE)
          dev.off()
          
          # stayer wage growth 
          pdf(file=paste0(map_output_here[t], "coefplot_within_grouped_", names(forms)[t], "_k", k_firm, ".pdf"), width=16*0.7, height=9*0.7)
          estimates <- stayer.wages.estimates(reg_main, company.types=c("MNE", "INT_COMP", "DOMESTIC"),  k=1)
          stayer.wages.plot(estimates, legend.add=TRUE, highlight.MNE=TRUE, title="", 
                            legend.names= c("MNE", "Int. firm"), graycolors=TRUE)
          dev.off()
          
          rm(estimates)
        }
        
      } else {
        reg_main <- feols(fml=as.formula(paste0(form,
                                                "| ",
                                                # fixed effects
                                                "worker.ID ",
                                                "+ grouped_firm",
                                                ""
        )),
        vcov="iid",
        nthreads=7,
        subset = boot.table$bootsample,
        data=boot.table)
      }
      
      #### Retrieve fixed effect estimates and calculate MNE premium #####
      # retrieve fe estimates relative to some fixed effect 
      fes <- fixef(reg_main, sorted=TRUE, nthreads=7, fixef.tol = .Machine$double.eps*10000) #fixef.iter doesn't seem to actually work (hard coded in source?)
      
      # to deal with changing reference groups per bootstrap, let's look at fixed effects in deviation of their mean
      firm_fes <- data.table(grouped_firm = as.integer(names(fes$grouped_firm)), firm_fe = fes$grouped_firm)
      firm_fes[, firm_fe := firm_fe - mean(firm_fe)]
      
      # What is the average wage premium experienced by an MNE/Domestic workers? 
      # employment weighted (Setzler and Tintelnot 2021)
      firm_fes_1 <- merge(firm_fes_1, firm_fes,
                          by = "grouped_firm")
      grouped_fe_diff[[t]][[k]]$weighted_prem$MNE[boot,2] <- firm_fes_1[, sum(firm_fe*MNE)]
      grouped_fe_diff[[t]][[k]]$weighted_prem$DOM[boot,2] <- firm_fes_1[, sum(firm_fe*DOMESTIC)]
      firm_fes_1[, firm_fe := NULL]
      
      # What is the average wage premium experienced MNEs/Domestics? 
      # firm weighted 
      firm_fes_2 <- merge(firm_fes_2, firm_fes,
                          by = "grouped_firm")
      grouped_fe_diff[[t]][[k]]$unweighted_prem$MNE[boot,2] <- firm_fes_2[, sum(firm_fe*MNE)]
      grouped_fe_diff[[t]][[k]]$unweighted_prem$DOM[boot,2] <- firm_fes_2[, sum(firm_fe*DOMESTIC)]
      firm_fes_2[, firm_fe := NULL]
      
      # clean up
      rm(fes, reg_main, firm_fes)
    }
    rm(firm_fes_1, firm_fes_2)
    
    # Print out average distance
    if (boot == 1){
      # Static premium
      cat(paste("K=", k_firm,  "Static premium is (weighted):", grouped_fe_diff[[1]][[k]]$weighted_prem$MNE[1,2]-grouped_fe_diff[[1]][[k]]$weighted_prem$DOM[1,2]), "\n")
      cat(paste("K=", k_firm, "Static premium is (unweighted):", grouped_fe_diff[[1]][[k]]$unweighted_prem$MNE[1,2]-grouped_fe_diff[[1]][[k]]$unweighted_prem$DOM[1,2]), "\n")
      # Dynamic w/o size
      cat(paste("K=", k_firm, "Dynamic w/o size premium is (weighted):", grouped_fe_diff[[2]][[k]]$weighted_prem$MNE[1,2]-grouped_fe_diff[[2]][[k]]$weighted_prem$DOM[1,2]), "\n")
      cat(paste("K=", k_firm, "Dynamic w/o size premium is (unweighted):", grouped_fe_diff[[2]][[k]]$unweighted_prem$MNE[1,2]-grouped_fe_diff[[2]][[k]]$unweighted_prem$DOM[1,2]), "\n")
      # Dynamic 
      cat(paste("K=", k_firm, "Dynamic premium is (weighted):", grouped_fe_diff[[3]][[k]]$weighted_prem$MNE[1,2]-grouped_fe_diff[[3]][[k]]$weighted_prem$DOM[1,2]), "\n")
      cat(paste("K=", k_firm, "Dynamic premium is (unweighted):", grouped_fe_diff[[3]][[k]]$unweighted_prem$MNE[1,2]-grouped_fe_diff[[3]][[k]]$unweighted_prem$DOM[1,2]), "\n")
    }
    
  }
  rm(firms)
}
rm(clusters)
rm(boot.table)

# Save result 
saveRDS(grouped_fe_diff, file = paste0(map_data_output_here, "grouped_fes.rds"), compress=T) # Estimated firm-type premia + bootstrap
saveRDS(jwconnect, file = paste0(map_data_output_here, "jwconnect.rds"), compress=T) # vector of Jochmans measures

timer <- Sys.time()-timer
timer  # ca. 17 hours

##################################################################################
##### Extract a data.table ####
# create table of all estimates of MNE_fe and DOM_fe per bootstrap iteration, K, model
groupfes <- data.table()
for (a in 1:length(grouped_fe_diff)){
  for (k in 1:length(grouped_fe_diff[[1]]))
    groupfes <- rbindlist(list(groupfes, 
                         data.table(K = names(grouped_fe_diff[[a]])[k],
                                    Jochmans = round(jwconnect[k+1],4),
                                    specification = names(grouped_fe_diff)[a],
                                    boot = 1:boots,
                                    MNE_fe = grouped_fe_diff[[a]][[k]]$weighted_prem$MNE[,2],
                                    DOM_fe = grouped_fe_diff[[a]][[k]]$weighted_prem$DOM[,2])))
}


##### add non-grouped estimates ####
###### add main estimates ###### 
# Note these were already estimated before 
# idea is to create data.table with estimates from non-grouped regression (same as data.table above)

# load estimates ("main", "fullwosize", "basic")
overall <- readRDS(paste0(map_data_analysis, "step4/companyfes_full.rds"))
# get employment weights by OG_BEID and type
## add overall N per company.type.1 to reg.table and calculate employment weights for overall averages
reg.table[, N_overall := .N, by=company.type.1]
weights <- reg.table[company.type.1 %in% c("MNE", "DOMESTIC"), .(weight=.N/N_overall[1]), by=c("OG_BEID", "company.type.1")]

# add weights to overall, keep for later
overall <- merge(overall, weights, by=c("OG_BEID", "company.type.1"))

# calculate overall differences for ("main", "fullwosize", "basic")
# and create allfes data.table

allfes <- data.table(K="max", Jochmans = jwconnect[1], 
                     specification="full", boot=1, 
                     MNE_fe = overall[company.type.1=="MNE", sum(main*weight)], 
                     DOM_fe = overall[company.type.1=="DOMESTIC", sum(main*weight)])
allfes <- rbindlist(list(allfes, 
                         data.table(K="max", Jochmans = jwconnect[1], 
                                    specification="fullwosize", boot=1, 
                                    MNE_fe = overall[company.type.1=="MNE", sum(fullwosize*weight)], 
                                    DOM_fe = overall[company.type.1=="DOMESTIC", sum(fullwosize*weight)])))
allfes <- rbindlist(list(allfes, 
                         data.table(K="max", Jochmans = jwconnect[1], 
                                    specification="basic", boot=1, 
                                    MNE_fe = overall[company.type.1=="MNE", sum(basic*weight)], 
                                    DOM_fe = overall[company.type.1=="DOMESTIC", sum(basic*weight)])))

###### add bootstrap estimates ###### 
# Note: A bootstrap was already conducted for this. Just need to prepare the data.
# idea is: 
# 1. loop through models and bootstraps of models
# 2. collect MNE and DOM premium by employment weighing in a matrix
# 3. add to allfes data.table
models <- c("full", "fullwosize", "basic")

for (model in models){
  cat(paste(Sys.time(), model), "\n")
  # create empty matrix to store bootstrap results
  mat <- matrix(NA, nrow=100, ncol=2)
  
  # add progress bar
  pb <- txtProgressBar(min = 1, max = 100, initial = 1)
  for (boot in 1:100){ 
    # update progress bar
    setTxtProgressBar(pb, boot)
    
    # load bootstrap data set
    current <- readRDS(paste0(map_data_analysis, "step5/bootstrap_wo_workerfe/", boot, "_", model, ".rds"))
    
    # subset to company fes
    current <- current$companyfes 
    current[, reference := NULL]
    # deviation of mean
    current[, fe_OG_BEID  := fe_OG_BEID - mean(fe_OG_BEID)]
    
    # add weights
    current <- merge(current, weights, by="OG_BEID")
    
    # calculate average MNE and DOM premium
    current <- current[, sum(fe_OG_BEID*weight), by=company.type.1]
    
    # add to matrix 
    mat[boot, 1] <- current[company.type.1=="MNE", V1]
    mat[boot, 2] <- current[company.type.1=="DOMESTIC", V1]
    
  }
  rm(boot, current)
  # close progress bar
  close(pb)
  
  # add to data.table 
  allfes <- rbindlist(list(allfes, 
                           data.table(K="max", Jochmans = jwconnect[1], specification=model, boot=2:101, MNE_fe = mat[,1], DOM_fe = mat[,2])))
}

###### Manipulate resulting table ###### 
allfes[specification=="full", specification := "dynamic"]
allfes[specification=="fullwosize", specification := "dynamic_wo_size"]
allfes[specification=="basic", specification := "static"]

##################################################################
##### Combine grouped and non-grouped estimates #####
all_group_fes <- rbindlist(list(allfes, groupfes))

##### Calculate bootstrapped t-tests #####
# t-test approach: Bootstrap estimates standard error of t-test
# t-test has 1 degree of freedom, as n=2
ttests <- all_group_fes[, .(Jochmans = Jochmans[1], diff=MNE_fe[boot==1]-DOM_fe[boot==1], se=sd(MNE_fe[boot>1]-DOM_fe[boot>1])), by=c("K", "specification")][order(K, -specification)]
ttests[, pval := 2*pt(abs(diff/se), df=1, lower.tail = F)]
ttests[, c("Jochmans", "diff", "se", "pval") := lapply(.SD, function(x) round(x,4)), .SDcols=c("Jochmans", "diff", "se", "pval")]

##################################################################
#### Print result table #####
# print table
print(xtable(ttests, caption="T-test on MNE wage premium", label="tab:rob_mobility_ttests",
             digits=4),
      include.rownames = F,
      file = paste0(map_output, "robustness/clustered_fe/ttests_rob_mobility.tex"))