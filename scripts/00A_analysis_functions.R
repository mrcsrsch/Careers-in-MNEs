##################################################################################
### CUSTOM FUNCTIONS FOR ANALYSIS ####
##################################################################################
##################################################################################
#### general functions ####
estimates.table <- function(estimates, df = degrees_freedom(reg_main, type="t")){
  # simple function that creates list of tables for calculated estimates (highly generic to functions below)
  
  # extract coefs and standard errors
  coefs <- estimates$coefs
  stde <- estimates$sds
  
  # calculate t- and p-values
  t <- coefs/stde
  pv <- 2*pt(-abs(t), df = df)
  
  # create list of simple output tables
  out <- list()
  for (i in 1:ncol(coefs)){
    out[[i]] <- matrix(NA, nrow=nrow(coefs), ncol = 4)
    colnames(out[[i]]) <- c("coef", "se", "t", "p")
    rownames(out[[i]]) <- rownames(coefs)
    out[[i]][, 1] <- coefs[, i] 
    out[[i]][, 2] <- stde[, i] 
    out[[i]][, 3] <- t[, i] 
    out[[i]][, 4] <- pv[, i] 
  }
  names(out) <- colnames(coefs)
  
  return(out)
}

##################################################################################
#### functions wage analysis ###### 
##### A) mover wage growth:  returns to company.type experience vs. domestic experience ##### 
###### plotting #### 
company.type.plot <- function(estimates, type="Total", legend.add=TRUE, only.legend=FALSE, legend.names = NA,
                              highlight.MNE = FALSE, title = NA, graycolors=FALSE){
  require(viridis) # color palette
  require(fixest) # source of plotting function 
  
  coefs <- estimates$coefs
  sds <- estimates$sds
  
  # set legend names if missing
  if (missing(legend.names)) legend.names <- colnames(coefs)
  
  # adjust x-labels in plot
  lastl <- rownames(coefs)[nrow(coefs)]
  lastl <- sub(",.*]", "", lastl)
  lastl <- sub("\\(", "", lastl)
  rownames(coefs)[nrow(coefs)] <- paste0("> ", lastl)
  
  # set plot limits
  yhigh <- max(coefs+1.96*sds)*1.05
  ylow <- min(coefs-1.96*sds)*0.95
  
  # set plotting parameters
  pt.pch.set <- 25:(25-ncol(coefs)+1) # symbol types
  if (ncol(coefs) == 2) range <- 0.1 else range <- 0.2 # x shifting 
  x.shift.set <- seq(-range,range, length.out=ncol(coefs)) # x shifting 
  
  # if graycolors request, draw from graycolor scale
  if (graycolors){
    col.set <- gray.colors(ncol(coefs), alpha=1, end=0.6, rev=FALSE) # gray draw colors
  } else if (highlight.MNE){
    # draw grey color scale and replace with viridis colors for "MNE" categories
    col.set <- gray.colors(ncol(coefs), alpha=1, end=0.6, rev=FALSE) # gray draw colors
    #col.set <- adjustcolor(col.set, alpha.f = 0.7) # add transparency
    MNE.pos <- grep("MNE", colnames(coefs))
    col.set[MNE.pos] <- viridis(length(MNE.pos), alpha=1, end=0.6, direction=-1) # draw colors
  } else{
    # just put all firm classes in viridis volors
    col.set <- viridis(ncol(coefs), alpha=1, end=0.6, direction=-1) # draw colors
  }
  
  # create legend plot if necessary
  if (only.legend==TRUE){
    # empty plot with legend
    plot(NULL, xaxt="n", yaxt="n", ylab="", xlab="", xlim=0:1, ylim=0:1, axes=FALSE)
    legend(0, 1, cex=1.5, #cex increases legend size
           col = col.set, legend = legend.names, pch = pt.pch.set, title="")
  } else{
    
    ### Create actual plots 
    
    ## create first plot
    coefplot(coefs[,1], ylim=c(ylow, yhigh), 
             sd=sds[,1],
             x.shift=x.shift.set[1],
             pt.pch=pt.pch.set[1], 
             col=col.set[1],
             grid = FALSE,
             ylab = "")
    
    ## add other plots
    if (ncol(coefs) > 1){
      for (c in 2:ncol(coefs)){
        coefplot(coefs[,c], add=TRUE, 
                 sd = sds[,c],
                 x.shift=x.shift.set[c],
                 pt.pch=pt.pch.set[c],
                 col=col.set[c],
                 grid = FALSE)
      }
    }

    
    if (legend.add) legend("topleft", col = col.set, legend=legend.names, pch = pt.pch.set)
    
    if (is.na(title)) main <- paste(type, "value in", estimates$company.type) else main <- title
    title(main, 
          xlab = "Years of experience",
          ylab = "Wage premium")
  }
  
}
###### within domestic firms #### 
## How much more valuable is experience from company.type than experience from domestic firms in domestic firms / (without interactions) in an outside firm? 
total.general.prem <- function(reg_tt, exp.types, k){
  ### function that calculates coefficient of difference between 'company.type' experience and domestic experience (with interactions, this applies to dom. firm, otherwise to all firms)
  require(fixest) # type of reg_tt
  
  # remove "DOMESTIC" from exp.types if it exists
  if ("DOMESTIC" %in% exp.types) exp.types <- exp.types[-which(exp.types=="DOMESTIC")]
  
  l <- 0
  coefs <- matrix(NA, ncol=length(exp.types), nrow=length(grep("^exp_group_DOMESTIC", names(reg_tt$coefficients))))
  sds <- matrix(NA, ncol=length(exp.types), nrow=length(grep("^exp_group_DOMESTIC", names(reg_tt$coefficients))))
  for (i in exp.types){
    l <- l+1
    
    # i group coefficient
    i_coef <- reg_tt$coefficients[grep(paste0("^exp_group_",i), names(reg_tt$coefficients))]
    # reference group coefficient
    ref_coef <- reg_tt$coefficients[grep("^exp_group_DOMESTIC", names(reg_tt$coefficients))]
    # store coefficient (different between i and reference)
    coefs[, l] <- i_coef - ref_coef
    
    # calculate st.err. of difference
    # Var((A-C)) = Var(A) + Var(C) - 2 * Cov(A,C) 
    
    # variances
    VarA <- diag(vcov(reg_tt)[grep(paste0("^exp_group_",i), rownames(vcov(reg_tt))), 
                              grep(paste0("^exp_group_",i), colnames(vcov(reg_tt)))])
    VarC <- diag(vcov(reg_tt)[grep(paste0("^exp_group_","DOMESTIC"), rownames(vcov(reg_tt))), 
                              grep(paste0("^exp_group_","DOMESTIC"), colnames(vcov(reg_tt)))])
    
    # Covariances
    # add all covariances, use formula below to calc std. err. Use as inputttt for plots! 
    CovAC <- diag(vcov(reg_tt)[grep(paste0("^exp_group_", i), colnames(vcov(reg_tt))),
                               grep(paste0("^exp_group_", "DOMESTIC"), rownames(vcov(reg_tt)))])
    # Var((A+B)-(C+D)) = Var(A) + Var(B) + Var(C) + Var(D) + 2 * (Cov(A,B) + Cov(C,D) - Cov(A,C) - Cov(A,D) - Cov(B,C) - Cov(B,D))
    sds[,l] <- sqrt(VarA+VarC-2*CovAC)
  }
  # add names to matrices, might use these for plotting input 
  colnames(coefs) <- exp.types
  colnames(sds) <- exp.types
  # store names, then replace prefix and suffix and assign to rownames
  nn <- names(reg_tt$coefficients)[grep("^exp_group_DOMESTIC", names(reg_tt$coefficients))]
  nn <- gsub("^exp_group_DOMESTIC", "", nn)
  nn <- gsub(paste0("_", k, "::"), "", nn)
  rownames(coefs) <- nn
  rownames(sds) <- nn
  
  # return coefs and sds
  return(list(coefs=coefs, sds=sds, company.type="OUTSIDE FIRM (general)"))
}

total.company.type <- function(reg_tt, company.type="FMNE", exp.types, k){
  ### function that calculates coefficient of difference between a level + interaction, to a the reference group of domestic firms + interaction
  require(fixest) # type of reg_tt
  
  # remove "DOMESTIC" from exp.types if it exists
  if ("DOMESTIC" %in% exp.types) exp.types <- exp.types[-which(exp.types=="DOMESTIC")]
  
  # loop through other experiences
  coefs <- matrix(NA, ncol=length(exp.types), nrow=length(grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))))
  sds <- matrix(NA, ncol=length(exp.types), nrow=length(grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))))
  l <- 0
  for (i in exp.types){
    l <- l+1
    
    # i group coefficient
    i_coef <- reg_tt$coefficients[grep(paste0("^exp_group_",i, "_", k, "::"), names(reg_tt$coefficients))] + 
      reg_tt$coefficients[grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), names(reg_tt$coefficients))]
    
    # reference group coefficient
    ref_coef <- reg_tt$coefficients[grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))] + 
      reg_tt$coefficients[grep(paste0("^company.type.", k, "::", company.type, ":exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))]
    
    # store coefficient (different between i and reference)
    coefs[, l] <- i_coef - ref_coef
    
    # calculate st.err. of difference
    # Var((A+B)-(C+D)) = Var(A) + Var(B) + Var(C) + Var(D) + 2 * (Cov(A,B) + Cov(C,D) - Cov(A,C) - Cov(A,D) - Cov(B,C) - Cov(B,D))
    
    # variances
    VarA <- diag(vcov(reg_tt)[grep(paste0("^exp_group_",i, "_", k, "::"), rownames(vcov(reg_tt))), 
                              grep(paste0("^exp_group_",i, "_", k, "::"), colnames(vcov(reg_tt)))])
    VarB <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), rownames(vcov(reg_tt))), 
                              grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), colnames(vcov(reg_tt)))])
    VarC <- diag(vcov(reg_tt)[grep(paste0("^exp_group_","DOMESTIC_", k, "::"), rownames(vcov(reg_tt))), 
                              grep(paste0("^exp_group_","DOMESTIC_", k, "::"), colnames(vcov(reg_tt)))])
    VarD <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt))), 
                              grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), colnames(vcov(reg_tt)))])  
    
    # Covariances
    # add all covariances, use formula below to calc std. err. Use as inputttt for plots! 
    CovAB <- diag(vcov(reg_tt)[grep(paste0("^exp_group_",i, "_", k, "::"), colnames(vcov(reg_tt))),
                               grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), rownames(vcov(reg_tt)))])
    CovCD <- diag(vcov(reg_tt)[grep(paste0("^exp_group_", "DOMESTIC_", k, "::"), colnames(vcov(reg_tt))),
                               grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt)))])
    CovAC <- diag(vcov(reg_tt)[grep(paste0("^exp_group_",i, "_", k, "::"), colnames(vcov(reg_tt))),
                               grep(paste0("^exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt)))])
    CovAD <- diag(vcov(reg_tt)[grep(paste0("^exp_group_",i, "_", k, "::"), colnames(vcov(reg_tt))),
                               grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt)))])
    CovBC <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), rownames(vcov(reg_tt))),
                               grep(paste0("^exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt)))])
    CovBD <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), rownames(vcov(reg_tt))),
                               grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt)))])
    # Var((A+B)-(C+D)) = Var(A) + Var(B) + Var(C) + Var(D) + 2 * (Cov(A,B) + Cov(C,D) - Cov(A,C) - Cov(A,D) - Cov(B,C) - Cov(B,D))
    sds[,l] <- sqrt(VarA+VarB+VarC+VarD+2*(CovAB+CovCD-CovAC-CovAD-CovBC-CovBD))
  }
  # add names to matrices, might use these for plotting input 
  colnames(coefs) <- exp.types
  colnames(sds) <- exp.types
  # store names, then replace prefix and assign to rownames
  nn <- names(reg_tt$coefficients)[grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))]
  nn <- gsub(paste0("^exp_group_DOMESTIC_", k, "::"), "", nn)
  rownames(coefs) <- nn
  rownames(sds) <- nn
  
  # return coefs and sds
  return(list(coefs=coefs, sds=sds, company.type=company.type))
}
####### excess return (over domestic firm) ####
##### Do other firm types value experience more than domestic experience (in deviation to return in domestic firms
##### Differently put: Does the optimal later employer differ with the current employer? Does the optimal career path differ
# The estimates are: difference in return to MNE experience vs. domestic experience, when working in an MNE vs a domestic firm
excess.company.type <- function(reg_tt, company.type="FMNE", exp.types, k){
  ### function that calculates coefficient of difference between interactions, to the reference group of domestic experience interaction
  require(fixest) # type of reg_tt
  
  # remove "DOMESTIC" from exp.types if it exists
  if ("DOMESTIC" %in% exp.types) exp.types <- exp.types[-which(exp.types=="DOMESTIC")]
  
  # loop through other experiences
  coefs <- matrix(NA, ncol=length(exp.types), nrow=length(grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))))
  sds <- matrix(NA,  ncol=length(exp.types), nrow=length(grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))))
  l <- 0
  for (i in exp.types){
    l <- l+1
    
    # i group coefficient
    i_coef <- reg_tt$coefficients[grep(paste0("^company.type.", k, "::",company.type, ":exp_group_", i, "_", k, "::"), names(reg_tt$coefficients))]
    
    # reference group coefficient
    ref_coef <- reg_tt$coefficients[grep(paste0("^company.type.", k, "::", company.type, ":exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))]
    
    # store coefficient (different between i and reference)
    coefs[, l] <- i_coef - ref_coef
    
    # calculate st.err. of difference
    # Var(B-D) = Var(B) + Var(D) - 2 * Cov(B,D)
    
    # variances
    VarB <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", i, "_", k, "::"), rownames(vcov(reg_tt))), 
                              grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", i, "_", k, "::"), colnames(vcov(reg_tt)))])
    VarD <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt))), 
                              grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k,  "::"), colnames(vcov(reg_tt)))])  
    
    # Covariances
    CovBD <- diag(vcov(reg_tt)[grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", i, "_", k, "::"), rownames(vcov(reg_tt))),
                               grep(paste0("^company.type.", k, "::", company.type, ":exp_group_", "DOMESTIC_", k, "::"), rownames(vcov(reg_tt)))])
    sds[,l] <- sqrt(VarB+VarD-2*CovBD)
  }
  # add names to matrices, might use these for plotting input 
  colnames(coefs) <- exp.types
  colnames(sds) <- exp.types
  # store names, then replace prefix and assign to rownames
  nn <- names(reg_tt$coefficients)[grep(paste0("^exp_group_DOMESTIC_", k, "::"), names(reg_tt$coefficients))]
  nn <- gsub(paste0("^exp_group_DOMESTIC_", k, "::"), "", nn)
  rownames(coefs) <- nn
  rownames(sds) <- nn
  
  # return coefs and sds
  return(list(coefs=coefs, sds=sds, company.type=company.type))
}
##### B) stayer wage growth: Returns to within firm tenure (rel. to domestic firm return) ##### 
###### plotting ####
stayer.wages.plot <- function(estimates, legend.add=TRUE, highlight.MNE = FALSE, 
                              title = NA, legend.names = NA, graycolors=FALSE){
  require(viridis) # color palette
  require(fixest) # source of plotting function 
  
  coefs <- estimates$coefs
  sds <- estimates$sds
  
  # set legend names if missing
  if (missing(legend.names)) legend.names <- colnames(coefs)
  
  # adjust x-labels in plot
  lastl <- rownames(coefs)[nrow(coefs)]
  lastl <- sub(",.*]", "", lastl)
  lastl <- sub("\\(", "", lastl)
  rownames(coefs)[nrow(coefs)] <- paste0("> ", lastl)
  
  
  # set plot limits
  yhigh <- max(coefs+1.96*sds)*1.05
  ylow <- min(coefs-1.96*sds)*0.95
  
  # set plotting parameters
  pt.pch.set <- 25:(25-ncol(coefs)+1) # symbol types
  if (ncol(coefs) == 2) range <- 0.1 else range <- 0.2 # x shifting 
  x.shift.set <- seq(-range,range, length.out=ncol(coefs)) # x shifting 
  # if graycolors request, draw from graycolor scale
  if (graycolors){
    col.set <- gray.colors(ncol(coefs), alpha=1, end=0.6, rev=FALSE) # gray draw colors
  } else if (highlight.MNE){
    # draw grey color scale and replace with viridis colors for "MNE" categories
    col.set <- gray.colors(ncol(coefs), alpha=1, end=0.6, rev=FALSE) # gray draw colors
    #col.set <- adjustcolor(col.set, alpha.f = 0.7) # add transparency
    MNE.pos <- grep("MNE", colnames(coefs))
    col.set[MNE.pos] <- viridis(length(MNE.pos), alpha=1, end=0.6, direction=-1) # draw colors
  } else{
    # just put all firm classes in viridis volors
    col.set <- viridis(ncol(coefs), alpha=1, end=0.6, direction=-1) # draw colors
  }
  
  ### Create actual plots 
  
  ## create first line
  coefplot(coefs[,1], ylim=c(ylow, yhigh), 
           sd=sds[,1],
           x.shift=x.shift.set[1],
           pt.pch=pt.pch.set[1], 
           col=col.set[1],
           grid = FALSE,
           ylab = "")
  
  ## add other lines
  if (ncol(coefs) > 1){
    for (c in 2:ncol(coefs)){
      coefplot(coefs[,c], add=TRUE, 
               sd = sds[,c],
               x.shift=x.shift.set[c],
               pt.pch=pt.pch.set[c],
               col=col.set[c],
               grid = FALSE)
    }
  }

  
  if (legend.add) legend("topleft", col = col.set, legend=legend.names, pch = pt.pch.set)
  
  # Adjust Axis ticks
  axis(1, at=1:nrow(coefs), labels=FALSE)
  
  if (is.na(title)) title <- "Within-company wage growth (vs. worker in domestic company)"
  
  title(title, 
        xlab = "Years of experience",
        ylab = "Wage premium")
}
###### transform estimates for plotting ####
stayer.wages.estimates <- function(reg_tt, company.types, k){
  # to trick the plotting function (x-axis labels), create matrices of estimates and standard errors
  require(fixest) # type of reg_tt
  
  # first remove "DOMESTIC" from company.types if it exists
  if ("DOMESTIC" %in% company.types) company.types <- company.types[-which(company.types=="DOMESTIC")]
  
  # loop through other types
  coefs <- matrix(NA, ncol=length(company.types), nrow=length(reg_tt$coefficients[grep(paste0("^company.type.", k, "::", company.types[1], ":tenure_group_.*"), names(reg_tt$coefficients))]))
  sds <- matrix(NA, ncol=length(company.types), nrow=length(reg_tt$coefficients[grep(paste0("^company.type.", k, "::",  company.types[1], ":tenure_group_.*"), names(reg_tt$coefficients))]))
  l <- 0
  for (i in company.types){
    l <- l+1
    coefs[, l] <- reg_tt$coefficients[grep(paste0("^company.type.", k, "::", i, ":tenure_group_.*"), names(reg_tt$coefficients))]
    sds[, l] <- reg_tt$se[grep(paste0("^company.type.", k, "::", i, ":tenure_group_.*"), names(reg_tt$se))]
  }
  # add names to matrices, might use these for plotting input 
  colnames(coefs) <- company.types
  colnames(sds) <- company.types
  # store names, then replace prefix and assign to rownames
  nn <- names(reg_tt$coefficients)[grep(paste0("^company.type.", k, "::", company.types[1], ":tenure_group_.*"), names(reg_tt$coefficients))]
  nn <- gsub(paste0("^company.type.", k, "::", company.types[1], ":tenure_group_OGBEID_spell::"), "", nn)
  rownames(coefs) <- nn
  rownames(sds) <- nn
  rm(nn)
  
  return(list(coefs=coefs, sds=sds))
}
##################################################################################
#### functions exiter analysis ###### 
##### transition probability matrix ####
create.transitions.matrix <- function(reg.table, company.types, order.types = FALSE, probs = FALSE){
  
  # get exit.types from data
  exit.types <- unique(reg.table$exit.type)
  # order exit.types if wanted (i.e. make sure order of firm and exit types is similar)
  if (order.types){
    # order exit.types 
    ord <- which(exit.types %in% company.types)
    exit.types <- c(exit.types[-ord], company.types)
  }
  
  # Want absolute numbers of probabilities?
  if (probs){
    tt <- reg.table[company.type.1 %in% company.types, lapply(exit.types, function(x) sum(exit.type==x)/.N), by=company.type.1]
  } else {
    tt <- reg.table[company.type.1 %in% company.types, lapply(exit.types, function(x) sum(exit.type==x)), by=company.type.1]
  }
  
  # turn into matrix with company.type as rownames and exit.types as colnames 
  # turn tt into matrix
  tt <- as.matrix(tt[,-1])
  colnames(tt) <- exit.types
  rownames(tt) <- company.types
  # add row sums for clarification
  tt <- cbind(tt, sum=rowSums(tt))
  
  # return matrix
  return(tt)
  
}
##### linear fe model probabilities plots ######
stayer.probs.plot <- function(estimates, legend.add=TRUE, highlight.MNE = FALSE){
  require(viridis) # color palette
  require(fixest) # source of plotting function 
  
  coefs <- estimates$coefs
  sds <- estimates$sds
  
  
  # adjust x-labels in plot (first and last)
  l <- rownames(coefs)[c(1,nrow(coefs))]
  l[2] <- sub(",.*]", "", l[2])
  l[2] <- sub("\\(", "", l[2])
  rownames(coefs)[nrow(coefs)] <- paste0("> ", l[2])
  l[1] <- sub("\\[.*\\,", "", l[1])
  l[1] <- sub("\\]", "", l[1])
  rownames(coefs)[1] <- paste0("[0,", l[1], "]") 
  
  # set plot limits
  yhigh <- max(coefs+1.96*sds)*1.05
  ylow <- min(coefs-1.96*sds)*0.95
  
  # set plotting parameters
  pt.pch.set <- 25:(25-ncol(coefs)+1) # symbol 
  if (ncol(coefs) == 2) range <- 0.1 else range <- 0.2 # x shifting 
  x.shift.set <- seq(-range,range, length.out=ncol(coefs)) # x shifting 
  if (highlight.MNE){
    # draw grey color scale and replace with viridis colors for "MNE" categories
    col.set <- gray.colors(ncol(coefs), start = 0.35, end = 0.65) # draw grey colors
    col.set <- adjustcolor(col.set, alpha.f = 0.7) # add transparency
    MNE.pos <- grep("MNE", colnames(coefs)) # find pos of MNE entries
    col.set[MNE.pos] <- viridis(length(MNE.pos), alpha=1, end=0.6, direction=-1) # draw viridis colors
  } else{
    # just put all firm classes in viridis volors
    col.set <- viridis(ncol(coefs), alpha=1, end=0.6, direction=-1) # draw colors
  }  
  ### Create actual plots 
  
  ## create first line
  coefplot(coefs[,1], ylim=c(ylow, yhigh), 
           sd=sds[,1],
           x.shift=x.shift.set[1],
           pt.pch=pt.pch.set[1], 
           col=col.set[1],
           ylab = "",
           grid = FALSE)
  
  ## add other lines
  for (c in 2:ncol(coefs)){
    coefplot(coefs[,c], add=TRUE, 
             sd = sds[,c],
             x.shift=x.shift.set[c],
             pt.pch=pt.pch.set[c],
             col=col.set[c],
             grid = FALSE)
  }
  
  if (legend.add) legend("topright", col = col.set, legend=colnames(coefs), pch = pt.pch.set, title="Company type")
  
  # Adjust Axis ticks
  axis(1, at=1:nrow(coefs), labels=FALSE)
  
  title(paste("Probability to exit company (vs. worker in domestic company)"), 
        xlab = "Years of experience (within company)",
        ylab = "Probability to exit company")
}

##################################################################################
#### functions workerfe interaction analysis (wages) ###### 
##### plotting #### 
workerfe.plot <- function(estimates, type="MNE", legend.add=TRUE, title=NA, across=FALSE, graycolors=FALSE){
  require(viridis) # color palette
  require(fixest) # source of plotting function 
  
  # find type in estimates
  ex <- which(names(estimates)==type)
  
  coefs <- estimates[[ex]]$coefs
  sds <- estimates[[ex]]$sds
  
  # adjust x-labels in plot
  lastl <- rownames(coefs)[nrow(coefs)]
  lastl <- sub(",.*]", "", lastl)
  lastl <- sub("\\(", "", lastl)
  rownames(coefs)[nrow(coefs)] <- paste0("> ", lastl)
  
  # set plot limits
  yhigh <- max(coefs+1.96*sds)*1.05
  ylow <- min(coefs-1.96*sds)*0.95
  
  # set plotting parameters
  pt.pch.set <- 25:(25-ncol(coefs)+1) # symbol types
  if (ncol(coefs) == 2) range <- 0.1 else range <- 0.2 # x shifting 
  x.shift.set <- seq(-range,range, length.out=ncol(coefs)) # x shifting 
  # put all groups in viridis colors or gray
  if (graycolors){
    col.set <- gray.colors(ncol(coefs), alpha=1, end=0.6, rev=TRUE) # gray draw colors
    
  } else{
    col.set <- viridis(ncol(coefs), alpha=1, end=0.6, direction=-1) # draw colors
  }
  
  ### Create actual plots 
  
  ## create first plot
  coefplot(coefs[,1], ylim=c(ylow, yhigh), 
           sd=sds[,1],
           x.shift=x.shift.set[1],
           pt.pch=pt.pch.set[1], 
           col=col.set[1],
           grid = FALSE,
           ylab = "")
  
  ## add other plots
  for (c in 2:ncol(coefs)){
    coefplot(coefs[,c], add=TRUE, 
             sd = sds[,c],
             x.shift=x.shift.set[c],
             pt.pch=pt.pch.set[c],
             col=col.set[c],
             grid = FALSE)
  }
  
  if (legend.add) legend("topleft", col = rev(col.set), legend=rev(colnames(coefs)), pch = rev(pt.pch.set), title="Worker fixed effect group")
  
  if (is.na(title)) main <- paste(ifelse(across, "Experience of", "Within-company wage growth in"), type, "per worker fixed effect group") else main <- title
  title(main, 
        xlab = "Years of experience",
        ylab = "Wage premium")
  
}



##### calculate difference estimates #######
get.coefname <- function(across=TRUE, workerfe=FALSE, type="MNE", k=1){
  if (across){
    if (workerfe){
      tt <- paste0("^exp_group_", type, "_", k, "::.*fe_workerID")
    } else{
      tt <- paste0("^exp_group_", type, "_", k, "::.*]$")
    }
  } else{
    if (workerfe){
      if (type=="DOMESTIC") tt <- paste0("^tenure_group_OGBEID_spell::.*fe_workerID")
      if (type!="DOMESTIC") tt <- paste0("^company.type.", k, "::", type, ":", "tenure_.*fe_workerID")
    } else{
      if (type=="DOMESTIC") tt <- paste0("^tenure_group_OGBEID_spell::.*]$")
      if (type!="DOMESTIC") tt <- paste0("^company.type.", k, "::", type, ":", "tenure_.*]$")
    }
  }
  return(tt)
}

workerfe.estimates <- function(reg_tt, across=TRUE, exp.types, percentile_means, k, bvcov = NA){
  ### function that calculates coefficient of difference between 'company.type' experience and domestic experience (with interactions, this applies to dom. firm, otherwise to all firms)
  ### for different levels work the worker fixed effects
  require(fixest) # type of reg_tt
  
  # remove "DOMESTIC" from exp.types if it exists
  if ("DOMESTIC" %in% exp.types) exp.types <- exp.types[-which(exp.types=="DOMESTIC")]
  
  # if no boostrapped variance covariance matrix is specified, use reg_tt
  if (all(is.na(bvcov))) bvcov <- vcov(reg_tt)
  
  # create a list entry per experience type
  estimates <- vector("list", length(exp.types))
  names(estimates) <- exp.types
  
  # loop over exp types from here
  for (ex in 1:length(exp.types)){
    
    # create matrices for coefficients and standard errors per workerfes group in percentile_means
    estimates[[ex]]$coefs <- matrix(NA, ncol=nrow(percentile_means), 
                                    nrow=length(grep(get.coefname(across=across, workerfe = FALSE, type = "DOMESTIC", k=k), names(reg_tt$coefficients))))
    estimates[[ex]]$sds <- matrix(NA, ncol=nrow(percentile_means), 
                                  nrow=length(grep(get.coefname(across=across, workerfe = FALSE, type = "DOMESTIC", k=k), names(reg_tt$coefficients))))
    
    if (across){ # estimates and standard errors for movers
      # calculate st.err. of difference
      # Var((A+B)-(C+D)) = Var(A) + Var(B) + Var(C) + Var(D) + 2 * (Cov(A,B) + Cov(C,D) - Cov(A,C) - Cov(A,D) - Cov(B,C) - Cov(B,D))
      
      # variances
      VarA <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                         grep(get.coefname(across=TRUE, workerfe = FALSE, type = exp.types[ex], k=k), colnames(bvcov))])
      VarB <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = TRUE, type = exp.types[ex], k=k), rownames(bvcov)),
                         grep(get.coefname(across=TRUE, workerfe = TRUE, type = exp.types[ex], k=k), colnames(bvcov))])
      VarC <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = FALSE, type = "DOMESTIC", k=k), rownames(bvcov)),
                         grep(get.coefname(across=TRUE, workerfe = FALSE, type = "DOMESTIC", k=k), colnames(bvcov))])
      VarD <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = TRUE, type = "DOMESTIC", k=k), rownames(bvcov)),
                         grep(get.coefname(across=TRUE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      
      # Covariances
      # add all covariances, use formula below to calc std. err. Use as inputttt for plots! 
      CovAB <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=TRUE, workerfe = TRUE, type = exp.types[ex], k=k), colnames(bvcov))])
      CovCD <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = FALSE, type = "DOMESTIC", k=k), rownames(bvcov)),
                          grep(get.coefname(across=TRUE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      CovAC <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=TRUE, workerfe = FALSE, type = "DOMESTIC", k=k), colnames(bvcov))])
      CovAD <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=TRUE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      CovBC <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = TRUE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=TRUE, workerfe = FALSE, type = "DOMESTIC", k=k), colnames(bvcov))])
      CovBD <- diag(bvcov[grep(get.coefname(across=TRUE, workerfe = TRUE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=TRUE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      # calculate standard error using: 
      # Var((A+B)-(C+D)) = Var(A) + Var(B) + Var(C) + Var(D) + 2 * (Cov(A,B) + Cov(C,D) - Cov(A,C) - Cov(A,D) - Cov(B,C) - Cov(B,D))
      sds <- sqrt(VarA+VarB+VarC+VarD+2*(CovAB+CovCD-CovAC-CovAD-CovBC-CovBD))
      
      
      # calculate basic coefficients
      across_ex_basic <- reg_tt$coefficients[grep(get.coefname(across=TRUE, workerfe = FALSE, type = exp.types[ex], k=k), names(reg_tt$coefficients))]
      across_DOM_basic <- reg_tt$coefficients[grep(get.coefname(across=TRUE, workerfe = FALSE, type = "DOMESTIC", k=k), names(reg_tt$coefficients))]
      across_ex_workerfe <- reg_tt$coefficients[grep(get.coefname(across=TRUE, workerfe = TRUE, type = exp.types[ex], k=k), names(reg_tt$coefficients))]
      across_DOM_workerfe <- reg_tt$coefficients[grep(get.coefname(across=TRUE, workerfe = TRUE, type = "DOMESTIC", k=k), names(reg_tt$coefficients))]
      
      # take excess of ex to DOM
      basic <- across_ex_basic-across_DOM_basic
      workerfe <- across_ex_workerfe-across_DOM_workerfe
    } else { # estimates and standard errors for non-movers
      # calculate st.err. of difference (company type Interaction + company type workerfe interaction - workerfe interaction)
      # Var(A+(B-C)) = Var(A) + Var(B) + Var(C) + 2*(Cov(A,B)-Cov(A,C)-Cov(B,C))
      
      # Variances 
      VarA <- diag(bvcov[grep(get.coefname(across=FALSE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                         grep(get.coefname(across=FALSE, workerfe = FALSE, type = exp.types[ex], k=k), colnames(bvcov))])
      VarB <- diag(bvcov[grep(get.coefname(across=FALSE, workerfe = TRUE, type = exp.types[ex], k=k), rownames(bvcov)),
                         grep(get.coefname(across=FALSE, workerfe = TRUE, type = exp.types[ex], k=k), colnames(bvcov))])
      VarC <- diag(bvcov[grep(get.coefname(across=FALSE, workerfe = TRUE, type = "DOMESTIC", k=k), rownames(bvcov)),
                         grep(get.coefname(across=FALSE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      
      # Covariances
      CovAB <- diag(bvcov[grep(get.coefname(across=FALSE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=FALSE, workerfe = TRUE, type = exp.types[ex], k=k), colnames(bvcov))])
      CovAC <- diag(bvcov[grep(get.coefname(across=FALSE, workerfe = FALSE, type = exp.types[ex], k=k), rownames(bvcov)),
                          grep(get.coefname(across=FALSE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      CovBC <-  diag(bvcov[grep(get.coefname(across=FALSE, workerfe = TRUE, type = exp.types[ex], k=k), rownames(bvcov)),
                           grep(get.coefname(across=FALSE, workerfe = TRUE, type = "DOMESTIC", k=k), colnames(bvcov))])
      
      # calculate standard error using: 
      # Var(A+(B-C)) = Var(A) + Var(B) + Var(C) + 2*(Cov(A,B)-Cov(A,C)-Cov(B,C))
      sds <- sqrt(VarA+VarB+VarC+2*(CovAB-CovAC-CovBC))
      
      # calculate basic coefficients
      basic <- reg_tt$coefficients[grep(get.coefname(across=FALSE, workerfe = FALSE, type = exp.types[ex], k=k), names(reg_tt$coefficients))]
      within_ex_workerfe <- reg_tt$coefficients[grep(get.coefname(across=FALSE, workerfe = TRUE, type = exp.types[ex], k=k), names(reg_tt$coefficients))]
      within_DOM_workerfe <- reg_tt$coefficients[grep(get.coefname(across=FALSE, workerfe = TRUE, type = "DOMESTIC", k=k), names(reg_tt$coefficients))]
      workerfe <- within_ex_workerfe-within_DOM_workerfe
    }
    
    
    
    # loop over rows in percentile means to get ceofficients and add sds
    l <- 0
    for (i in percentile_means$fe_workerID){
      l <- l+1
      # calculate coefficients
      estimates[[ex]]$coefs[,l] <- basic+workerfe*i
      # add standard errors
      estimates[[ex]]$sds[,l] <- sds
      
    }
    colnames(estimates[[ex]]$coefs) <- percentile_means$fe_workerID_percentile 
    colnames(estimates[[ex]]$sds) <- percentile_means$fe_workerID_percentile 
    
    # store names, then replace prefix and suffix and assign to rownames
    nn <- names(reg_tt$coefficients)[grep(get.coefname(across=across, workerfe = FALSE, type = "DOMESTIC", k=k), names(reg_tt$coefficients))]
    nn <- gsub("^exp_group_DOMESTIC", "", nn)
    nn <- gsub("^tenure_group_OGBEID_spell::", "", nn)
    nn <- gsub(paste0("_", k, "::"), "", nn)
    rownames(estimates[[ex]]$coefs) <- nn
    rownames(estimates[[ex]]$sds) <- nn
  }
  # return the list
  return(estimates)
}
##################################################################################
#### functions cohort analysis ######
cohort.plot <- function(estimates, workerfe_interaction=FALSE, horizontal = TRUE, graycolors=FALSE, legend.names = NA, tit = "Probability of MNE exployment since labor market entry (rel. to entry year)"){
  # this function plots the estimates with a flipped cooardinate system
  require(ggplot2)
  require(viridis)
  
  coefs <- estimates$coefs
  sds <- estimates$sds
  
  # set legend names if missing
  if (missing(legend.names)) legend.names <- colnames(coefs)
  
  # adjust labels in plot
  nn <- rownames(coefs)
  nn <- sub("\\([0-9],", "", nn)
  nn <- sub("\\]", "", nn)
  lastl <- rownames(coefs)[nrow(coefs)]
  lastl <- sub(",.*]", "", lastl)
  lastl <- sub("\\(", "", lastl)
  rownames(coefs) <- c(nn[-length(nn)], paste0("> ", lastl))
  
  # set plot limits
  yhigh <- max(coefs+1.96*sds)
  yhigh <- ifelse(yhigh<0, yhigh*0.95, yhigh*1.05)
  ylow <- min(coefs-1.96*sds)
  ylow <- ifelse(ylow<0, ylow*1.05, ylow*0.95)
  
  # set plotting parameters
  
  ## set symbol type when only one col
  if (ncol(coefs)==1){
    pt.pch.set <- 23 # symbol types
  } else{
    pt.pch.set <- rev(25:(25-ncol(coefs)+1)) # symbol types
  }
  if (ncol(coefs) == 2) range <- 0.1 else range <- 0.2 # x shifting 
  if (ncol(coefs) == 1) range <- 0
  x.shift.set <- seq(-range,range, length.out=ncol(coefs)) # x shifting 
  # put all groups in viridis volors
  if (graycolors){
    col.set <- gray.colors(ncol(coefs), alpha=1, end=0.6, rev=FALSE) # gray draw colors
  }else{
    col.set <- viridis(ncol(coefs), alpha=1, end=0.6, direction=-1) # draw colors
  }
  
  ### Create actual plots 
  
  ## create first plot
  if (horizontal){
    # reorder coefs and sds
    coefs <- apply(coefs, MARGIN=2, function(x) x[length(x):1])
    sds <- apply(sds, MARGIN=2, function(x) x[length(x):1])
    
    coefplot(coefs[,1], xlim=c(ylow, yhigh), horiz=TRUE,
             sd=sds[,1],
             x.shift=x.shift.set[1],
             pt.pch=pt.pch.set[1], 
             col=col.set[1],
             grid = FALSE,
             ylab = "Years on labor market",
             xlab = "MNE employment probability")
  } else{
    coefplot(coefs[,1], ylim=c(ylow, yhigh), 
             sd=sds[,1],
             x.shift=x.shift.set[1],
             pt.pch=pt.pch.set[1], 
             col=col.set[1],
             grid = FALSE,
             ylab = "")
  }
  
  
  ## add other plots, if workerfe interaction
  
  if (workerfe_interaction){
    for (c in 2:ncol(coefs)){
      if (horizontal){
        coefplot(coefs[,c], add=TRUE, horiz=TRUE,
                 sd = sds[,c],
                 x.shift=x.shift.set[c],
                 pt.pch=pt.pch.set[c],
                 col=col.set[c],
                 grid = FALSE)
      } else{
        coefplot(coefs[,c], add=TRUE, 
                 sd = sds[,c],
                 x.shift=x.shift.set[c],
                 pt.pch=pt.pch.set[c],
                 col=col.set[c],
                 grid = FALSE)
      }
      
    }
    legend("bottomleft", col = col.set, legend=legend.names, pch = pt.pch.set, title="Worker fixed effect group")
  } 
  if (exists("tit")) title(tit)
}

cohort.estimates <- function(reg_tt, workerfe_interaction=FALSE, percentile_means=NA, bvcov = NA){
  # function that returns either the non-interacted coefficients/standard errors in matrix form, 
  # or interacted coefficients per percentile_means row. 
  
  # if no boostrapped variance covariance matrix is specified, use reg_tt
  if (all(is.na(bvcov))) bvcov <- vcov(reg_tt)
  
  # create matrices for new coefficients and their standard errors (per workerfe group if specified)
  if (workerfe_interaction){
    cols <- nrow(percentile_means)
  } else {cols <- 1}
  
  coefs <- matrix(NA, ncol=cols, 
                  nrow=length(grep("labor_exp_group.*]$", names(reg_tt$coefficients))))
  sds <- matrix(NA, ncol=cols, 
                nrow=length(grep("labor_exp_group.*]$", names(reg_tt$coefficients))))
  
  if (workerfe_interaction){
    # calculate coefficients
    basic <- reg_tt$coefficients[grep("labor_exp_group.*]$", names(reg_tt$coefficients))]
    workerfe <- reg_tt$coefficients[grep("labor_exp_group.*fe_workerID$", names(reg_tt$coefficients))]
    
    # calculate standard errors: Var(A+B) = Var(A) + Var(B) + 2*Cov(A,B)
    ## Variances
    VarA <- diag(bvcov[grep("labor_exp_group.*]$", rownames(bvcov)),
                       grep("labor_exp_group.*]$", colnames(bvcov))])
    VarB <- diag(bvcov[grep("labor_exp_group.*fe_workerID$", rownames(bvcov)),
                       grep("labor_exp_group.*fe_workerID$", colnames(bvcov))])
    
    ## Covariances
    CovAB <- diag(bvcov[grep("labor_exp_group.*]$", rownames(bvcov)),
                        grep("labor_exp_group.*fe_workerID$", colnames(bvcov))])
    ### calculate standard errors
    se <- sqrt(VarA+VarB+2*CovAB)
    
    l <- 0
    for (i in percentile_means$fe_workerID){
      l <- l+1
      # calculate coefficients
      coefs[,l] <- basic+workerfe*i
      # add standard errors
      sds[,l] <- se
    }
    # adjust names
    colnames(coefs) <- percentile_means$fe_workerID_percentile 
    colnames(sds) <- percentile_means$fe_workerID_percentile 
    
  } else{
    coefs[, 1] <- reg_tt$coefficients[grep("labor_exp_group.*]$", names(reg_tt$coefficients))]
    sds[, 1] <- sqrt(diag(bvcov[grep("labor_exp_group.*]$", rownames(bvcov)),
                                grep("labor_exp_group.*]$", colnames(bvcov))]))
  }
  
  # store names, then replace prefix and suffix and assign to rownames
  nn <- names(reg_tt$coefficients)[grep("labor_exp_group.*]$", names(reg_tt$coefficients))]
  nn <- gsub("^labor_exp_group_2::", "", nn)
  rownames(coefs) <- nn
  rownames(sds) <- nn
  
  return(list(coefs=coefs, sds=sds))
}
