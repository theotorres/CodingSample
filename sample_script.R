# ============================================================================ #
  # Coding sample - Research Assistant (STC) position - World Bank group
  # Theo da Fonseca Torres - University of Brasília



# Install packages
install.packages("survey", dependencies=TRUE)
install.packages("data.table", dependencies=TRUE)
install.packages("foreign", dependencies=TRUE)
install.packages("stargazer", dependencies=TRUE) 
install.packages("MartchIt", dependencies=TRUE)
install.packages("sandwich", dependencies=TRUE)
install.packages("rbounds", dependencies=TRUE)
install.packages("psych", dependencies=TRUE) 
install.packages("dplyr", dependencies=TRUE)
install.packages("optmatch", dependencies=TRUE)

# Loading packages
library(survey)
library(data.table)
library(foreign)
library(stargazer)
library(MatchIt)
library(sandwich)
library(rbounds)
library(psych)
library(dplyr)
library(optmatch)

# Here you set the directory which the sample dataset is saved
workingdirectory <- "\\your\\path"
setwd(workingdirectory)

# Loading sample dataset
load("sample_data.Rda")

# Define options
options(survey.lonely.psu = "adjust")

#-------------------------------------------------------------------------------
# Auxiliary function
#-------------------------------------------------------------------------------
t.test2 <- function(m1, m2, s1, s2, n1, n2, m0=0, equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df formula
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}


#--------------------------------------------------------------------------------#
## Setting variables and functions 
indvars <- c("carboidrato_f") 
depvars <- c("trat_dom") 
controlvars <- c("branco","idadefilhos_m","idaderesponsavel_m",
                 "n_filhos","casado","renda_percapita",
                 "chefefamilia_mulher",
                 colnames(POF_F)[grep("UF\\d", colnames(POF_F))])

formulas <- lapply(
  paste(apply(expand.grid(indvars, depvars), 1, paste, collapse = " ~ "),
        "+", paste(controlvars, collapse = " + ")), as.formula)

aux_formulas <- lapply(
  rep(paste(depvars, "~", paste(controlvars, collapse = " + ")),
      length(indvars)), as.formula)

POF2 <- svydesign(id=~ID, strata=~ESTRATO_POF, weights =~pesofinal_c, data=POF_F, nest=TRUE)


#--------------------------------------------------------------------------------#
###################################################
## Running models
###################################################

## Setting result lists

results <- list()
results_aux <- list()

#-------------------------------------------------------------------------------
## OLS with complex structure
#-------------------------------------------------------------------------------
models <- list()
stds <- list()
for (i in 1:length(formulas)) {
  
  reg <- svyglm(formulas[[i]], POF2)
  vcov <- summary(reg, df.resid = degf(POF2))
  models[[i]] <- reg
  stds[[i]] <- vcov
  rm(list = c("reg", "vcov"))
  
}
results$ols_survey <- models
results_aux$ols_survey <- stds

#-------------------------------------------------------------------------------
## PSW with complex structure
#-------------------------------------------------------------------------------
models <- list()
stds <- list()
ps_b <- list()
for (i in 1:length(formulas)) {
  idx <- complete.cases(POF_F[, all.vars(formulas[[i]]), with = FALSE])
  ps_b[[i]] <- glm(aux_formulas[[i]], data = POF_F[idx], family = "binomial")
  pscore <- predict(ps_b[[i]], data = POF_F[idx], type = "response")
  att.wt <- ifelse(POF_F[idx, trat_dom] == 1, 1, pscore/(1-pscore))
  att.ww <- POF_F[idx, pesofinal_c]*att.wt
  POF3 <- subset(POF2, idx)
  update(POF3, pscore = att.ww)
  reg <- svyglm(formulas[[i]], POF3, weights = pscore)
  vcov <- summary(reg, df.resid = degf(POF3))
  models[[i]] <- reg
  stds[[i]] <- vcov
  rm(list = c("idx", "att.wt", "POF3", "reg", "vcov"))
  
}
results$wls_survey <- models
results_aux$wls_survey <- stds

#-------------------------------------------------------------------------------
## PSM with complex structure
#-------------------------------------------------------------------------------
models <- list()
stds <- list()
mdata <- list()
ps <- list()
for (i in 1:length(formulas)) {
  
  idx <- complete.cases(POF_F[, all.vars(formulas[[i]]), with = FALSE])
  auxdata <- model.frame(formulas[[i]], droplevels(POF_F[idx]))
  auxdata <- cbind(auxdata, POF_F[idx, .(ESTRATO_POF, ID, pesofinal_c)])
  set.seed(123)
  ps[[i]] <- matchit(aux_formulas[[i]], data = auxdata,
                     method = "nearest", ratio = 1, distance = "logit") 
  mdata[[i]] <- match.data(ps[[i]])
  POF3 <- svydesign(id = ~ID, strata = ~ESTRATO_POF, data = mdata[[i]],
                    weights = ~pesofinal_c, nest = TRUE)
  reg <- svyglm(formulas[[i]], POF3)
  vcov <- summary(reg, df.resid = degf(POF3))
  models[[i]] <- reg
  stds[[i]] <- vcov
  rm(list = c("idx", "auxdata", "reg", "vcov"))
}
results$match_survey <- models
results_aux$match_survey <- stds


#--------------------------------------------------------------------------------#
## Rename objects
results_total <- results
results_total_aux <- results_aux
ps_total <- ps
ps_b_total <- ps_b

#-------------------------------------------------------------------------------
## Covariates balance after matching
#-------------------------------------------------------------------------------
treated <- POF_F[trat_dom == 1 & complete.cases(POF_F[, all.vars(formulas[[1]]), with = FALSE]), controlvars, with = FALSE]
balance <- summary(ps_total[[1]])$sum.matched[2:8, 1:4] 
balance <- as.data.frame(balance)
balance$"SD Treated" <- apply(treated, 2, sd)[1:7] 
balance <- balance[, c(1, 5, 2:4)]

ttest <- numeric(nrow(balance))
pttest <- numeric(nrow(balance))

# Running function ttest2
for (i in 1:nrow(balance)) {
  tfunc <- t.test2(balance[i, 1], balance[i, 3], balance[i, 2], balance[i, 4], n1 = nrow(treated), n2 = nrow(treated))
  ttest[i] <- tfunc[3]
  pttest[i] <- tfunc[4]
}
balance$"T-test" <- ttest
balance$"P-value" <- pttest
balance <- round(balance, 4)
balance<-select(balance, -2,-4,-5)
rownames(balance) <- c("Guardians are white", "Average age of children", "Average age of guardians", "Number of children", "Guardians are married", "Per capita income", "Female head of household")

## Balance Table
bal_t1 <- stargazer(balance, summary = FALSE, type = "text")


#-------------------------------------------------------------------------------
## Results
#-------------------------------------------------------------------------------
list.results <- list(results_total$ols_survey[[1]], results_total$wls_survey[[1]],
                     results_total$match_survey[[1]])
t1 <- stargazer(list.results,
                se = list(results_total_aux$ols_survey[[1]]$coefficients[, 2],
                          results_total_aux$wls_survey[[1]]$coefficients[, 2],
                          results_total_aux$match_survey[[1]]$coefficients[, 2]),
                omit = c(controlvars, "Constant"),
                column.labels = c("OLS", "PSW", "PSM"),
                column.separate = c(1, 1, 1), dep.var.labels.include = FALSE,
                style = "qje", font.size = "footnotesize",
                covariate.labels = c("Carbohydrates"),
                omit.table.layout = "n", omit.stat = c("ll", "aic"),
                title = "Impact of PBF", type = "text")

