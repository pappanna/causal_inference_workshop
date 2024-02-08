#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 4 - IV and RDD, IV using published data (Card 1995)
# Based on: https://mixtape.scunning.com/07-instrumental_variables
# last modified: 01/25/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, dplyr, tidyr, AER, haven, tidyverse, stargazer)    

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

## set seed for reproducibility
set.seed(123)

# Download data ------------------------------------------------------------------------------

# download data (from https://mixtape.scunning.com/07-instrumental_variables) and remove attributes (since they're loaded from stata file)
dataCard <- read_dta("https://github.com/scunning1975/mixtape/raw/master/card.dta")
dataCard[] <- lapply(dataCard, function(x) { attributes(x) <- NULL; x })

# keep relevant data 
# lwage : log wages; educ : education (endogenous); exper, black, south, married, smsa: covariates, nearc4: college in county (instrument)
dataCard <- dataCard %>% dplyr::select(lwage, educ, exper, black, south, married, smsa, nearc4)

# Summary stats ------------------------------------------------------------------------------

stargazer(data.frame(dataCard), type="text", digits =2)

# Regressions --------------------------------------------------------------------------------

## OLS 
modelOLS <- lm(data = dataCard, lwage ~ educ + exper + black + south + married + smsa)

## do 2SLS manually (CAUTION: these SEs are NOT correct, just for illustration), note same control variables 
modelFirstStage <- lm(data = dataCard, educ ~ nearc4 + exper + black + south + married + smsa, na.action=na.exclude)
dataCard$educHat <- predict(modelFirstStage)
modelSecondStage <- lm(data = dataCard, lwage ~ educHat + exper + black + south + married + smsa)
# modify name of coefficient here for table (CAUTION: this is just for the display below, NOT a good idea otherwise)
names(modelSecondStage$coefficients)[names(modelSecondStage$coefficients) == "educHat"] <- "educ"

## now let's bootstrap standard errors instead 
save <- data.frame()

for (i in 1:nrow(dataCard)){
  
  # sample the data with replacement   
  sample <- sample_n(dataCard, nrow(dataCard), replace=T)
  
  # run regressions again 
  modelFirstStageBS <- lm(data = sample, educ ~ nearc4 + exper + black + south + married + smsa, na.action=na.exclude)
  sample$educHat <- predict(modelFirstStageBS)
  modelSecondStageBS <- lm(data = sample, lwage ~ educHat + exper + black + south + married + smsa)
 
  # save coefficient 
  save <- rbind(save, as.numeric(modelSecondStageBS$coefficients[2]))
  
} 

# calculate standard deviation of coefficients 
colnames(save) <- c("coef")
seBootStrap <- sd(save$coef, na.rm=T)

## use IVREG 
model2SLS <- ivreg(data=dataCard, lwage ~ educ + exper + black + south + married + smsa | nearc4 + exper + black + south + married + smsa)
summary(model2SLS, diagnostics = T) # can get first-stage F-stat from here  

## Table ---- 
stargazer(modelOLS, modelSecondStage, model2SLS, modelFirstStage,
          type="text", df=F, report=("vc*sp"), model.names = FALSE,
          omit = c("Constant"),
          keep = c("educ", "nearc4"),
          dep.var.labels = c("Log Earnings", "Years of Schooling"),
          covariate.labels = c("Years of Schooling", 'College in County'),
          title = "OLS vs. IV - Card (1995)",
          add.lines = list(c("Estimator", "OLS", "2SLS", "2SLS", "FS OLS"),
                           c("Bootstrapped SEs", " ", round(seBootStrap, 3), " ", " ")))
