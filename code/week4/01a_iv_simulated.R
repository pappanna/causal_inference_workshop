#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 4 - IV and RDD, IV using simulated data 
# last modified: 01/25/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, dplyr, tidyr, 
               MASS, AER, stargazer)    

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

## set seed for reproducibility
set.seed(123)

# Generate data ------------------------------------------------------------------------------

# constants / parameters 
n <- 1000 
cor <- 0.50
instrStrength <- 0.25

# generate D* and error 
gen <- mvrnorm(n, c(10, 5), matrix(c(1, cor, cor, 1), 2, 2))
Ds <- gen[,1]
c <- gen[,2]
rm(gen)

# generate instrument 
Z <- rnorm(n)

# generate observed D
D <- Ds + instrStrength * Z

# generate error 
e <- c + rnorm(n, 0, 0.5)

# generate outcome variable 
y <- D + e

# check correlations 
cor(D, e) 
cor(Z, e)

# put known values into dataframe 
data <- data.frame(y, D, Z)

# Regressions ------------------------------------------------------------------------------

## OLS 
modelOLS <- lm(data = data, y ~ D)

## do 2SLS manually (CAUTION: these SEs are NOT correct, just for illustration)
modelFirstStage <- lm(data = data, D ~ Z)
data$DHat <- predict(modelFirstStage)
modelSecondStage <- lm(data = data, y ~ DHat)
# modify name of coefficient here for table (CAUTION: this is just for the display below, NOT a good idea otherwise)
names(modelSecondStage$coefficients)[names(modelSecondStage$coefficients) == "DHat"] <- "D"

## now let's bootstrap standard errors instead 
save <- data.frame()

for (i in 1:1000){
  
  # sample the data with replacement   
  sample <- sample_n(data, n, replace=T)
  
  # run regressions again 
  modelFirstStageBS <- lm(data = sample, D ~ Z)
  sample$DHat <- predict(modelFirstStageBS)
  modelSecondStageBS <- lm(data = sample, y ~ DHat)
  
  # save coefficient 
  save <- rbind(save, as.numeric(modelSecondStageBS$coefficients[2]))

} 

# calculate standard deviation of coefficients 
colnames(save) <- c("coef")
seBootStrap <- sd(save$coef)

## use IVREG 
model2SLS <- ivreg(data=data, y ~ D | Z)
summary(model2SLS, diagnostics = T) # can get first-stage F-stat from here too 

## Table ---- 
stargazer(modelOLS, modelSecondStage, model2SLS, modelFirstStage,
          type="text", df=F, report=("vc*sp"), model.names = FALSE,
          omit = c("Constant"),
          title = "Simulated Data - OLS vs. IV",
          add.lines = list(c("Estimator", "OLS", "2SLS", "2SLS", "FS OLS"),
                           c("Bootstrapped SEs", " ", round(seBootStrap, 3), " ", " ")))

# Exercises ------------------------------------------------------------------------------

# modify the strength of the instrument - what happens to the 2SLS estimates? 
# modify the correlation between D and e - what happens to the OSL vs. 2SLS estimates? 
# (bonus) modify the DGP to include another variable affected by the instrument that then affects the instrument (e.g., a mediating path from Z to Y) - how does this change estimates?

  
