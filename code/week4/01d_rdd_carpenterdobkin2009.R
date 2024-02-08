#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 4 - IV and RDD, RD using published data (Carpenter and Dobkin 2009)
# Based on: https://rpubs.com/phle/r_tutorial_regression_discontinuity_design
# last modified: 02/08/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, dplyr, tidyr, tidyverse, 
               rddtools, rdrobust)   

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

# Get Data -------------------------------------------------------------------------------------

# Carpenter and Dobkin, 2009 data (from: https://github.com/jrnold/masteringmetrics/blob/master/masteringmetrics/data/mlda.rda)
# aggregated values according to respondents' ages 
load("week4/data/carpenter_dobkin_2009.rda")
data <- mlda
rm(mlda)

# Scatterplot -----------------------------------------------------------------------------------

# scatterplot of aggregated values 
ggplot(data = data, aes(x = agecell, y = all)) + 
  geom_point() +
  geom_vline(xintercept = 21, color = "red", size = 1, linetype = "dashed") + 
  labs(y = "Mortality rate (per 100,000)",x = "Age (binned)") + 
  theme_bw()

# now do the same with rdplot from rdrobust
# more details here: https://rdrr.io/cran/rdrobust/man/rdplot.html
# note, here I am manually setting nbins = 50 because the data is already binned, so I don't want further binning! 
rdplot(data$all, data$agecell, c = 21,
       nbins = 50,
       x.label = 'Age (binned)', y.label = "Mortality rate (per 100,000)", title = "")

# Regressions -----------------------------------------------------------------------------------

data <- data %>% mutate(threshold = ifelse(agecell >= 21, 1, 0))

# same slope regression
modelSS <- lm(data = data, all ~ threshold + I(agecell - 21))
summary(modelSS)

# different slope regression 
modelDS <- lm(data = data, all ~ threshold + I(agecell - 21) + threshold:I(agecell - 21))
summary(modelDS)

# quadratic regression
modelQD <- lm(data = data, all ~ threshold + I(agecell - 21) + I((agecell -21)^2) + threshold:I(agecell - 21) + threshold:I((agecell - 21)^2))
summary(modelQD)

# Bandwidth -----------------------------------------------------------------------------------

# bandwidth selection using rdrobust package 
bw <- rdbwselect(data$all, data$agecell, c = 21)
summary(bw)

# now let's compare using this bandwidth 
data <- data %>% filter(agecell >= 21 - 0.493, agecell <= 21 + 0.493)

# scatterplot of aggregated values 
ggplot(data = data, aes(x = agecell, y = all)) + 
  geom_point() +
  geom_vline(xintercept = 21, color = "red", size = 1, linetype = "dashed") + 
  labs(y = "Mortality rate (per 100,000)",x = "Age (binned)") + 
  theme_bw()

# run different slope regression again 
modelDS <- lm(data = data, all ~ threshold + I(agecell - 21) + threshold:I(agecell - 21))
summary(modelDS)

# Exercises -----------------------------------------------------------------------------------

# do some sensitivity checks with the bandwidth and/or f()




