#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 4 - IV and RDD, RD using simulated data 
# Based on: https://mixtape.scunning.com/06-regression_discontinuity
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

## set seed for reproducibility
set.seed(123)

# constants / parameters 
n <- 1000 
LATE <- 35
int <- 25
cutoff <- 50

# Part 1 - linear -------------------------------------------------------------------------
# Generate data ------------------------------------------------------------------------------

# running variable 
data <- data.frame(x = rnorm(n, 50, 25)) %>% mutate(x = ifelse(x < 0, 0, x)) %>% filter(x < 100) 

# treatment
data <- data %>% mutate(D = ifelse(x > cutoff, 1, 0))

# potential outcome WITHOUT treatment 
data <- data %>% mutate(yp = ifelse(x < cutoff, 0 * D + 1.4 * x + rnorm(n(), 0, 30), 
                                     -(3-1.4)*cutoff + 0 * D + 3.0 * x + rnorm(n(), 0, 30)))

# potential outcome WITH treatment 
data <- data %>% mutate(y = yp + LATE * D)

# center cutoff 
data <- data %>% mutate(xCentered = x - cutoff)

write.csv(data, "test.csv")

# Plot data ---------------------------------------------------------------------------------

# now we can plot the potential outcome, which we cannot observe 
ggplot(aes(xCentered, yp, colour = factor(D)), data = data) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome") + 
  theme_bw() + theme(legend.position="none")+
  ggtitle("Smoothness of Potential Outcomes")

# now plot observed outcomes 
ggplot(aes(xCentered, y, colour = factor(D)), data = data) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  theme_bw() + theme(legend.position="none")+
  labs(x = "Test score (X)", y = "Observed Outcome (Y)")

# OR can also do this with the rdplot command from rdrobust
# more details here: https://rdrr.io/cran/rdrobust/man/rdplot.html
rdplot(data$y, data$x, c = cutoff, 
       binselect="es", 
       x.label = 'Test score (X)', y.label = "Observed Outcome (Y)", title = "")

# Regression ---------------------------------------------------------------------------------

# same slopes 
modelSS <- lm(data = data, y ~ D + I(x-cutoff))
summary(modelSS)

# different slopes 
modelDS <- lm(data = data, y ~ D + I(x-cutoff) + D:I(x-cutoff))
summary(modelDS)

# using rddtools package 
rddData <- rdd_data(y = data$y, 
                    x = data$x, 
                    cutpoint = cutoff)
modelRDDToolsSame <- rddData %>% rdd_reg_lm(slope = "same") 
modelRDDToolsDiff <- rddData %>% rdd_reg_lm(slope = "separate") 
summary(modelRDDToolsSame)
summary(modelRDDToolsDiff)

# Part 2 - nonlinear ----------------------------------------------------------------------
# Generate data ------------------------------------------------------------------------------

# take same data but now create y nonlinearly, no discontinuity near D 
dataNL <- data %>% dplyr::select(x, D)
dataNL <- dataNL %>% mutate(x2 = x * x,  
                            x3 = x * x * x)
dataNL <- dataNL %>% mutate(y = 10000 - 300 * x + 6 * x2 + 0.00003 * x3 + rnorm(n(), 0, 1000))

# center cutoff 
dataNL <- dataNL %>% mutate(xCentered = x - cutoff)

# now plot observed outcomes 
ggplot(aes(xCentered, y, colour = factor(D)), data = dataNL) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = cutoff, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  theme_bw() + theme(legend.position="none")+
  labs(x = "Test score (X)", y = "Observed Outcome (Y)")

# OR can also do this with the rdplot command from rdrobust
# more details here: https://rdrr.io/cran/rdrobust/man/rdplot.html
rdplot(dataNL$y, dataNL$x, c = cutoff, 
       binselect="es", 
       x.label = 'Test score (X)', y.label = "Observed Outcome (Y)", title = "")

# same slopes 
modelSS <- lm(data = dataNL, y ~ D + I(x-cutoff))
summary(modelSS)

# different slopes 
modelDS <- lm(data = dataNL, y ~ D + I(x-cutoff) + D:I(x-cutoff))
summary(modelDS)

# quadratic 
modelQD <- lm(data = dataNL, y ~ D + I(x-cutoff) + I((x-cutoff)^2) + D:I(x-cutoff) + D:I((x-cutoff)^2))
summary(modelQD)

write.csv(dataNL, "test.csv")

# Exercises ------------------------------------------------------------------------------

# change some of the arguments of rdplot (https://rdrr.io/cran/rdrobust/man/rdplot.html)
# change DGP in some way 

  
  