#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 9 - Randomization inference simple example 
# Based on: https://www.alexstephenson.me/post/randomization-inference-a-simple-example/
# last modified: 03/28/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

# clear variables in environment and script
rm(list=ls(all=TRUE)); cat("\014") 

# libraries 
library(dplyr)
library(ggplot2)
library(readr)

# set random seed 
set.seed(12345)

# Generate Data -------------------------------------------------------------

# generate some data 
data <- data.frame(id = c(1:50),
                   y = round(rnorm(50, 0, 10)),
                   treat = sample(0:1, 50, replace = T))

# potential outcomes 
data <- data %>% mutate(y_i1 = ifelse(treat == 1, y, NA),
                        y_i0 = ifelse(treat == 0, y, NA))

# Sharp Null ----------------------------------------------------------------

# fill in potential outcomes for the sharp null (for illustration)
dataRandInf <- data %>% mutate(y_i1 = ifelse(is.na(y_i1), y_i0, y_i1),
                               y_i0 = ifelse(is.na(y_i0), y_i1, y_i0))


# ATE -----------------------------------------------------------------------

# calculate ATE 
ATE <- mean(dataRandInf$y[dataRandInf$treat == 1]) - mean(dataRandInf$y[dataRandInf$treat == 0])
print(ATE)

# (or regression)
summary(lm(y ~ treat, data = dataRandInf))

# Randomization Inference ---------------------------------------------------

# number of draws 
n <- 10000

# generate possible treatments
possibleTreat <-  matrix(NA, n, 50)
for(i in 1:n){
  possibleTreat[i,] <- sample(dataRandInf$treat, 50, replace = T)
}

# now calculate ATE for each 
possibleATE <- NA 
for(i in 1:n){
  
  # calculate mean of treated and not treated for each random assignment 
  meanTreat <- mean(dataRandInf$y[possibleTreat[i,]== 1])
  meanNotTreat <- mean(dataRandInf$y[possibleTreat[i,]== 0])
  
  # calculate ATE for each 
  possibleATE[i] <- meanTreat - meanNotTreat
  
}

# plot the distribution of possible ATEs
ggplot(as.data.frame(possibleATE), aes(x = possibleATE))+
  geom_histogram(aes(y=..density..), binwidth = 1)+
  geom_vline(xintercept = ATE, color = "red", size = 1)+
  theme_minimal()+
  xlab("Randomized Average Treatment Effects")+
  ylab("Density")+
  ggtitle("Randomization Inference Example")

# calculate p-value 
sum(possibleATE > ATE)/length(possibleATE)


