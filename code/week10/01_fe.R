#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 10 - Fixed Effects
# Based on:
# last modified: 04/09/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

# clear variables in environment and script
rm(list=ls(all=TRUE)); cat("\014") 

# libraries 
library(dplyr)
library(ggplot2)

# random seed 
set.seed(123)

#### Example 1: Individual + city FE --------------------------------------------------------
# Data generation ---------------------------------------------------------------------------

# generate some data 
year <- c(2000:2023)
individuals <- c(1:200)

# each year x month combination 
data <- expand.grid(individuals = individuals, year = year, KEEP.OUT.ATTRS = FALSE)
data <- data %>% arrange(individuals, year) 

# generate random data on individuals 
individuals <- mutate(data.frame(individuals), 
                      city = sample(1:25, 200, replace = TRUE), 
                      moved = ifelse(runif(200) < 0.10, 1, 0), 
                      treatmentInd = rnorm(200), mean = 20, sd =2)

# for those that moved, pick a new city and year moved 
individuals <- individuals %>% mutate(newCity = ifelse(moved == 1, sample(1:25, 200, replace = TRUE), NA), 
                                      yearMoved = ifelse(moved == 1, sample(2000:2023, 200, replace = TRUE), NA))

# now match data with individuals 
data <- left_join(data, individuals, by = "individuals")

# replace city for those that moved after move 
data <- data %>% mutate(city = ifelse(moved == 1 & year >= yearMoved, newCity, city))

# keep only individual, city, year 
data <- data %>% dplyr::select(i = individuals, moved, city, year, treatmentInd)
rm(individuals)

# generate some random outcome 
data <- data %>% mutate(rand = rnorm(nrow(data), mean = 1, sd = 0.5))
data <- data %>% mutate(outcome = treatmentInd * 5 + city + rand * 2.5 + rnorm(nrow(data), mean = 0.25, sd = 0.1))

# Regression --------------------------------------------------------------------------------

# regression for those who didn't move 
model1a <- feols(data = data %>% filter(moved == 0), outcome ~ rand | i + city)
summary(model1a)

# same as:
model1b <-feols(data = data %>% filter(moved == 0), outcome ~ rand | i)
summary(model1b)

# regression for those who moved
model2a <- feols(data = data %>% filter(moved == 1), outcome ~ rand | i + city)
summary(model2a)

# NOT the same as:
model2b <- feols(data = data %>% filter(moved == 1), outcome ~ rand | i)
summary(model2b)


#### Example 2: Treatment dummy collinear with time FE --------------------------------------
# Data generation ---------------------------------------------------------------------------

# generate some data 
year <- c(1997:2002)
month <- c(1:12)
counties <- c(1:25)

# each year x month combination 
data <- expand.grid(year = year, month = month, county = counties, KEEP.OUT.ATTRS = FALSE)
data <- data %>% arrange(county, year, month)

# generate some random data, but add some linear trend and lower december outcomes, higher january outcomes 
data <- data %>% mutate(outcome = rnorm(nrow(data), mean = 100, sd = 10),
                        outcome = outcome + (year-1997)*1.5, 
                        outcome = ifelse(month == 12, outcome - 10, outcome), 
                        outcome = ifelse(month == 1, outcome + 10, outcome))

# generate treatment 
data <- data %>% mutate(treatment = ifelse(year >= 1999, 1, 0))

# transform to log 
data <- data %>% mutate(logOutcome = log(outcome))

# Regression --------------------------------------------------------------------------------

# regression without treatment variable 
model1 <- lm(data = data, formula = logOutcome ~  factor(year) + factor(month) + factor(county))
summary(model1)

# now add treatment variable
model2 <- lm(data = data, formula = logOutcome ~ treatment + factor(year) + factor(month) + factor(county))
summary(model2)

# now change order of variables 
model3 <- lm(data = data, formula = logOutcome ~ factor(year) + treatment + factor(month) + factor(county))
summary(model3)

# compare 
stargazer(model1, model2, model3,
          type="text", df=F,
          report=("vc*sp"),
          font.size="scriptsize",
          column.sep.width = c("10pt"),
          omit.stat=c("ser","adj.rsq"),
          digits=3,
          keep = c("treatment", "year"))

# now use feols - this gives an error! 
model <- feols(data = data, logOutcome ~  treatment | year + month + county)


#### Example 3: Treatment dummy collinear with unit FE --------------------------------------
# Data generation ---------------------------------------------------------------------------

# generate some data 
year <- c(1997:2002)
month <- c(1:12)
counties <- c(1:10)

# each year x month combination 
data <- expand.grid(year = year, month = month, county = counties, KEEP.OUT.ATTRS = FALSE)
data <- data %>% arrange(county, year, month)

# generate some random data, but add some linear trend and lower december outcomes, higher january outcomes 
data <- data %>% mutate(outcome = rnorm(nrow(data), mean = 100, sd = 10),
                        outcome = outcome + (year-1997) * 0.75, 
                        outcome = ifelse(county %in% c(2, 5, 9, 10), outcome - 10, outcome))

# generate treatment 
data <- data %>% mutate(treatment = ifelse(county %in% c(2, 5, 9,10), 1, 0))

# transform to log 
data <- data %>% mutate(logOutcome = log(outcome))

# Regression --------------------------------------------------------------------------------

# regression without treatment variable 
model1 <- lm(data = data, formula = logOutcome ~  factor(year) + factor(month) + factor(county))
summary(model1)

# now add treatment variable
model2 <- lm(data = data, formula = logOutcome ~ treatment + factor(year) + factor(month) + factor(county))
summary(model2)

# now change order of variables 
model3 <- lm(data = data, formula = logOutcome ~ factor(county) + treatment + factor(month) + factor(year))
summary(model3)

# compare 
stargazer(model1, model2, model3,
          type="text", df=F,
          report=("vc*sp"),
          font.size="scriptsize",
          column.sep.width = c("10pt"),
          omit.stat=c("ser","adj.rsq"),
          digits=3,
          keep = c("treatment", "county"))

# now use feols - this gives an error! 
model <- feols(data = data, logOutcome ~  treatment | year + month + county)

