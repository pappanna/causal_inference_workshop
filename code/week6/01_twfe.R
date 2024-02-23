#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 6 - TWFE
# last modified: 02/22/24

# Based on Goodman-Bacon (2021), Sun and Abraham (2020), Callaway and Sant'Anna (2021), Borusyak et al. (2021)
# Other coding sources: 
# https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
# https://cran.r-project.org/web/packages/bacondecomp/vignettes/bacon.html
# https://causalinf.substack.com/p/callaway-and-santanna-dd-estimator
# https://rdrr.io/cran/didimputation/man/did_imputation.html
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## TWFE libraries 
# bacondecomp: Bacon decomposition package
# did: Callaway and Sant'anna package
# didimputation: Borusyak et al package 
# DIDmultiplegt: de Chaisemartin and D'Haultfoeuille package 
# fixest, ~sunab() Sun and Abraham event study 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, dplyr, tidyr, ggplot2, bacondecomp, fixest, did, didimputation, DIDmultiplegt)    

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

# Load data ---------------------------------------------------------------------------------

## file name 
# Stevenson and Wolfers (2006): stevensonwolfers2006
# Cheng and Hoekstra (2013): chenghoekstra2013
filename <- "stevensonwolfers2006"
#filename <- "chenghoekstra2013"

## load file 
data <- read.csv(file=paste("week6/data/", filename, ".csv", sep=""))
data <- data.frame(data)

## variable names ####
# for Stevenson and Wolfers 2006:
outcomevar <- "asmrs"
postvar <- "post"
unitvar <- "stfips"
timevar <- "year"
timetreatvar <- "X_nfd"

## for Cheng and Hoekstra 2013:
#outcomevar <- "l_homicide"
#postvar <- "post"
#unitvar <- "sid"
#timevar <- "year"
#timetreatvar <- "effyear"

# 01 - TWFE ---------------------------------------------------------------------------------

# twfe model 
regTWFE <- lm(data = data, asmrs ~ post + factor(stfips) + factor(year))

# save coefficients 
coefTWFE <- as.numeric(regTWFE$coefficients[2])
seTWFE <- as.numeric(sqrt(diag(vcov(regTWFE)))[2])
print(paste("Two-way FE estimate =", round(coefTWFE, 4)))

# table
stargazer(regTWFE, 
          type="text", df=F, report=("vc*sp"), model.names = FALSE,
          keep = "post", 
          dep.var.labels = c("Suicides per Million Women"), 
          covariate.labels = c("Post"),
          title = "TWFE Estimator (Stevenson and Wolfers 2006)")

rm(regTWFE)

# 02 - Event Study (TWFE) -------------------------------------------------------------------

# we need an indicator of which states received treatment
data <- data %>% mutate(treat = ifelse(is.na(X_nfd), 0, 1))

# now create a "time_to_treatment" variable for each state; for never-treated units set to 0
data <- data %>% mutate(time_to_treat = ifelse(treat == 1, year - X_nfd, 0))

# run fe ols 
regES <- feols(data = data, asmrs ~ i(time_to_treat, treat, ref = -1) | stfips + year, cluster = ~ stfips)

# plot TWFE event study
iplot(regES, 
      xlab = 'Time to Treatment',
      main = 'Event Study: Differential Timing of Treatment (TWFE)')

rm(regES)

# 03 - Bacon Decomposition ------------------------------------------------------------------

# run bacon decomp 
dfBacon <- bacon(asmrs ~ post,
                data = data,
                id_var = 'stfips',
                time_var = 'year')

# can check that this is equal to TWFE estimate
coefBacon <- sum(dfBacon$estimate * dfBacon$weight)
print(paste("Weighted sum of decomposition =", round(coefBacon, 4)))

# plot decomposition 
ggplot(dfBacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point() + 
  ggtitle("Bacon Decomposition") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# NOTE: 
# can also use https://cran.r-project.org/web/packages/TwoWayFEWeights/index.html for calculating weights 

rm(dfBacon)

# 04 - Event Study, Sun and Abraham ---------------------------------------------------------

# give never-treated units a fake "treatment" date far outside the relevant study period
data <- data %>% mutate(year_treated = ifelse(treat == 0, 10000, X_nfd))

# now run Sun and Abraham method model 
regSA <- feols(data = data, asmrs ~ sunab(year_treated, year) | stfips + year, cluster = ~ stfips)
summary(regSA)

# get coefficients in table (could add plot)
coefSATable <- data.frame(as.matrix(regSA$coeftable)) %>% mutate(time = ifelse(row_number() < 21, row_number()-22, row_number()-21)) %>% dplyr::select(time, estimate = Estimate, se = Std..Error)
coefSATable <- rbind(coefSATable, c(0, 0, 0))
row.names(coefSATable) <- NULL
coefSATable <- coefSATable %>% arrange(time)
rm(regSA)

# 05 - Callaway and Sant'anna ---------------------------------------------------------------

# set untreated units to have effective year of 0
data <- data %>% mutate(timetreatvar = ifelse(is.na(X_nfd), 0, X_nfd))

# now with not yet treated 
regCS <- att_gt(yname = "asmrs", # LHS variable
               tname = "year", # time variable
               idname = "stfips", # id variable
               gname = "timetreatvar", # first treatment period variable
               data = data, # data
               xformla = NULL, # no covariates
               #xformla = ~ , # with covariates
               est_method = "drimp", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "stfips", # cluster level
               panel = TRUE)  # whether the data is panel or repeated cross-sectional

# aggregate ATT, across all years 
regCSAgg <- aggte(regCS, type = "group")
coefCS <- regCSAgg$overall.att
seCS <- regCSAgg$overall.se
rm(regCS, regCSAgg)
  
# event-study version
#agg_effects_es_notyet <- aggte(atts_notyet, type = "dynamic")
#coefCSTable  <- cbind.data.frame(estimate = agg_effects_es_notyet$att.egt,
#                              se = agg_effects_es_notyet$se.egt,
#                              time = agg_effects_es_notyet$egt + 1)
#coefCSTable <- coefCSTable %>% dplyr::select(time, estimate, se)
  
# 06 - Borusyak et al. ----------------------------------------------------------------------

# estimate borusyak (overall)
regBor <- did_imputation(data = data, yname = "asmrs", gname = "X_nfd", tname = "year", idname = "stfips", cluster_var = "stfips")
coefBorusyak <- regBor$estimate
seBorusyak <- regBor$std.error
rm(regBor)

# event study version, set horizon = T
#regBorES <- did_imputation(data = data, yname = "asmrs", gname = "X_nfd", tname = "year", idname = "stfips", cluster_var = "stfips", horizon = TRUE)
#coefBYTable <- regBorES %>% dplyr::select(term, estimate, se=std.error)

# 07 - de Chaisemartin and D'Haultfoeuille --------------------------------------------------

# estimate de Chaisemartin and D'Haultfoeuille
# note that to get SEs, have to bootstrap (which takes a while if you set brep to be high)
regDeChais <- did_multiplegt(df = data, Y = "asmrs", G = "stfips", T = "year", D = "post", cluster = "stfips", brep = 10, parallel = TRUE)
coefDeChais <- regDeChais$effect 
seDeChais <- regDeChais$se_effect 
rm(regDeChais)

# 08 - Compare estimates ---------------------------------------------------------------------

coefs <- data.frame(rbind(c("Callaway and Sant'anna", coefCS, seCS), 
                          c("Borusyak et al.", coefBorusyak, seBorusyak), 
                          c("de Chaisemartin and D'Haultfoeuille", coefDeChais, seDeChais)))
colnames(coefs) <- c("estimator", "estimate", "se")

