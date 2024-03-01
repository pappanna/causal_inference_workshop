#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 7 - Synthetic control and synthetic difference-in-differences
# last modified: 02/27/24

# Based on: https://cran.r-project.org/web/packages/tidysynth/readme/README.html 
# and https://synth-inference.github.io/synthdid/index.html
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## install packages
#devtools::install_github("synth-inference/synthdid")
#install.packages('tidysynth')

## load packages
library(tidysynth)
library(synthdid)
library(ggplot2)
library(dplyr)

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

##############################################################################################
# Part 1: Abadie et al. 2010 -----------------------------------------------------------------

# Load data ----------------------------------------------------------------------------------

# example data from Abadie et al. 2010 (California Prop 99)
data('smoking')

##############################################################################################
# tidysynth package -------------------------------------------------------------------------

# create synthetic control 
smokingSC <- smoking %>%
  # initial the synthetic control object
  synthetic_control(outcome = cigsale, # outcome
                    unit = state, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "California", # unit where the intervention occurred
                    i_time = 1988, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  # Generate the aggregate predictors used to fit the weights
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1980:1988,
                     ln_income = mean(lnincome, na.rm = T),
                     ret_price = mean(retprice, na.rm = T),
                     youth = mean(age15to24, na.rm = T)) %>%
  # average beer consumption in the donor pool from 1984 - 1988
  generate_predictor(time_window = 1984:1988,
                     beer_sales = mean(beer, na.rm = T)) %>%
  # Lagged cigarette sales 
  generate_predictor(time_window = 1975,
                     cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980,
                     cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988,
                     cigsale_1988 = cigsale) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1988, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()

# plot output 
smokingSC %>% plot_trends()

# plot weights
smokingSC %>% plot_weights()

##############################################################################################
# synthdid package ---------------------------------------------------------------------------

# Setup --------------------------------------------------------------------------------------

# need to have variable treated which is zero for untreated units and before treatment for treated unit
# then need to arrange variables: unit, year, outcome, treated dummy
dataSynthdid <- smoking %>% dplyr::select(state = state, year = year, packsPerCapita = cigsale)
dataSynthdid <- dataSynthdid %>% mutate(treated = ifelse(state == "California" & year >= 1988, 1, 0))

# this creates setup for synthdid 
setup <- panel.matrices(data.frame(dataSynthdid))

# Synthetic Diff-in-Diff ---------------------------------------------------------------------

synthDiD <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
seSynthDiD <- sqrt(vcov(synthDiD, method='placebo'))

sprintf('point estimate: %1.2f', synthDiD)
sprintf('95%% CI (%1.2f, %1.2f)', synthDiD - 1.96 * seSynthDiD, synthDiD + 1.96 * seSynthDiD)

# Diff-in-Diff -------------------------------------------------------------------------------

did <- did_estimate(setup$Y, setup$N0, setup$T0)
seDid <- sqrt(vcov(did, method='placebo'))

sprintf('point estimate: %1.2f', did)
sprintf('95%% CI (%1.2f, %1.2f)', did - 1.96 * seDid, did + 1.96 * seDid)


# Synthetic Control --------------------------------------------------------------------------

synth <- sc_estimate(setup$Y, setup$N0, setup$T0)
seSynth <- sqrt(vcov(synth, method='placebo'))

sprintf('point estimate: %1.2f', synth)
sprintf('95%% CI (%1.2f, %1.2f)', synth - 1.96 * seSynth, synth + 1.96 * seSynth)

# Graphs -------------------------------------------------------------------------------------

# combine estimates 
estimates <- list(did, synth, synthDiD)

# plot estimates 
synthdid_plot(estimates, facet.vertical=FALSE,
              control.name='control', treated.name='california',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.26,.07), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank())

# plot weights 
synthdid_units_plot(estimates, se.method='none') +
  theme(legend.background=element_blank(), legend.title = element_blank(),
        legend.direction='horizontal', legend.position=c(.17,.07),
        strip.background=element_blank(), strip.text.x = element_blank())
