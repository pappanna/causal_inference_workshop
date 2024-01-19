*#############################################################################################
*# Causal Inference Workshop 
*# Anna Papp (ap3907@columbia.edu)
*# Week 1 - Modeling assumptions, regressions in Stata
*# last modified: 01/18/24
*#############################################################################################

cd "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/week1/"

import delimited "data/01a_data_sample.csv", clear 
drop v1

reg y x, vce(robust)

reg y x, cluster(countyid)

* conley standard errors: https://github.com/erikylewis/spatial_regression  
