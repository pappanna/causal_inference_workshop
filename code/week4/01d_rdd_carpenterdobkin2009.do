*********************************************************************************
* Causal Inference Workshop 
* Anna Papp (ap3907@columbia.edu)
* Week 4 - IV and RDD, RD using published data (Carpenter and Dobkin 2009)
* Based on: https://rpubs.com/phle/r_tutorial_regression_discontinuity_design
* last modified: 02/08/24
*********************************************************************************

***** Setup ***** 

* setup 
clear all
macro drop _all
cd "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/"

* install rdrobust 
*ssc install rdrobust
* more info here: https://rdpackages.github.io/references/Calonico-Cattaneo-Farrell-Titiunik_2017_Stata.pdf

* set seed for reproducibility
set seed 123

* parameters 
local n = 1000 
local LATE = 35
local int = 25
local cutoff = 50

***** Load Data *****  

* Carpenter and Dobkin, 2009 data (from: https://github.com/jrnold/masteringmetrics/blob/master/masteringmetrics/data/mlda.rda)
* aggregated values according to respondents' ages 
import delimited "week4/data/carpenter_dobkin_2009.csv", clear

* formatting 
drop v1 
destring all internal external alcohol homicide suicide mva drugs externalother, replace force 

* scatterplot
scatter all agecell, xline(21) ytitle("Mortality rate (per 100,000)") xtitle("Age (binned)") ///
    title("Scatterplot of Mortality Rate by Age")

***** Regressions *****  

* subtract threshold 
gen threshold = 0
replace threshold = 1 if agecell >= 21 
gen agecell21 = agecell - 21
gen agecell21_2 = agecell21 * agecell21 

* same slope regression
regress all threshold c.agecell21, r

* different slope regression 
regress all threshold c.agecell21 c.agecell21#1.threshold, r

* quadratic regression
regress all threshold c.agecell21 c.agecell21_2 c.agecell21#1.threshold c.agecell21_2#1.threshold, r

***** Bandwidth *****  

* bandwidth selection
rdrobust all agecell, c(21)

* let's compare using this bandwidth 
keep if agecell >= 21-0.493 & agecell <= 21 + 0.493

* ccatterplot with adjusted bandwidth
scatter all agecell, xline(21) ytitle("Mortality rate (per 100,000)") xtitle("Age (binned)") ///
    title("Scatterplot of Mortality Rate by Age (Adjusted Bandwidth)")

* Different slope regression with adjusted bandwidth
regress all threshold c.agecell21 c.agecell21#1.threshold, r

*********************************************************************************
* Exercises
* do some sensitivity checks with the bandwidth and/or f()
