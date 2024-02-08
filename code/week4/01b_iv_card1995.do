*********************************************************************************
* Causal Inference Workshop 
* Anna Papp (ap3907@columbia.edu)
* Week 4 - IV and RDD, IV using published data (Card 1995)
* Based on: https://mixtape.scunning.com/07-instrumental_variables
* last modified: 02/08/24
*********************************************************************************

***** Setup ***** 

* setup 
clear all
macro drop _all
cd "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/"

* install ivreg2 for ivregress if you don't have it installed 
*ssc install ivreg2

* set seed for reproducibility
set seed 123

***** Download data and summary stats ***** 

* download data
use "https://github.com/scunning1975/mixtape/raw/master/card.dta", clear

* keep relevant variables; educ (education, endogenous), control variables, nearc4 (college in county, instrument)
keep lwage educ exper black south married smsa nearc4 

* summary stats
summarize

***** Regressions ***** 

* OLS 
regress lwage educ exper black south married smsa, r 
estimates store OLS 

* 2SLS manually,  (CAUTION: these SEs are NOT correct, just for illustration)
* note, same control variables in two stages 
regress educ nearc4 exper black south married smsa
estimates store firstStage
predict educHat, xb
* CAUTION: renaming for table, NOT good idea otherwise!!
rename educ educOld 
rename educHat educ 
regress lwage educ exper black south married smsa
estimates store secondStage 
rename educ educHat 
rename educOld educ 

* before 
tempfile before 
save `before'

* bootstrap standard errors instead 
program bootstrap2SLS, rclass
	drop educHat
	regress educ nearc4 exper black south married smsa, r
	predict educHat, xb
	regress lwage educHat exper black south married smsa, r
	return scalar coefBS = _b[educHat]
 end

bootstrap r(coefBS), reps(1000) seed(`seed'): bootstrap2SLS

* restore original data 
use `before', clear 

* use IVREG 
ivregress 2sls lwage (educ = nearc4) exper black south married smsa
estimates store twoSLS
estat firststage 

* table
estout OLS secondStage twoSLS firstStage, ///
    cells(b(star fmt(3)) se(par)) ///
    legend label title("OLS vs. IV - Card (1995)") ///
    stats(r2 N, fmt(%9.0gc %9.0fc)) ///
	keep(educ nearc4) /// 
    varwidth(10) mlabels("OLS" "2SLS" "2SLS" "FS OLS") ///
    ///
    nonumbers 
