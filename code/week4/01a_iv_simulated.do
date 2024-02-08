*********************************************************************************
* Causal Inference Workshop 
* Anna Papp (ap3907@columbia.edu)
* Week 4 - IV and RDD, IV using simulated data 
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

* parameters
local n = 1000
local cor = 0.50
local instrStrength = 0.25

***** Generate Data ***** 

* generate D* and error 
matrix define corrm = (1, `cor' \ `cor', 1)
corr2data Ds c, n(`n') means(10 5) sds(1 .5) corr(corrm)

* generate instrument 
drawnorm Z, n(`n')

* generate observed D
generate D = Ds + `instrStrength' * Z

* generate error 
generate e = c + rnormal(0, 0.5)

* generate outcome variable 
generate y = D + e

* check correlations 
correlate D e 
correlate Z e

***** Regressions *****

* OLS 
regress y D, r
estimates store OLS

* 2SLS manually
regress D Z, r
estimates store firstStage 
predict DHat, xb
rename D Dold 
rename DHat D 
regress y D, r
rename D DHat 
rename Dold D 
estimates store secondStage 

* before 
tempfile before 
save `before'

* bootstrap standard errors instead 
program bootstrap2SLS, rclass
	drop DHat
	regress D Z, r
	predict DHat, xb
	regress y DHat, r
	return scalar coefBS = _b[DHat]
 end

bootstrap r(coefBS), reps(`n') seed(`seed'): bootstrap2SLS

* restore original data 
use `before', clear 

* use IVREG 
ivregress 2sls y (D = Z), r
estimates store twoSLS
* can get first stage F-stat from here 
estat firststage 

* table
estout OLS secondStage twoSLS firstStage, ///
    cells(b(star fmt(3)) se(par)) ///
    legend label title("Simulated Data - OLS vs. IV") ///
    stats(r2 N, fmt(%9.0gc %9.0fc)) ///
	keep(D Z) ///
    varwidth(10) mlabels("OLS" "2SLS" "2SLS" "FS OLS") ///
    ///
    nonumbers 

*********************************************************************************
***** Exercises ***** 

* modify the strength of the instrument - what happens to the 2SLS estimates? 
* modify the correlation between D and e - what happens to the OSL vs. 2SLS estimates? 
* (bonus) modify the DGP to include another variable affected by the instrument that then affects the instrument (e.g., a mediating path from Z to Y) - how does this change estimates?
