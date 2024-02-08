*********************************************************************************
* Causal Inference Workshop 
* Anna Papp (ap3907@columbia.edu)
* Week 4 - IV and RDD, RDD using simulated data 
* Based on: https://mixtape.scunning.com/06-regression_discontinuity
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

**********************************************************************************
* Part 1 - Linear DGP ************************************************************

***** Generate Data *****  

* generate data
set obs `n'

* running variable 
generate x = rnormal(50, 25)
replace x = 0 if x < 0
drop if x >= 100

* treatment
generate D = cond(x > `cutoff', 1, 0)

* potential outcome WITHOUT treatment 
generate yp = cond(x < `cutoff', 0 * D + 1.4 * x + rnormal(0, 30), ///
    -(3 - 1.4) * `cutoff' + 0 * D + 3.0 * x + rnormal(0, 30))

* potential outcome WITH treatment 
generate y = yp + `LATE' * D

* center cutoff 
generate xCentered = x - `cutoff'

***** Plots *****  

* potential outcomes, which we normally cannot observe 
twoway (scatter yp xCentered if D == 0, mcolor(red) msize(tiny)) (lfit yp xCentered if D == 0, lcolor(red)) (scatter yp xCentered if D == 1, mcolor(blue) msize(tiny)) (lfit yp xCentered if D == 1, lcolor(blue)), legend(off) xline(0, lcolor(black) lpattern(dash)) xtitle("Test Score (X)") ytitle("Potential Outcome")

* observed outcomes 
twoway (scatter y xCentered if D == 0, mcolor(red) msize(tiny)) (lfit y xCentered if D == 0, lcolor(red)) (scatter y xCentered if D == 1, mcolor(blue) msize(tiny)) (lfit y xCentered if D == 1, lcolor(blue)), legend(off) xline(0, lcolor(black) lpattern(dash)) xtitle("Test Score (X)") ytitle("Observed Outcome (Y)")

* now using rdplot 
rdplot y x, c(`cutoff') binselect(es)

***** Regressions *****  

* same slopes 
regress y D c.xCentered, r

* different slopes 
regress y D c.xCentered c.xCentered#1.D, r

**********************************************************************************
* Part 2 - Non-linear DGP ********************************************************

***** Generate Data *****  

* generate data
gen x2 = x^2
gen x3 = x^3
gen yNL = 10000 - 300 * x + 6 * x2 + 0.00003 * x3 + rnormal(0, 1000)
gen xCentered2 = xCentered^2

***** Plots *****  

* observed outcome
twoway (scatter yNL xCentered if D == 0, mcolor(red) msize(tiny)) (lfit yNL xCentered if D == 0, lcolor(red)) (scatter yNL xCentered if D == 1, mcolor(blue) msize(tiny)) (lfit yNL xCentered if D == 1, lcolor(blue)), legend(off) xline(0, lcolor(black) lpattern(dash)) xtitle("Test Score (X)") ytitle("Observed Outcome (Y)")

* now using rdplot 
rdplot yNL x, c(`cutoff') binselect(es)

***** Regressions *****  
* Same slopes 
regress yNL D c.xCentered, r

* Different slopes 
regress yNL D c.xCentered c.xCentered#1.D, r

* Quadratic 
regress yNL D c.xCentered c.xCentered2 c.xCentered#1.D  c.xCentered2#1.D, r

*********************************************************************************
* Exercises
* Change some of the arguments of rdplot (help rdplot or http://fmwww.bc.edu/RePEc/bocode/r/rdplot.pdf)
* Change DGP in some way 
