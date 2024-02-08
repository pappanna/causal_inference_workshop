# Causal Inference Workshop
Materials for Sustainable Development PhD Spring 2024 Causal Inference Workshop 

Based on Claire Palandri's 2022 [version](https://clairepalandri.github.io/docs/CIworkshop_syllabus.pdf) of the workshop

## Outline
- Week 1: Modeling assumptions
- Week 2: Potential outcomes framework (and DAGs, briefly)
- Week 3: IV and RDD (overview, DGP, assumptions, examples)
- Week 4: IV and RDD coding exercises 


## Coding examples and exercises 
- Week 1: What happens when errors are spatially correlated? Simulated exercise
- Week 4:
  - 01a_iv_simulated: IV exercise using simulated data (adjusting strength of instrument, exclusion restriction)
  - 01b_iv_card1995: IV exercise using published data (Card 1995), based on https://mixtape.scunning.com/07-instrumental_variables
  - 01c_rdd_simulated: RD exercise using simulated data (different functional forms of X, non-linear DGP, etc.), based on https://mixtape.scunning.com/07-instrumental_variables
  - 01d_rdd_carpenterdobkin2009: RD exercise using published data (Carpenter and Dobkin 2009), based on https://rpubs.com/phle/r_tutorial_regression_discontinuity_design


## Other helpful resources for causal inference and research design 
- [Causal Inference: The Mixtape](https://mixtape.scunning.com/) by Scott Cunningham (contains helpful coding examples in Stata, R, and Python)
- [The Effect: An Introduction to Research Design and Causality](https://theeffectbook.net/index.html) by Nick Huntington-Klein
