# Causal Inference Workshop
Materials for Sustainable Development PhD Spring 2024 Causal Inference Workshop 

Based on Claire Palandri's 2022 [version](https://clairepalandri.github.io/docs/CIworkshop_syllabus.pdf) of the workshop

## Outline
- Week 1: Modeling assumptions
- Week 2: Potential outcomes framework (and DAGs, briefly)
- Week 3: IV and RDD (overview, DGP, assumptions, examples)
- Week 4: IV and RDD coding exercises and SDev examples
- Week 5: Event study, DiD, DiDiD, and the (potential) problem with TWFEs
- Week 6: New TWFE literature, DiD/TWFE coding exercises
- Week 7: Synthetic Control and Synthetic DiD
- Week 8: Pre-estimation, estimation, and post-estimation steps
- Week 9: Inference 

## Coding examples and exercises (in R and Stata)
- Week 1: What happens when errors are spatially correlated? Simulated exercise
- Week 4:
  - __01a_iv_simulated__: IV exercise using simulated data (adjusting strength of instrument, exclusion restriction)
  - __01b_iv_card1995__: IV exercise using published data (Card 1995), based on [this](https://mixtape.scunning.com/07-instrumental_variables)
  - __01c_rdd_simulated__: RD exercise using simulated data (different functional forms of X, non-linear DGP, etc.), based on [this](https://mixtape.scunning.com/07-instrumental_variables)
  - __01d_rdd_carpenterdobkin2009__: RD exercise using published data (Carpenter and Dobkin 2009), based on [this](https://rpubs.com/phle/r_tutorial_regression_discontinuity_design)
- Week 6:
  - __01_twfe__: Implements various new TWFE estimators in R 
- Week 7: 
  - __01_synthdid__: Implements synthetic control and synthetic DiD examples in R
- Week 9:
  - __01_rand_inf__: Simple randomization inference example, based on [this](https://www.alexstephenson.me/post/randomization-inference-a-simple-example/)

## Other helpful resources for causal inference and research design 
- [Causal Inference: The Mixtape](https://mixtape.scunning.com/) by Scott Cunningham (contains helpful coding examples in Stata, R, and Python)
- [The Effect: An Introduction to Research Design and Causality](https://theeffectbook.net/index.html) by Nick Huntington-Klein
- Paul Goldsmith-Pinkham's [Yale Applied Empirical Methods PhD Course repo](https://github.com/paulgp/applied-methods-phd)
- Kirill Borusyak's [Berkeley ARE PhD Applied Econometrics](https://github.com/borusyak/are213)
- TWFE/DiD with heterogeneous treatment effects resources:
  - Helpful starting point: de Chaisemartin and D'Haultfoeuille [review/survey](https://www.nber.org/papers/w29691)
  - [YouTube](https://www.youtube.com/playlist?list=PLVObvb_htcuBt8mV9yNagt7hK9FL5KXeE) DiD reading group videos (very helpful!)
  - Jonathan Roth's [DiD resources page](https://www.jonathandroth.com/did-resources/)
    
