# Epipredictr Workplan 

## Models and Model Fit
  - [x] linear regression 
  - [x] bsts, global trend
  - [x] bsts local trend
    
## Graphical Model Assessment
  - [x] plot with predictions against true values
  - [ ] priors vs posterior plots
  
## Analytical Model Assessment and Scoring Rules
  - [ ] CRPS (use scoringRules package, should be straight-forward
  - [ ] Maybe LogS. Less straight forward and probably inferior, but also implemented in scoringRules
  
  
Popular examples of proper scoring rules for Ω = R include the logarithmic score and the continuous ranked probability score. The logarithmic score (LogS;Good 1952) is defined as LogS(F, y) = −log(f(y)),
  
#### Literature on that: 
The packages caret(Kuhnet al.2018) and forecast(Hyndman and Khandakar 2008) provide cross-validation tools suitable for cross-sectional and time series data, respectively. Theloo(Vehtariet al.2018) package implementsrecent proposals to select among Bayesian models. The ensembleBMA(Fraleyet al.2018) and ensembleMOS(Yuenet al.2018) packages contain formulas for the CRPS of a small subset of the distributions listed in Table1whichare relevant for post-processing ensemble weather forecasts (Fraleyet al.2011), and can onlybe applied to specific data structures utilized in the packages. The surveillance(Meyeret al.2017) package provides functions to compute the logarithmic score and other scoring rules forcount data models in epidemiology. The scoring(Merkle and Steyvers 2013) package focuses on discrete (categorical) outcomes,for which it offers a large number of proper scoring rules.



### (Maybe future extensions)
  - autoregressive time model
  - semi-mechanistic model
  - maybe machine learning
  - maybe deep learning

