---
output: html_document
---


# Epipredictr





## Models
Three models are fit:
1. Simple windowed linear regression
2. BSTS with global trend
3. BSTS with local trend





## Model Fit

### Prior vs. Posterior Distributions

#### BSTS model global trend
![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

#### BSTS model local trend
![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
## Forecasts

### Table with forecasts

### Plots


![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## Scoring of Forecasts

```
##           mean        sd
## CRPS 0.5227991 0.5363262
## logs 2.9566401 3.6206817
```

```
##           mean        sd
## CRPS 0.2754406 0.2039380
## logs 0.6693461 0.7527948
```

```
##           mean        sd
## CRPS 0.3613821 0.3315153
## logs 0.8462064 0.7746215
```



