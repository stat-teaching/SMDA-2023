# Example with binary/discrete data

- non constant variance
- out of bound predictions
- we need an extensions of normal linear models

# Intro GLM

- general idea
- why using glms
- components
    - random part
    - systematic part
    - link function
   
# Recap about distributions

- normal
- binomial
- poisson

# Intro to data simulation [EXTRA]

- sample from probability distributions
- monte carlo simulations

# Binomial regression

## Example with binary data

- treatment effect
- exam and tv-shows

### Data exploration

- contingency tables
- OR, RR, probability differences
- identifiy the glm components

## Binomial GLM

- general approach
- how to do in R
- recovery parameters from the **data exploration** examples

## Models examples

- intercept only
- binary (B) or continous (C) predictor
- multiple regression BB, CC or BC
- interactions BB, CC, BB

### Simulate binary data [EXTRA]

- quick simulation for binary data from explained models

## Inference/Interpretation

- wald test
- model comparison
- parameters interpretation
    - divide by 4 rule
    - marginal effects
    - odds ratios
    - predicted probabilities
- ROC analysis [Extra]

## Diagnostic

- model assumptions (compared to lm)
- residual deviance
- residuals
- plots
    - raw, standardized, studentized, pearson, deviance, quantile
    - binned residuals
- classification accuracy
- R2
    - different type
    - some references
    
## Useful plots

- plotting predictions (e.g., sigmoid)
- plotting effects
    
## Dose P and inverse prediction [EXTRA]

- useful to find x given y (e.g., pychophysics or difficulty of questions given the probability of success)

## Probit and Signal Detection Theory [EXTRA]

- show an example why choosing a probit link function

# Poisson Regression

Same overall structure

## Overdispersion

## Peculiarities
 
