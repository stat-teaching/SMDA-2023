# Introduction

- what you saw until now is a linear model --> some assumptions
- usually a linear model is used when the response variable y has some properties
- what about a variable that do no have these properties? --> e.g. binary data
- we need a method to still benefit from the linear model approach but being able to analyze different variables

- few examples:
    - number of errors --> counting
    - reaction times --> skewed, only positive
    - accuracy --> 0 and 1
    
- binomial vs binary data
    - more efficiency for large dataset
    
- fig. 14.1 Fox pp 397 very interesting for the difference between linear model and glm

- showing given the variance of the binomial distribution, as the $pi$ approaches 0 or 1 also the variance decrease --> no linear model assumption

- section 4.2.1 When Linear Models Are a Poor Choice of Dunn 2018
- section 5.5.1 Link Function of Dunn 2018

- "logistic regressions are nonlinear on the probability scale and linear on the logit scale. This is because logistic regression is linear in the parameters but nonlinear in the relation of inputs to outcome" pp 249 - Gelman Regression and other stories
- Binned residuals from Gelman Regression and other stories <!-- TODO check -->
- divide by 4 rule from Gelman Regression and other stories <!-- TODO check -->
- Overdispersion and underdispersion refer to data that show more or less variation than expected based on a fitted probability model from Gelman Regression and other stories 266
- Said another way, we can think of this standard deviation as a measure of the average distance each observation falls from its prediction from the model. Gelman Regression and other stories 
- There are a few things about R2 that you should know. First, it does not change if you multiply x or y in the regression by a constant. So, if you want to change the units of the predictors or response to aid interpretation, you won’t change the summary of the fit of the model to the data Gelman Regression and other stories 

## Divide by 4 rule

- https://stats.stackexchange.com/questions/415928/does-the-divide-by-4-rule-give-the-upper-bound-marginal-effect
- 

# Diagnostic

- standardize residuals --> the reason at generalized additive models pp 133
- forse fare un grafico che faccia capire response vs pearson residuals per vedere la differenza con la diagnostica precedente342

# Inference

- 9.9 When Wald Tests Fail pp 351 Dunn 2018

# Slides

- the linear model has some assumptions

- what about if the y variable 
