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

# Altoè suggestions

sdtandardized residuals
deviance residuals

fox

fitted --> 01 

distanze di cook
leverage

cook.distance --> numero di soggetti/parametri

la logica del binned residual è di accorpare un gruppo di punti e calcolare il valore predetto medio, il valore vero medio (0,1) e poi rappresentare la loro differenza
se i valori sono molto distanti dalla linea dello 0 significa che in quella condizione il modello sta funzionando meno bene


raw residuals = resid(type.residuals = "response")

# Residuals (dunn 2018)

## response residuals

observed (01) - fitted (probability)

## pearson residuals

- divide out the effect of non-constant variance (in glm mean and variance are often correlated)

## deviance residuals

- similar to pearson

## quantile residuals



# glm general (dunn 2018)

- random component --> Random component: The observations yi come independently from a specified edm such that yi ∼ edm(μi, φ/wi) for i = 1, 2,...,n.
- systematic component --> linear predictor linked to the mean with a link function with eventually an offset (usually no estimation)

# inference (dunn 2018)

- wald test
- confidence intervals, first on the link scale then lower and upper with the inverse link function
- likelihood ratio test
- non nested models, AIC and BIC

# goodness of fit

- current model vs saturated model
    + the saturated model fitted values equal the data values (no residuals)
    
# assupmtions check

- linear component:
    + residuals against fitted
    + residuals against xs
- random component
    + qq plot with quantile residuals
    + studentized residuals: his suggests omitting Observation i from the calculation of s2 when computing the residual for Observation i. These residuals
are called Studentized residuals.
    
# outliers and influential observation

- standardized/studentized or quantile residuals

# 9.6 Median Effective Dose, ED50 (psychophysics)

- dose.p, inverse prediction

# overdispersion

- when the model as more standard error that was could be espected by the glm

# diagnostic (`influence.measures()`)

- cook distance
    + D exceed the 50th percentile of the F distribution with F(p, n - p)
- dffits: how much a fitted value for observation $i$ changes between the model fitted with all the data and the model fitted when $i$ is omitted --> equal the cook distance when is squared
    + abs(DFFITS) > 3 / sqrt(p / (n - p))
- dfbeta --> coefficient specific
    + abs(DFBETA) > 1
- covariance ratio --> increase in uncertainty when an observation is omitted. More simply, the square root of cr can be interpreted as the average factor by which the confidence intervals for the regression coefficients become wider when Observation i is omitted. (`covratio()`)
    + CR > 3p / (n - p)
- leverages h <!-- TODO check this -->
    + h > 3p/n
    + the influence that an observed value has on its prediction from the model
    + https://online.stat.psu.edu/stat501/lesson/11/11.2
    

# residuals

- standardizing --> dividing by s under a normal distribution
- studentizing --> dividing by s under a t distribution

# transforming the y

- transoforming the y
    + e.g., logarithm
- variance stabilyzing functions
    + used to reduce or eliminate the mean-variance relationship
    + to have constant variance
    

# outliers

The first step in dealing with outliers is to try to identify their cause. This will lead to one of following conclusions:

- The observation is a known mistake. For example, too much herbicide
was accidentally used, the operator made a mistake using the machine,
or the observation was simply mis-recorded.
- The observation is known to come from a different population. For example, in an analysis of hospital admission rates, the outlier turns out on
closer examination to correspond to a hospital much larger than others
in the study.
- There is no known reason for why the observation might be an outlier.

# collinearity

- Collinearity, sometimes called multicollinearity, occurs when some of the covariates are highly correlated with each other, implying that they measure
almost the same information
    + correlation between predictors
    

# books

- https://link.springer.com/book/10.1007/978-0-387-87458-6#toc
- "file:///C:/Users/user/Google%20Drive/_PhD/Library/Statistics/Agresti%202015%20-%20Categorical%20Data%20Analysis.pdf"
# slides

- https://statmath.wu.ac.at/courses/heather_turner/glmCourse_001.pdf