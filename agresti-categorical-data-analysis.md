---
title: Catgorical Data Analysis - Agresti
date: 2015
author: Filippo Gambarota
---

# 4 - Generalized Linear Models

- GLM are made by:
	+ random component: that identify $y$ and its probability distribution
	+ systematic component: explanatory variables
	+ link function: specifies the function of $E(Y)$ that model equates to the linear predictor
	
- deviance of a GLM
	+ saturated model --> model with the maximum likelihood (not useful but used for comparison)
	
> logistic regression is a regression with a binomial random component and a logit link function

- <!-- EXTRA --> Equation 4.10 show the parametrization using the latent distribution finding the PSE and JND
- <!-- TODO --> view section 5.1

## 6.2.1 Residuals: Pearson, Deviance, and Standardized

- pearson residuals divide the raw residual by the estimated standard deviation for that value

## 6.3 Predictive Power ($R^2$)

### 6.3.2 Likelihood and Deviance Measures

$$
\frac{L_{M} - L_{0}}{L_{S} - L_{0}}
$$

Where $L_{M}$ is the likelihood for the current model, $L_{S}$ is the likelihood of the saturated model and $L_{0}$ is the likelihood of the null (only intercept) model.

### Classification Tables

- this is similar to Gelman error rate but used sensitivity and specificity i.e. binary accuracy (similar to Youden J)