hist(residuals(fit, type = "pearson"))
hist(residuals(fit, type = "deviance"))

ri <- residuals(fit, type = "response")

(ri / sqrt((1 - hatvalues(fit))))[1:5]

((residuals(fit, type = "pearson")[1:5]) / sqrt(1 - hatvalues(fit)))[1:5]

rstandard(fit)[1:5]
(residuals(fit, type = "pearson"))[1:5]
    
# raw residuals, logit scale
# equivalent to residuals(fit, type = "response")
# equivalent to dat$y - predict(fit, type = "response")
ri_l <- dat$y - fitted(fit)

# pearson residuals
# equivalent to residuals(fit, type = "pearson")
pi <- predict(fit, type = "response")
ri_pearson <- ri_l / sqrt(pi * (1 - pi))

# standardized pearson residuals
# equivalent to rstandard(fit, type = "pearson")
# An interesting property of Standardized Pearson Residuals is that they have an approximate Standard 
# Normal distribution if the model fits (Agresti, 2002)
h <- hatvalues(fit)
ri_pearson_std <- ri_pearson / sqrt(1 - h)

# deviance residuals
# equivalent to residuals(fit, type = "deviance")
ri_d <- sign(ri_l)*sqrt(-2*(y*log(pi) + (1 - dat$y)*log(1 - pi)))

# residual deviance
# deviance(fit)
sum(ri_d^2)


binned <- data.frame(performance::binned_residuals(fit, n_bins = 30))

