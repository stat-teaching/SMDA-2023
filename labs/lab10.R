#' ---
#' title: "Statistical Methods and Data Analysis in Developmental Psychology"
#' subtitle: "Lab 10"
#' author: "Filippo Gambarota"
#' output: 
#'     html_document:
#'         code_folding: show
#'         toc: true
#'         toc_float: true
#'         code_download: true
#' date: "Updated on `r Sys.Date()`"
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      dev = "svg")

#' 
## ----packages, message=FALSE, warning=FALSE---------------------------------------------------------------
devtools::load_all() # if using the rproject dowloaded from the slides
# source("utils-glm.R") # if using a standard setup
library(here)
library(tidyr) # for data manipulation
library(dplyr) # for data manipulation
library(ggplot2) # plotting
library(car) # general utilities
library(effects) # for extracting and plotting effects 
library(emmeans) # for marginal effects

#' 
## ----options, include = FALSE-----------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 15))

#' 
## ----script, echo=FALSE-----------------------------------------------------------------------------------
## Link in Github repo
downloadthis::download_link(
  link = download_link("https://github.com/stat-teaching/SMDA-2023/blob/master/labs/lab10.R"),
  button_label = "Download the R script",
  button_type = "danger",
  has_icon = TRUE,
  icon = "fa fa-save",
  self_contained = FALSE
)

#' 
## ----loading-data-----------------------------------------------------------------------------------------
dat <- read.csv(here("data", "tantrums.csv"))

#' 
#' # Overview
#' 
#' The dataset `tantrums.csv` is about the number of tantrums of `nrow(child)` toddlers during two days at the nursery. The columns are:
#' 
#' - `id`: identifier for the child
#' - `temperament`: the temperament of the child as "easy" or "difficult"
#' - `attachment`: the attachment of the child as "secure" or "insecure"
#' - `parent_se`: an average self-esteem value of the parents (self report)
#' - `parent_skills`: a score representing the teacher judgment about parenting skills
#' - `tantrums`: the number of tantrums
#' 
#' We want to predict the number of tantrums as a function of these predictors.
#' 
#' 1. Importing data and check
#'     - in the presence of `NA`, remove the children
#'     - convert to factors the categorical variable with "difficult" and "insecure" as reference values
#' 2. Exploratory data analysis
#' 3. Model fitting with `glm()`
#' 4. Diagnostic
#' 5. Interpreting parameters
#' 6. Model selection
#' 
#' # 1. Importing data and check
#' 
#' Firstly we import the data into R:
#' 
## ----eval = FALSE-----------------------------------------------------------------------------------------
## dat <- read.csv("data/tantrums.csv")

#' 
#' Check the structure:
#' 
## ---------------------------------------------------------------------------------------------------------
str(dat)

#' 
#' Check for `NA`:
#' 
## ---------------------------------------------------------------------------------------------------------
sapply(dat, function(x) sum(is.na(x)))

#' 
#' So we have some `NA` values. We managed them according to the instructions:
#' 
## ---------------------------------------------------------------------------------------------------------
dat <- dat[complete.cases(dat), ]
dat$id <- 1:nrow(dat) # restore the id
rownames(dat) <- NULL

#' 
#' Let's convert the categorical variables into factor with the appropriate reference level:
#' 
## ---------------------------------------------------------------------------------------------------------
dat$temperament <- factor(dat$temperament, levels = c("difficult", "easy"))
dat$temperament[1:5]

dat$attachment <- factor(dat$attachment, levels = c("insecure", "secure"))
dat$attachment[1:5]

#' 
#' 
#' 
#' 
#' 
#' 
#' # 2. Exploratory data analysis
#' 
#' Let's compute some summary statistics and plots.
#' 
## ---------------------------------------------------------------------------------------------------------
summary(dat)

#' 
## ---------------------------------------------------------------------------------------------------------
table(dat$temperament)
table(dat$attachment)
table(dat$attachment, dat$temperament)

#' 
## ---------------------------------------------------------------------------------------------------------
par(mfrow = c(1,3))
hist(dat$parent_se)
hist(dat$parent_skills)
hist(dat$tantrum)

#' 
#' Let's compute some bivariate relationships:
#' 
## ---------------------------------------------------------------------------------------------------------
plot(dat$parent_se, dat$tantrum, pch = 19)
plot(dat$parent_skills, dat$tantrum, pch = 19)

#' 
## ---------------------------------------------------------------------------------------------------------
boxplot(tantrum ~ temperament, data = dat)
boxplot(tantrum ~ attachment, data = dat)

#' 
#' # 3. Model fitting with `glm()`
#' 
#' We can start by fitting our null model with the `poisson()` family:
#' 
## ---------------------------------------------------------------------------------------------------------
fit0 <- glm(tantrum ~ 1, family = poisson(link = "log"), data = dat)

#' 
#' What is the intercept here?
#' 
#' Then we can fit a model with the attachment effect:
#' 
## ---------------------------------------------------------------------------------------------------------
fit1 <- glm(tantrum ~ parent_se, family = poisson(link = "log"), data = dat)
summary(fit1)

#' 
#' What about the overdispersion? What could be the reason?
#' 
#' Assuming that the `attachment` is the only variable that we have, we could estimate the degree of overdispersion:
#' 
## ---------------------------------------------------------------------------------------------------------
sum(residuals(fit1, type = "pearson")^2)/fit1$df.residual
performance::check_overdispersion(fit1)

#' 
#' Let's have a look also at the residual plot:
#' 
## ---------------------------------------------------------------------------------------------------------
plot_resid(fit1, type = "pearson")

# or in base R
residualPlots(fit1)

#' 
#' There is clear evidence of overdispersion. But we have several other variables so before using another model let's fit everything:
#' 
## ---------------------------------------------------------------------------------------------------------
fit_s <- glm(tantrum ~ attachment + temperament + parent_se + parent_skills, family = poisson(link = "log"), data = dat)
summary(fit_s)

#' 
#' Let's check again overdispersion and pearson residuals:
#' 
## ---------------------------------------------------------------------------------------------------------
plot_resid(fit_s, type = "pearson")

# or in base R
residualPlots(fit_s)

#' 
#' The majority of the distribution seems ok, but there are some values with very high residuals and the overdispersion is still present:
#' 
## ---------------------------------------------------------------------------------------------------------
sum(residuals(fit_s, type = "pearson")^2)/fit_s$df.residual
performance::check_overdispersion(fit_s)

#' 
#' # 4. Diagnostic
#' 
#' Another reason for overdispersion could be the presence of outliers and influential points. Let's have a look at the Cook distances:
#' 
## ---------------------------------------------------------------------------------------------------------
car::influenceIndexPlot(fit_s, vars = c("cook", "hat", "Studentized"))

#' 
#' There are two values (111 and 112) with a very high cook distance and very high studentized residual. We can try to fit a model without these values and check what happens to the model:
#' 
## ---------------------------------------------------------------------------------------------------------
dat_no_out <- dat[-c(111, 112), ]
fit_no_out <- glm(tantrum ~ attachment + temperament + parent_se + parent_skills, family = poisson(link = "log"), data = dat_no_out)
summary(fit_no_out)

#' 
#' The model seems to be clearly improved, especially in terms of overdispersion:
#' 
## ---------------------------------------------------------------------------------------------------------
sum(residuals(fit_no_out, type = "pearson")^2)/fit_no_out$df.residual
performance::check_overdispersion(fit_no_out)

#' 
#' We can also compare the two models in terms of coefficients:
#' 
## ---------------------------------------------------------------------------------------------------------
car::compareCoefs(fit_s, fit_no_out)

#' 
#' In fact, there are some coefficients with different values. We can check also the dfbeta plots:
#' 
## ---------------------------------------------------------------------------------------------------------
dfbeta_plot(fit_s)

#' 
#' The previous observations seems to do not affect the estimated parameters but they impact the overall model fit, deviance and residuals.
#' 
#' Let's have a look at residuals now:
#' 
## ---------------------------------------------------------------------------------------------------------
car::residualPlot(fit_no_out, type = "rstandard")

#' 
#' There is still some strange pattern but the majority of the distribution seems to be between -1 and 1.
#' 
#' # 5. Interpreting parameters
#' 
#' Before anything else, just plot the effects:
#' 
## ---- fig.width=10, fig.height=10-------------------------------------------------------------------------
plot(allEffects(fit_no_out))

#' 
#' 
#' Now we can interpret model parameters:
#' 
## ---------------------------------------------------------------------------------------------------------
summary(fit_no_out)

#' The `(Intercept)` is the expected number of tantrums for "insecure", "difficult" children where parent_skills are rated as 0 and parent self esteem is 0, thus `r exp(coef(fit_no_out)[1])`. Similarly to the binomial lab, we could center the two numerical variables to have a more meaningful interpretation or we can use the `predict` function to obtain the values that we want.
#' 
## ---------------------------------------------------------------------------------------------------------
predict(fit_no_out, newdata = data.frame(attachment = "insecure", 
                                         temperament = "difficult",
                                         parent_se = mean(dat$parent_se), 
                                         parent_skills = mean(dat$parent_skills)),
        type = "response") # same as exp(prediction)

#' 
#' The `attachmentsecure` is the expected difference in log number of tantrums between `secure - insecure` attachment, controlling for other variables:
#' 
## ---------------------------------------------------------------------------------------------------------
emmeans(fit_no_out, pairwise~attachment)

#' 
#' In terms of the response scale, we can intepret it as the multiplicative increase of the number of tantrums from secure to insecure attachment:
#' 
## ---------------------------------------------------------------------------------------------------------
exp(coef(fit_no_out)["attachmentsecure"])

#' 
#' Moving from insecure from secure attachment, there is a decrease in the expected number of tantrums of `r 100 - exp(coef(fit_no_out)["attachmentsecure"]) * 100` %.
#' 
#' The `temperamenteasy` can be interpreted in the same way:
#' 
## ---------------------------------------------------------------------------------------------------------
emmeans(fit_no_out, pairwise~temperament)

#' 
## ---------------------------------------------------------------------------------------------------------
exp(coef(fit_no_out)["temperamenteasy"])

#' 
#' So there is a reduction of the `r 100 - exp(coef(fit_no_out)["temperamenteasy"]) * 100` % by moving from difficult to easy temperament.
#' 
#' `parent_se` and `parent_skills` are interpreted similarly. The coefficient represent the increase/decrease in the log number of tantrums for a unit increase in the predictors.
#' 
## ---------------------------------------------------------------------------------------------------------
exp(coef(fit_no_out)[4:5])

#' 
#' So the number of tantrums seems to be unaffected by the parents self-esteem but as the parent skills increases there is a reduction in the number of tantrums.
#' 
#' # 6. Model selection
#' 
#' Let's compare the model with and without the `parent_se` terms that appear to be not very useful:
#' 
## ---------------------------------------------------------------------------------------------------------
fit_no_parent_se <- update(fit_no_out, . ~ . -parent_se)
summary(fit_no_parent_se)
anova(fit_no_parent_se, fit_no_out, test = "LRT")

#' 
## ---------------------------------------------------------------------------------------------------------
drop1(fit_no_out, test = "LRT")

#' 
#' Or using the `MuMIn::dredge()` function:
#' 
## ---------------------------------------------------------------------------------------------------------
fit_no_out <- update(fit_no_out, na.action = na.fail)
MuMIn::dredge(fit_no_out, rank = "AIC")

#' 
