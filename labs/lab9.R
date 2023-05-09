#' ---
#' title: "Statistical Methods and Data Analysis in Developmental Psychology"
#' subtitle: "Lab 9"
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
## ----setup, include=FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      dev = "svg")

#' 
## ----packages, message=FALSE, warning=FALSE------------------------------------------------------------
devtools::load_all() # if using the rproject dowloaded from the slides
# source("utils-glm.R") # if using a standard setup
library(here)
library(tidyr) # for data manipulation
library(dplyr) # for data manipulation
library(ggplot2) # plotting
library(car) # general utilities
library(effects) # for extracting and plotting effects 
library(emmeans) # for marginal effectss

#' 
## ----options, include = FALSE--------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 15))

#' 
## ----script, echo=FALSE--------------------------------------------------------------------------------
## Link in Github repo
downloadthis::download_link(
  link = download_link("https://github.com/stat-teaching/SMDA-2023/blob/master/labs/lab9.R"),
  button_label = "Download the R script",
  button_type = "danger",
  has_icon = TRUE,
  icon = "fa fa-save",
  self_contained = FALSE
)

#' 
#' # Overview
#' 
#' We are gonna work with the `admission.csv` dataset containing $n = 400$ students for the admission to the UCLA University. A researcher is interested in how variables, such as `gre` (Graduate Record Exam scores), `gpa` (GPA), `rank` (prestige of the undergraduate institution) have an influence for the admission into graduate school. The response variable, admit/don’t admit, is a binary variable.
#' 
#' ...(continue from lab 8)
#' 
#' 1. Fitting the model with interactions
#' 2. Plotting results
#' 3. Interpreting the parameters
#' 4. Model comparison for the interactions effect
#' 
#' # 0. Importing data
#' 
#' We need to set the **working directory** on the root of the course folder using `set.wd()`. Using R Projects is just necessary to open the `.RProj` file and the working directory will be automatically correctly selected.
#' 
## ------------------------------------------------------------------------------------------------------
# relevant steps of the previous lab
admission <- read.csv(here("data", "admission.csv"), header=TRUE)
admission$rankc <- factor(admission$rank, levels = 1:4, labels = 1:4)

#' 
#' # 1. Fitting model with interactions
#' 
#' Here we fit the two 2-way interactions between `gpa`, `gre` and `rankc`. Let's start from only one interaction:
#' 
## ------------------------------------------------------------------------------------------------------
# gpa * rankc + gre

fit1 <- glm(admit ~ gre + gpa + rankc + gpa:rankc, family = binomial(link = "logit"), data = admission)
# this is equivalent to 
# fit1 <- glm(admit ~ gpa * rankc + gre, family = binomial(link = "logit"), data = admission)
summary(fit1)

#' 
#' # 2. Plotting results
#' 
#' With interactions it is even more important and useful to plot the effects before anything else:
#' 
## ------------------------------------------------------------------------------------------------------
plot(effects::allEffects(fit1))

#' 
#' The plot on the left represent the main effect of `gre` and the plot on the right is the interaction between `gpa` and `rankc`. In this case we have an interaction between a numerical and a categorical variable. The model is essentially estimating the relationship between `gpa` and `admit` splitting by the level of `rankc`.
#' 
#' # 3. Interpreting the parameters
#' 
#' The interpretation of interactions (especially with categorical variables) from model parameters is not always easy because it depends on which **contrasts** are used. By default, R uses **dummy coding** where the reference level of the factor (`rankc`) is the first category and all the other categories are compared to the reference. This influence also other parameters:
#' 
#' - `(Intercept)`: log odds of being admitted for `gpa = 0`, `gre = 0` and `rankc` at the reference (i.e., 1)
#' - `gpa`: the increase in log odds of being admitted for a unit increase in the `gpa` for people in the reference level of `rankc` (i.e., 1)
#' - `gre`: the increase in log odds of being admitted for a unit increase in the `gre` for people in the reference level of `rankc` (i.e., 1)
#' - `rankc2`: is the difference in the log odds of being admitted between `rankc2` and `rankc1` (i.e. is the log odds ratio) when `gpa` and `gre` are 0
#' - `rankc3`: is the difference in the log odds of being admitted between `rankc3` and `rankc1` (i.e. is the log odds ratio) when `gpa` and `gre` are 0
#' - `rankc4`: is the difference in the log odds of being admitted between `rankc4` and `rankc1` (i.e. is the log odds ratio) when `gpa` and `gre` are 0
#' - `gpa:rankc2`: the difference between `rankc` 2 and 1 in the rate of increase of the log odds of being admitted for a unit increase in the `gpa`
#' - `gpa:rankc3`: the difference between `rankc` 3 and 1 in the rate of increase of the log odds of being admitted for a unit increase in the `gpa`
#' - `gpa:rankc4`: the difference between `rankc` 4 and 1 in the rate of increase of the log odds of being admitted for a unit increase in the `gpa`
#' 
#' For complex interactions like this, a suggestion is to plot the effects (as we did before) and to estimate the individual slopes checking if the interpretation is correct:
#' 
## ------------------------------------------------------------------------------------------------------
# emmeans is an options to estimate effects regardless the model parameters
emm <- data.frame(emmeans::emtrends(fit1, ~rankc, var = "gpa"))
emm

# reference level
emm$gpa.trend[1]

# gpa:rankc2
emm$gpa.trend[2] - emm$gpa.trend[1]

# ...

#' 
#' The other difficulty with interactions is interpreting the categorical variable effects. The `rankc2`, `rankc3` ... effects are interpreted as usual BUT in the presence of interactions by definition the difference between i.e. `rankc2` and `rankc1` depends on the level of other variables (in particular `gpa` in this case). Let's explain this visually:
#' 
#' This is the main effect of the `rankc` without considering the other variables:
#' 
## ------------------------------------------------------------------------------------------------------
plot(effects::effect("rankc", fit1))

#' 
## ---- out.width="100%", echo = FALSE-------------------------------------------------------------------
knitr::include_graphics("img/main-effect-rankc.png")

#' 
#' However, in the presence of interactions, the odds ratio could be influenced by the `gpa` level where it is evaluated:
#' 
## ---- echo = FALSE-------------------------------------------------------------------------------------
preds <- expand.grid(
    rankc = c("1", "2", "3", "4"),
    gpa = seq(0, 6, 0.01),
    gre = mean(admission$gre)
)

preds |> 
    add_predict(fit1) |> 
    ggplot(aes(x = gpa, y = pr, color = rankc)) +
    geom_line() +
    theme_minimal(20) +
    geom_vline(xintercept = c(0, 2, 4, 5, 5.8))

#' 
#' The model (without transformations), evaluate the effect of `rankc` when other variables are 0 and this could be meaningful or not.
#' 
#' Without interaction by definition the point at which I evaluate the `renkc` effect is not relevant.
#' 
## ---- echo = FALSE-------------------------------------------------------------------------------------
fit_noint <- glm(admit ~ gre + gpa + rankc, family = binomial(link = "logit"), data = admission)
preds <- expand.grid(
    rankc = c("1", "2", "3", "4"),
    gpa = seq(0, 6, 0.01),
    gre = mean(admission$gre)
)

preds |> 
    add_predict(fit_noint) |> 
    ggplot(aes(x = gpa, y = pr, color = rankc)) +
    geom_line() +
    theme_minimal(20) +
    geom_vline(xintercept = c(0, 2, 4, 5, 5.8))

#' 
#' # 4. Inference and model comparison for the interactions effect
#' 
#' Even if from the plot there is evidence for a little bit of interaction (i.e, the slopes are not the same across `rankc`) we need a statistical test. The first option is to see the Wald test of model coefficients testing if the slopes are different (e.g., `gpa:rankc2`).
#' 
#' To test the overall interaction we can use the `car::Anova()` function that reports main effects and interactions:
#' 
## ------------------------------------------------------------------------------------------------------
car::Anova(fit1)

#' 
#' We see that as reported in the Lab 8, there are the main effects of `gre`, `gpa` and `rankc` but there is no evidence for the interaction. The `gpa:rankc` test if there is at least one slope difference that is statistically significant.
#' 
#' To note, the `car::Anova(fit1)` results for the interaction is just a likelihood ratio test comparing a model with the interaction vs a model without the interaction:
#' 
## ------------------------------------------------------------------------------------------------------
fit_noint <- glm(admit ~ gre + gpa + rankc, family = binomial(link = "logit"), data = admission)

anova(fit_noint, fit1, test = "LRT")

#' 
#' In fact the `Chisq` and the `p values` are the same. The model is just testing if including the interaction reduces the residual deviance.
