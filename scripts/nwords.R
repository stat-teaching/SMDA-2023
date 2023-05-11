## number of words pronounced by children during one day
## as a function of several predictors

library(dplyr)
library(tidyr)
library(MASS)
library(purrr)

# Generate the population

S <- diag(c(2,2,2,8,8,8)) %*% (0.6 + diag(1 - 0.6, nrow = 6)) %*% diag(c(2,2,2,8,8,8))
xs <- MASS::mvrnorm(1e5, mu = c(8, 5, 3, 20, 15, 8), Sigma = S, empirical = TRUE)
colnames(xs) <- c("caregiving_low", "caregiving_middle", "caregiving_high",
                  "timebonding_low", "timebonding_middle", "timebonding_high")

xs <- data.frame(xs)

caregiving_d <- xs[, 1:3] |> 
    pivot_longer(1:3) |> 
    separate(name, c("x", "ses"), sep = "_")

timebonding_d <- xs[, 4:6] |> 
    pivot_longer(1:3) |> 
    separate(name, c("x", "ses"), sep = "_")

pop <- data.frame(
    id = 1:nrow(xs),
    timebonding = timebonding_d$value,
    caregiving = caregiving_d$value,
    ses = timebonding_d$ses
)

# to discrete

pop$timebonding <- round(pop$timebonding)
pop$timebonding <- ifelse(pop$timebonding < 0, 0, pop$timebonding)

pop$caregiving <- round(pop$caregiving)
pop$caregiving <- ifelse(pop$caregiving < 0, 0, pop$caregiving)

# generate the sample
set.seed(2024)

n <- 150
pop$ses <- factor(pop$ses, levels = c("low", "middle", "high"))
ses_prob <- c(0.3, 0.5, 0.2)
nsample <- floor(n*ses_prob)
datl <- split(pop, pop$ses)
datl <- map2(datl, nsample, ~.x[sample(1:nrow(.x), .y), ])
dat <- bind_rows(datl)

dat$babysitter <- ifelse(dat$ses == "low", 
                     sample(c("yes", "no"), sum(dat$ses == "low"), TRUE, prob = c(0.2, 0.8)),
                     ifelse(dat$ses == "middle",
                            sample(c("yes", "no"), sum(dat$ses == "middle"), TRUE, prob = c(0.4, 0.6)),
                            sample(c("yes", "no"), sum(dat$ses == "high"), TRUE, prob = c(0.8, 0.2))))


dat$babysitter <- factor(dat$babysitter, levels = c("no", "yes"))
dat$id <- 1:nrow(dat)
dat <- set_contrasts(dat, contr.treatment)

dat$timebonding0 <- dat$timebonding - mean(dat$timebonding)
dat$caregiving0 <- dat$caregiving - mean(dat$caregiving)
dat <- add_numeric_contrast(dat)

theta <- 5 # expected vmr = var_from_theta(mean(dat$nwords), theta) / mean(dat$nwords)

# true model nwords ~ caregiving + babysitter + timebonding*ses

b0 <- log(20) # intercept
b1 <- log(1.05) # caregiving
b2 <- log(1) # babysitter
b3 <- log(1.02) # timebonding (babysitter 0 and ses low)
b4 <- log(0.9) # middle vs low
b5 <- log(0.8) # high vs low
b6 <- log(1.03) - b3 # timebonding effect middle vs low
b7 <- log(1.04) - b3 # timebonding effect high vs low

B <- c(b0, b1, b2, b3, b4, b5, b6, b7)

X <- model.matrix(~caregiving0 + babysitter + timebonding0*ses, data = dat)
dat$lp <- exp(X %*% B)
#dat$nwords <- rpois(nrow(dat), dat$lp)
dat$nwords <- rnegbin(nrow(dat), dat$lp, theta)

# check
# fit <- glm(nwords ~ caregiving + babysitter + timebonding*ses, data = dat, family = poisson())
# car::Anova(fit)

# saving

nwords <- dat |> 
    dplyr::select(id, timebonding, caregiving, ses, babysitter, nwords)

write.csv(nwords, file = "data/nwords.csv", row.names = FALSE)
