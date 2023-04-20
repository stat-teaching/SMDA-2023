# 2 continous variables
# passing the exam as a function of hours of study
# and self perception of having fun with R

n <- 100
b0 <- qlogis(0.01)
b1 <- 0.08
b2 <- 0.8
x1 <- rpois(n, 50)
x2 <- round(runif(n, 1, 10))
dat <- data.frame(study_h = x1, funR = x2)
dat$exam <- rbinom(nrow(dat), 1, plogis(b0 + b1*dat$study_h + b2*dat$funR))
fit <- glm(exam ~ study_h + funR, data = dat, family = binomial(link = "logit"))

# 1 continous and 1 categorical variable, interaction
# passing the exam as a function of hours of study
# for people with a background in stat and math and people
# without

n <- 1e5
b0 <- qlogis(0.5)
b1 <- 0.1 # ~log(odds_ratio(0.7, 0.6))
b2 <- 0.22 # ~log(odds_ratio(0.7, 0.65))
b3 <- -0.1

dat <- data.frame(bg = rep(c("math", "nomath"), each = n/2))
dat$study_h <- rpois(n, 50)
dat$bg <- factor(dat$bg)
contrasts(dat$bg) <- c(math = 0.5, nomath = -0.5)
dat$bg_e <- ifelse(dat$bg == "math", 0.5, -0.5)
dat$study_h0 <- dat$study_h - mean(dat$study_h)

dat$lp <- with(dat, b0 + b1*study_h0 + b2*bg_e + b3*bg_e*study_h0)
dat$exam <- rbinom(nrow(dat), 1, plogis(dat$lp))

fit <- glm(exam ~ study_h0 * bg, data = dat, family = binomial(link = "logit"))

# 2 categorical variables (2 levels)

n <- 1e5
b0 <- qlogis(0.5)
b1 <- 0.1 # ~log(odds_ratio(0.7, 0.6))
b2 <- 0.22 # ~log(odds_ratio(0.7, 0.65))
b3 <- -0.1

dat <- expand.grid(x1 = c("a", "b"),
                   x2 = c("c", "d"),
                   n = 1:(n/4)) |> select(-n)

b0 <- qlogis(0.5) # grand mean
b1 <- 0.1
b2 <- 0.2
b3 <- 0

dat$x1 <- factor(dat$x1)
dat$x2 <- factor(dat$x2)

contrasts(dat$x1) <- c(a = -0.5, b = 0.5)
contrasts(dat$x2) <- c(c = -0.5, d = 0.5)

dat$x1c <- with(dat, contrasts(x1)[x1])
dat$x2c <- with(dat, contrasts(x2)[x2])

dat$lp <- with(dat, b0 + b1*x1c + b2*x2c + b3*x1c*x2c)
dat$y <- rbinom(nrow(dat), 1, plogis(dat$lp))
