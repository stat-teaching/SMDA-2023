# probability of drop out school

n <- 50

parenting <- c("authoritative", 
               "authoritarian", 
               "permissive", 
               "neglectful")

academic <- c("high", "low")
ps <- c(0.4, 0.2, 0.3, 0.1, 0.1, 0.2, 0.3, 0.4)
p <- ps / sum(ps)
ns <- sample(1:8, n, replace = TRUE, prob = p)

dat <- expand.grid(parenting = parenting, academic = academic)
dat <- dat[ns, ]
rownames(dat) <- NULL

dat$parenting <- factor(dat$parenting, levels = c("neglectful",
                                                  "permissive",
                                                  "authoritative",
                                                  "authoritarian"))
dat$academic <- factor(dat$academic, levels = c("low", "high"))

dat$y <- rnorm(nrow(dat)) 

fit <- lm(y ~ parenting * academic, data = dat)

summary(fit)

b0 <- qlogis(0.4)
b1 <- log(odds_ratio(0.3, 0.4))
b2 <- log(odds_ratio(0.1, 0.4))
b3 <- log(odds_ratio(0.4, 0.4))
b4 <- log(odds_ratio(0.2, 0.4))
b5 <- log(1)
b6 <- log(1)
b7 <- log(1)

B <- c(b0, b1, b2, b3, b4, b5, b6, b7)
X <- model.matrix(~parenting * academic, data = dat)

dat$lp <- X %*% B
dat$drop <- rbinom(nrow(dat), 1, plogis(dat$lp))
