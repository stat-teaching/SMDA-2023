# probability of drop out school
set.seed(2023)

n <- 500
    
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

ps_names <- do.call(paste0, expand.grid(parenting, academic))

ps <- c(0.05, 0.1, 0.15, 0.2,
        0.2, 0.3, 0.3, 0.4)

names(ps) <- ps_names

b0 <- qlogis(ps["neglectfullow"]) # p drop out neglectful (low)
b1 <- log(odds_ratio(ps["authoritativelow"], ps["neglectfullow"])) # odds ratio permissive vs neglectful (low)
b2 <- log(odds_ratio(ps["authoritarianlow"], ps["neglectfullow"])) # odds ratio authoritative vs neglectful (low)
b3 <- log(odds_ratio(ps["permissivelow"], ps["neglectfullow"])) # odds ratio authoritarian vs neglectful (low)
b4 <- log(odds_ratio(ps["neglectfulhigh"], ps["neglectfullow"])) # odds ratio high vs low (neglectful)
b5 <- log(odds_ratio(ps["authoritativehigh"], ps["neglectfulhigh"])) - b1
b6 <- log(odds_ratio(ps["authoritarianhigh"], ps["neglectfulhigh"])) - b2
b7 <- log(odds_ratio(ps["permissivehigh"], ps["neglectfulhigh"]))  - b3

B <- c(b0, b1, b2, b3, b4, b5, b6, b7)
X <- model.matrix(~parenting * academic, data = dat)

dat$lp <- X %*% B
dat$drop <- rbinom(nrow(dat), 1, plogis(dat$lp))
dat$id <- 1:nrow(dat)

dat_save <- select(dat, id, parenting, academic, drop)

write.csv("data/dropout.csv", row.names = FALSE)

