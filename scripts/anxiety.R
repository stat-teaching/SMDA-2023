n <- 1e5
age <- round(rnorm(n, 15, 1))
fam_had_anx <- sample(c("yes", "no"), n, replace = TRUE, prob = c(0.3, 0.7))

S <- diag(c(3, 3, 3)) %*% (0.5 + diag(1 - 0.6, nrow = 3)) %*% diag(c(3, 3, 3))
X <- MASS::mvrnorm(n, mu = c(4, 5, 7), Sigma = S)

X <- data.frame(X)
names(X) <- c("depression", "stress", "network")

X$ses <- sample(c("low", "middle", "high"), 
              n, 
              replace = TRUE, 
              prob = c(0.3, 0.5, 0.2))


X$ses <- factor(X$ses, levels = c("low", "middle", "high"))
X$age <- age
X$fam_had_anx <- factor(fam_had_anx, levels = c("no", "yes"))

XT <- model.matrix(~depression + stress + network + ses + fam_had_anx + depression:fam_had_anx + stress:fam_had_anx,
                   data = X)

b0 <- qlogis(0.05)
b1 <- log(1.1)
b2 <- log(1.2)
b3 <- log(1.05)
b4 <- log(1)
b5 <- log(1)
b6 <- log(5)
b7 <- log(1.15) - b1
b8 <- log(1.25) - b2

B <- c(b0, b1, b2, b3, b4, b5, b6, b7, b8)
X$lp <- XT %*% B

X$anxiety <- rbinom(nrow(X), 1, plogis(X$lp))

fit <- glm(anxiety ~ depression + stress + network + ses + fam_had_anx + depression:fam_had_anx + stress:fam_had_anx,
    data = X,
    family = binomial())

summary(fit)

plot(allEffects(fit))

