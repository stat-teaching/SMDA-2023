# developing andatpiety disorders as a function of the
# - andatpiety familiarity (yes/no)
# - age
# - self esteem
# - social network

devtools::load_all()
library(dplyr)

set.seed(510)

n <- 200
family <- sample(c("yes", "no"), n, replace = TRUE, prob = c(0.3, 0.7))

vars <- c(4, 250, 2.25)
mus <- c(7, 40, 16)

R <- matrix(c(1, 0.6, 0.4,
              0.6, 1, 0.3,
              0.4, 0.3, 1),
            nrow = 3,
            byrow = TRUE)

S <- diag(sqrt(vars)) %*% R %*% diag(sqrt(vars))

datp <- data.frame(MASS::mvrnorm(n, mus, S))
names(datp) <- c("selfesteem", "socialnetwork", "age")

datp$age <- round_cut(datp$age, 12, 19)
datp$selfesteem <- round_cut(datp$selfesteem, 0, 10)
datp$socialnetwork <- round_cut(datp$socialnetwork, 0, 100)
datp$family <- family

datp$family <- factor(datp$family, levels = c("no", "yes"))

datp <- center(datp)

B <- c(
    b0 = qlogis(0.1),
    b1 = -log(1.3),
    b2 = -log(1.05),
    b3 = log(1),
    b3 = log(3)
)

X <- model.matrix(~selfesteem0 + socialnetwork0 + age0 + family, data = datp)

datp$lp <- X %*% B
datp$anx <- rbinom(nrow(datp), 1, plogis(datp$lp))

#fit <- glm(anx ~ selfesteem + socialnetwork + age + family, data = datp, family = binomial())

anxiety <- datp
anxiety <- select(anxiety, anx, selfesteem, socialnetwork, age, family)

save(anxiety, file = "labs/exam-simulation/anxiety.rda")
