library(lme4)

nsim <- 1000
nt <- 30
ns <- 30
tau_id <- 0.2

p0 <- 0.7
p1 <- 0.75

dat <- expand.grid(
    id = 1:ns,
    cond = c("a", "b"),
    nt = nt/2
)

b0 <- qlogis(p0)
b1 <- log((p1/(1-p1))/(p0/(1-p0)))

log_or <- vector(mode = "numeric", length = nsim)
glm_d <- vector(mode = "numeric", length = nsim)
lm_d <- vector(mode = "numeric", length = nsim)

for(i in 1:nsim){
    b0i <- rnorm(ns, 0, 0.4)
    dat$p <- plogis(with(dat, b0 + b0i[id] + b1*ifelse(cond == "a", 0, 1)))
    dat$nc <- rbinom(nrow(dat), nt/2, dat$p)
    dat$nf <- dat$nt - dat$nc
    dat$acc <- dat$nc / dat$nt
    fit <- glmer(cbind(nc, nf) ~ cond + (1|id), data = dat, family = binomial(link = "logit"))
    log_or[i] <- fixef(fit)[2]
    glm_d[i] <- (sqrt(3) * fixef(fit)[2])/pi
    lm_d[i] <- as.numeric(effectsize::cohens_d(acc ~ cond, data = dat, paired = TRUE))[1]
}

hist((-lm_d) / glm_d)
