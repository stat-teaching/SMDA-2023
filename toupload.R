dat <- read.csv(here("data", "nwords.csv"))

count_na(dat)

sapply(dat, function(x) sum(is.na(x)))

dat$ses <- factor(dat$ses, levels = c("low", "middle", "high"))
dat$babysitter <- factor(dat$babysitter, levels = c("no", "yes"))

dat$timebonding0 <- dat$timebonding - mean(dat$timebonding)
dat$caregiving0 <- dat$caregiving - mean(dat$caregiving)


summary(dat)


par(mfrow = c(1,2))
hist(dat$timebonding)
hist(dat$timebonding0)

plot(dat$timebonding, dat$nwords, pch = 19)



ggplot(dat, aes(x = caregiving, y = nwords, color = ses)) +
    geom_point() +
    stat_smooth(method = "glm", method.args = list(family = poisson()))

fit <- glm(nwords ~ timebonding + caregiving + babysitter + ses, family = poisson(link = "log"), data = dat)
summary(fit)

performance::check_overdispersion(fit)

plot(allEffects(fit))

fit2 <- glm(nwords ~ timebonding * ses + caregiving + babysitter, family = poisson(link = "log"), data = dat)

summary(fit2)

plot(allEffects(fit2))

fit3 <- glm(nwords ~ timebonding * ses + caregiving*ses + babysitter, family = poisson(link = "log"), data = dat)

plot(allEffects(fit3))

car::Anova(fit3)

fit4 <- glm(nwords ~ timebonding * ses + caregiving*ses + babysitter*timebonding + caregiving*babysitter, family = poisson(link = "log"), data = dat)


plot(allEffects(fit4))

car::Anova(fit4)

emtrends(fit4, pairwise~babysitter, var = "timebonding")

summary(fit4)

library(MASS)

fit5 <- glm.nb(nwords ~ timebonding * ses + caregiving*ses + babysitter*timebonding + caregiving*babysitter, data = dat)

summary(fit5)

car::Anova(fit5)

plot(allEffects(fit5))




