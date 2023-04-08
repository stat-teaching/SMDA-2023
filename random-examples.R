# contingency table example
# passing the exam ~ watching tv-shows
# passing the exam = 0/1 --> 0 = failed, 1 = passed
# watching tv-shows = 0/1 --> 0 = no, 1 = yes (no quantity info)

n <- 100

passing_tv_yes <- 0.8
passing_tv_no <- 0.6

OR <- odds_ratio(passing_tv_yes, passing_tv_no)

# simulate data

dat <- data.frame(
   tv_shows = rep(c("yes", "no"), each = n/2)
)

dat$exam <- ifelse(dat$tv_shows == "yes",
                   rbinom(n/2, 1, passing_tv_yes),
                   rbinom(n/2, 1, passing_tv_no))

m <- ctable(dat$tv_shows, dat$exam)

p_tv_no <- mean((dat$exam == 1)[dat$tv_shows == "no"])
p_tv_yes <- mean((dat$exam == 1)[dat$tv_shows == "yes"])

# observed or
or <- odds_ratio(p_tv_yes, p_tv_no)

# calculating difference of probabilities
pdiff(p_tv_yes, p_tv_no, n/2)

fit <- glm(exam ~ tv_shows , data = dat, family = binomial())

# same predicted difference as manually calculated
pd <- data.frame(predict(fit, 
                         data.frame(tv_shows = c("yes", "no")), 
                         type = "response", 
                         se.fit = TRUE)
)

# same or as the manually calculated
exp(coef(fit)[2])

# with confidence interval
exp(confint(fit, "tv_showsyes"))

