# logistic vs linear predictions
# useful to explain why the logistic is more appropriate
# adapted from fig. 14.1 Fox pp 372

b0 <- qlogis(0.01)
b1 <- 0.1

x <- runif(100, 0, 100)
p <- plogis(b0 + b1*x)
y <- rbinom(length(x), 1, p)

dat <- data.frame(y, x)

fit_lm <- lm(y ~ x, data = dat)
fit_glm <- glm(y ~ x, data = dat, family = binomial(link = "logit"))

dat$glm <- predict(fit_glm, type = "response")
dat$lm <- predict(fit_lm)
dat <- pivot_longer(dat, c(lm, glm), names_to = "model", values_to = "yp")

library(ggplot2)

dat |> 
    ggplot(aes(x = x, y = y)) +
    geom_point(size = 2, alpha = 0.5) +
    geom_line(aes(x = x, y = yp, color = model),
              size = 1) +
    ggthemes::theme_par(base_size = 15) +
    ylab("p(y|x)") +
    theme(legend.title = element_blank(),
          legend.position = c(0.9, 0.3),
          panel.grid.minor = element_blank()) +
    ylim(-0.2, 1.2) +
    ggtitle("Predicted values lm() vs glm()")
    


