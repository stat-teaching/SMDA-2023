ggtheme <- function(size = 15){
    theme_minimal(base_size = size)
}

odds <- function(p){
    p / (1 - p)
}


difficulty <- rep(0:7, each = 5000)

b0 <- qlogis(0.95)
b1 <- -0.7

dat <- data.frame(id = 1:length(difficulty),
                  difficulty)
dat$p <- plogis(b0 + b1*difficulty)
dat$y <- rbinom(nrow(dat), 1, dat$p)

dat |> 
    group_by(difficulty) |> 
    summarise(nc = sum(y),
              tot = n(),
              nf = tot - nc) |> 
    ggplot(aes(x = difficulty, y = nc/tot)) +
    ylim(c(0,1)) +
    geom_smooth(method = "glm",
                method.args = list(family = "binomial"), 
                se = FALSE) +
    geom_point()


# frequency table
table(dat$difficulty, dat$y)

tab_dat <- dat |> 
    group_by(difficulty) |> 
    summarise(nc = sum(y),
              tot = n(),
              nf = tot - nc,
              pc = nc/tot,
              pf = 1 - pc)

# odds

log(odds(tab_dat$pc)[4]) - log(odds(tab_dat$pc)[3])

fit <- glm(y ~ difficulty, family = binomial(link = "logit"), data = dat)

# example of the different scale between proportions and odds
predict(fit, data.frame(difficulty = c(1, 0))) |> diff()
predict(fit, data.frame(difficulty = c(2, 1))) |> diff()

predict(fit, data.frame(difficulty = c(2, 1)), type = "response") |> diff()

odds(tab_dat$pc[3])

odds(tab_dat$pc[2])*exp(coef(fit)[2])

# odds ~ x vs log odds ~ x
# figure 9.5 pp 341 Dunn 2018

tab_dat |> 
    mutate(odds = odds(pc),
           log_odds = log(odds)) |> 
    pivot_longer(c(odds, log_odds), names_to = "type", values_to = "y") |> 
    ggplot(aes(x = difficulty, y = y)) +
    geom_point() +
    facet_wrap(~type, scales = "free") +
    geom_line() +
    ggtheme()

# maybe interesting, inverse prediction
?MASS::dose.p(fit, p = 0.8)

