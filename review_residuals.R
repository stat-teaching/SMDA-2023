dat <- data.frame(
    x = c(rnorm(30), 10)
)

dat$y <- 0.2 + 0.5 * dat$x + rnorm(nrow(dat))

dat$y[31] <- -10

fit <- lm(y ~ x, data = dat)

par(mfrow = c(2,2))
plot(fit)

par(mfrow = c(1,1))
plot(dat$x, dat$y)
abline(fit)
abline(update(fit, . ~ ., data = dat[1:30,]))
points(dat[31, ], col = "red")

sigma(fit)

n <- nrow(dat)
h <- hatvalues(fit)
p <- length(coef(fit))
r <- residuals(fit, type = "response")
rs <- r / sqrt(sigma(fit)^2 * (1 - h))
rs <- rstandard(fit)

(rs * sqrt(((n - p - 2) / (n - p - 1 - rs^2))))[1:5]
rstudent(fit)[1:5]

fit

X = model.matrix(fit)
H = X %*% solve((t(X) %*% X)) %*% t(X)
diag(H)[1:5]
hatvalues(fit)[1:5]

(H %*% dat$y)[1:5]
fitted(fit)[1:5]

sigma(fit)^2 * (1 - diag(H))
