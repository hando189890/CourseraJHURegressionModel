x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y ~ x))$coef

summary(lm(y ~ x))$sigma

data(mtcars)
fit <- lm(mpg ~ I(wt - mean(wt)), data = mtcars)
confint(fit)

fit <- lm(mpg ~ wt, data = mtcars)
predict(fit, newdata = data.frame(wt = 3), interval = "prediction")

fit <- lm(mpg ~ wt, data = mtcars)
confint(fit)[2, ] * 2
fit <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit)[2, ]

fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared
sse1 <- sum((predict(fit1) - mtcars$mpg)^2)
sse2 <- sum((predict(fit2) - mtcars$mpg)^2)
sse1/sse2

