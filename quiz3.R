#datasets(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)$coef

fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
summary(fit1)$coef

summary(fit2)$coef

anova(fit1, fit2)

lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
influence(lm(y ~ x))$hat
xm <- cbind(1, x)
diag(xm %*% solve(t(xm) %*% xm) %*% t(xm))

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
influence.measures(lm(y ~ x))

