library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))

## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))

## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail") + magn, data = shuttle, 
           family = binomial)
exp(coef(fit))

fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
exp(coef(fit))[2]

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])