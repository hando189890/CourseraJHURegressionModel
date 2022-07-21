rgp1()
rgp2()
head(swiss)
mdl <- lm(Fertility ~ ., swiss)
vif(mdl)
mdl2 <- lm(Fertility ~ . -Examination, swiss)
vif(mdl2)

x1c <- simbias()
apply(x1c, 1, mean)
fit1 <- lm(Fertility ~ Agriculture, swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, swiss)
anova(fit1, fit3)
deviance(fit3)
d <- deviance(fit3)/43
n <- (deviance(fit1) - deviance(fit3))/2
n/d
pf(n/d, 2, 43, lower.tail=FALSE)
shapiro.test(fit3$residuals)
anova(fit1, fit3, fit5, fit6)


View(ravenData)
mdl <- glm(ravenWinNum ~ ravenScore, binomial, ravenData)
lodds <- predict(mdl, data.frame(ravenScore=c(0, 3, 6)))
exp(lodds)/(1+exp(lodds))
summary(mdl)
exp(confint(mdl))
anova(mdl)
qchisq(0.95, 1)


var(rpois(1000, 50))
View(hits)
class(hits[,'date'])
as.integer(head(hits[,'date']))
mdl <- glm(visits ~ date, poisson, hits)
summary(mdl)
exp(confint(mdl, 'date'))
which.max(hits[,'visits'])
hits[704,]
mdl$fitted.values[704]
lambda <- mdl$fitted.values[704]
qpois(.95, lambda)
mdl2 <- glm(simplystats ~ date, poisson, hits, offset=log(visits+1))
qpois(.95, mdl2$fitted.values[704])
