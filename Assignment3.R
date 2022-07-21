6
B
dim(InsectSprays)
head(InsectSprays,15)
sA
summary(InsectSprays[,2])
sapply(InsectSprays,class)
fit <- lm(count ~ spray, InsectSprays)
summary(fit)$coef
est <- summary(fit)$coef[,1]
mean(sA)
mean(sB)
nfit <- lm(count ~ spray - 1, InsectSprays)
summary(nfit)$coef
spray2 <- relevel(InsectSprays$spray,"C")
summary(fit2)$coef
mean(sC)
(fit$coef[2]-fit$coef[3])/1.6011


dim(hunger)
names(hunger)
fit <- lm(hunger$Numeric ~ hunger$Year)
summary(fit)$coef
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"] ~ hunger$Year[hunger$Sex=="Female"])
lmM <- lm(hunger$Numeric[hunger$Sex=="Male"] ~ hunger$Year[hunger$Sex=="Male"])
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex)
summary(lmBoth)
lmInter <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex + hunger$Year * hunger$Sex)
summary(lmInter)


fit <- lm(y ~ x, out2) 
plot(fit, which=1)
out2[-1, ]
fitno <- lm(y ~ x, out2[-1, ])
plot(fitno, which=1)
coef(fit)-coef(fitno)
head(dfbeta(fit))
resno <- out2[1, "y"] - predict(fitno, out2[1,]) 
1-resid(fit)[1]/resno
View(hatvalues(fit))
sigma <- sqrt(deviance(fit)/df.residual(fit))
rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit)))
head(cbind(rstd, rstandard(fit)))
plot(fit, which=3)
plot(fit, which=2)
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))
resid(fit)[1]/(sigma1*sqrt(1-hatvalues(fit)[1]))
View(rstudent(fit))
dy <- predict(fitno, out2)-predict(fit, out2)
sum(dy^2)/(2*sigma^2)
plot(fit, which=5)


