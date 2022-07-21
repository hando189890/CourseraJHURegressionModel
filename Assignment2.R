fit <- lm(child ~ parent, galton)
sqrt(sum(fit$residuals^2) / (n - 2))
summary(fit)$sigma
sqrt(deviance(fit)/(n-2))
mu <- mean(galton$child) 
sTot <- sum((galton$child-mu)^2)
sRes <- deviance(fit)
1-sRes/sTot
summary(fit)$r.squared
cor(galton$parent,galton$child)^2

ones <- rep(1, nrow(galton))
lm(child ~ ones + parent - 1, galton)
lm(child ~ parent, galton)
lm(child ~ 1, galton)
head(trees)
fit <- lm(Volume ~ Girth + Height + Constant -1, trees)
trees2 <- eliminate("Girth", trees)
View(trees2)
fit2 <- lm(Volume ~ Height + Constant -1, trees2)
lapply(list(fit, fit2), coef) 

all <- lm(Fertility ~ ., swiss)
summary(all)
summary(lm(Fertility ~ Agriculture, swiss))
cor(swiss$Examination,swiss$Education)
cor(swiss$Agriculture,swiss$Education)
makelms()
ec <- swiss$Examination+swiss$Catholic
efit <- lm(Fertility ~ . + ec, swiss)
all$coefficients-efit$coefficients