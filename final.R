library(ggplot2)
data(mtcars)
View(mtcars)
summary(mtcars)

mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))

head(mtcars$vs)
head(mtcars$am)

t.test(mtcars$mpg ~ mtcars$am)
boxplot(mpg ~ am, data=mtcars, ylab = "MPG", xlab="Transmission",main="MPG ~ Transmission", col = "white")

#Since p-value = 0.0014 < 0.05, thus reject null hypothesis in alternative conclude that manual is better than automatic for MPG. 
#The conclusion is also represented by boxplot where clearly shows Manual is much better than Automatic in plot. 



simple <- lm(mpg ~ factor(am), data=mtcars)
summary(simple)
model <- lm(mpg~ am + wt + qsec, data=mtcars)
summary(model)
par(mfrow = c(2, 2))
plot(bestfit)

#With Multiple R-squared 0.8947 and Adjusted R-squared 0.8336 we conclude that 
# when the weight increased by 1000 lbs,  mpg increased about 2.9358 for manual transmission cars
# when acceleration speed decreased, mpg factor increased by 1.2259 miles for manual transmission cars

# It follows that, mpg is determined by transmission, weight and acceleration. 
