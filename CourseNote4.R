download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda" ,destfile="./data/ravensData.rda",method="curl")
load("./data/ravensData.rda")
head(ravensData)

lmRavens<-lm(ravensData$ravenWinNum~ravensData$ravenScore) 
summary(lmRavens)$coef

x<-seq(-10,10,length=1000) 
manipulate(
  plot(x,exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)), type="l",lwd=3,frame=FALSE),
  beta1=slider(-2,2,step=.1,initial=2), beta0=slider(-2,2,step=.1,initial=0) 
  )

logRegRavens<-glm(ravensData$ravenWinNum~ravensData$ravenScore,family="binomial") 
summary(logRegRavens)

plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="ProbRavensWin")

exp(logRegRavens$coeff)

exp(confint(logRegRavens))

anova(logRegRavens,test="Chisq")

n<-500;x<-seq(0,4*pi,length=n);y<-sin(x)+rnorm(n,sd=.3)
knots<-seq(0,8*pi,length=20); 
splineTerms<-sapply(knots,function(knot)(x>knot)*(x-knot)) 
xMat<-cbind(1,x,splineTerms)
yhat<-predict(lm(y~xMat-1)) 
plot(x,y,frame=FALSE,pch=21,bg="lightblue",cex=2) 
lines(x,yhat,col="red",lwd=2)


splineTerms<-sapply(knots,function(knot)(x>knot)*(x-knot)^2) 
xMat<-cbind(1,x,x^2,splineTerms) 
yhat<-predict(lm(y~xMat-1)) 
plot(x,y,frame=FALSE,pch=21,bg="lightblue",cex=2) 
lines(x,yhat,col="red",lwd=2)

##Chordfinder,playingthewhitekeysonapianofromoctavec4-c5
notes4<-c(261.63,293.66,329.63,349.23,392.00,440.00,493.88,523.25) 
t<-seq(0,2,by=.001);n<-length(t) 
c4<-sin(2*pi*notes4[1]*t);e4<-sin(2*pi*notes4[3]*t); 
g4<-sin(2*pi*notes4[5]*t) 
chord<-c4+e4+g4+rnorm(n,0,0.3) 
x<-sapply(notes4,function(freq)sin(2*pi*freq*t)) 
fit<-lm(chord~x-1)


##(Howyouwouldreallydoit)
a<-fft(chord);plot(Re(a)^2,type="l")

