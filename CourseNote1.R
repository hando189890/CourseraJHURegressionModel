library(UsingR);data(galton)
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)


library(manipulate)
myHist <- function(mu){
  mse <- mean((galton$child-mu)^2)
  g <- ggplot(galton, aes(x=child)) + geom_histogram(fill='salmon', colour = 'black', binwidth=1)
  g <- g+geom_vline(xintercept = mu, size =3)
  g <- g+ggtitle(paste("mu= ", mu, ", MSE= ", round(mse, 2), sep = ""))
  g
}

manipulate(myHist(mu), mu=slider(62,74,step = 0.5))

library(manipulate)
myHist<-function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu,mu),c(0,150),col="red",lwd=5)
  mse<-mean((galton$child-mu)^2)
  text(63,150,paste("mu=",mu))
  text(63,140,paste("MSE=",round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))


hist(galton$child,col="blue",breaks=100) 
meanChild<-mean(galton$child) 
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

plot(galton$parent,galton$child,pch=19,col="blue")


myPlot<-function(beta){ 
  y<-galton$child-mean(galton$child) 
  x<-galton$parent-mean(galton$parent) 
  freqData<-as.data.frame(table(x,y)) 
  names(freqData)<-c("child","parent","freq") 
  plot(
    as.numeric(as.vector(freqData$parent)), 
    as.numeric(as.vector(freqData$child)), 
    pch=21,col="black",bg="lightblue", 
    cex=.15*freqData$freq, 
    xlab="parent",
    ylab="child"
  )
  abline(0,beta,lwd=3) 
  points(0,0,cex=2,pch=19) 
  mse<-mean((y-beta*x)^2) 
  title(paste("beta=",beta,"mse=",round(mse,3)))
}
manipulate(myPlot(beta),beta=slider(0.6,1.2,step=0.02))

lm(I(child-mean(child))~I(parent-mean(parent))-1,data=galton)


