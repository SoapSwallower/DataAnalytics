setwd("/Users/tungstentoothpick/Downloads")
EPI_data <- read.csv("epi2024results06022024.csv") 
View(EPI_data)
attach(EPI_data)
EPI.new #prints out data of EPI.new values
NAs <- is.na(EPI.new) #records True values if the value is NA 
EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array


## Exercise 1
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) #making histogram
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new) #tracks individual data values

boxplot(EPI.new, APO.new) #box plot of epi and apo

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) #adding derivative lines for hist?
rug(EPI.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ")) #different bandwidth method
rug(EPI.new) 

x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

##Example 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new) 
   qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)





#Exercise 2A ~ do it again for 2 other variables in the EPI data set
#First will be ECO.new 
NAs <- is.na(ECO.new) #records True values if the value is NA 
ECO.new.noNAs <- ECO.new[!NAs] # filters out NA values, new array
summary(ECO.new)
fivenum(ECO.new,na.rm=TRUE)
stem(ECO.new) # stem and leaf plot 
hist(ECO.new) #making histogram
hist(ECO.new, seq(20., 85., 1.0), prob=TRUE) 
lines(density(ECO.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(ECO.new) #tracks individual data values

boxplot(ECO.new, ECO.old) #box plot of eco.ew and eco.old

hist(ECO.new, seq(20., 85., 1.0), prob=TRUE) 
lines(density(ECO.new,na.rm=TRUE,bw=1.)) #adding derivative lines for hist?
rug(ECO.new) 

hist(ECO.new, seq(20., 85., 1.0), prob=TRUE)
lines(density(ECO.new,na.rm=TRUE,bw="SJ")) #different bandwidth method
rug(ECO.new) 

x<-seq(20,85,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

plot(ecdf(ECO.new), do.points=FALSE, verticals=TRUE)
qqnorm(ECO.new); qqline(ECO.new) 
  qqplot(rnorm(250), ECO.new, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.new)

qqplot(rt(250, df = 5), ECO.new, xlab = "Q-Q plot for t dsn") 
qqline(ECO.new)

#Then ECO.old to have a comparison
NAs <- is.na(ECO.old) #records True values if the value is NA 
ECO.old.noNAs <- ECO.old[!NAs] # filters out NA values, new array
summary(ECO.old)
fivenum(ECO.old,na.rm=TRUE)
stem(ECO.old) # stem and leaf plot 
hist(ECO.old) #making histogram
hist(ECO.old, seq(20., 85., 1.0), prob=TRUE) 
lines(density(ECO.old,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(ECO.old) #tracks individual data values

boxplot(ECO.old, BDH.new) #box plot of bdh.new and eco.old

hist(ECO.old, seq(20., 85., 1.0), prob=TRUE) 
lines(density(ECO.old,na.rm=TRUE,bw=1.)) #adding derivative lines for hist?
rug(ECO.old) 

hist(ECO.old, seq(20., 85., 1.0), prob=TRUE)
lines(density(ECO.old,na.rm=TRUE,bw="SJ")) #different bandwidth method
rug(ECO.old) 

x<-seq(20,85,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

plot(ecdf(ECO.old), do.points=FALSE, verticals=TRUE)
qqnorm(ECO.old); qqline(ECO.old) 
  qqplot(rnorm(250), ECO.old, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.old)

qqplot(rt(250, df = 5), ECO.old, xlab = "Q-Q plot for t dsn") 
qqline(ECO.old)