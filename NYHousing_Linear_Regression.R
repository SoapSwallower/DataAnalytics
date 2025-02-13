library("ggplot2")
library("readr")
setwd("/Users/tungstentoothpick/Downloads")
## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(BEDS), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



ggplot(dataset, aes(x = (BEDS), y = log10(PRICE))) +
  geom_point()

dataset <- dataset[dataset$BEDS<15,] 
#removing abnormally high amount of beds, multi family homes accounted for
# i dont think 50 beds is a reasonable number of beds to keep in the dataset lol
dataset <- dataset[dataset$PRICE<5000000,] 
#avoiding manhattan co-ops that price at many millions but also have very few beds

ggplot(dataset, aes(x = (BEDS), y = (PRICE))) +
  geom_point()

ggplot(dataset, aes(x = (BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

#Baths
ggplot(dataset, aes(x = (BATH), y = log10(PRICE))) +
  geom_point()

dataset <- dataset[dataset$BATH<15,] 
#clean outlier 
dataset <- dataset[dataset$PRICE<5000000,] 

ggplot(dataset, aes(x = (BATH), y = (PRICE))) +
  geom_point()

ggplot(dataset, aes(x = (BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



#wait i dont think i needed to do this at all... i misread the assignment
library(ggplot2)
library(randomForest)


dataset$PRICE <- as.numeric(dataset$PRICE)

set.seed(71)
rf <- randomForest(PRICE ~ BATH, data = dataset, ntree = 500, importance = TRUE)


print(rf)
varImpPlot(rf)

library(rpart)
library(rpart.plot)

tree_model <- rpart(PRICE ~ BATH, data = dataset)
rpart.plot(tree_model)