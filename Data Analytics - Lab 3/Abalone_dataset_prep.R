###################
##### Abalone #####
###################

library(e1071) 
library(caTools) 
library(class) 
library(ggplot2)


setwd("/Users/tungstentoothpick/Downloads/Data Analytics F25 - Lab 3-selected")

# read dataset
abalone <- read.csv("/Users/tungstentoothpick/Downloads/Data Analytics F25 - Lab 3-selected/abalone_dataset.csv")
dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

str(dataset)
head(dataset)


split <- sample.split(dataset, SplitRatio = 0.7) 
train_cl <- subset(dataset, split == "TRUE") 
test_cl <- subset(dataset, split == "FALSE") 

train_scale <- scale(train_cl[, 2:5]) 
test_scale <- scale(test_cl[, 2:5]) 


head(train_scale)
head(test_scale)

nrow(train_scale)       # Should be equal to:
length(train_cl$age.group)  # ... this number


# Fitting KNN Model to training dataset 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 1) 
classifier_knn 

# Confusiin Matrix 
cm <- table(test_cl$age.group, classifier_knn) 
cm 

# Calculate out of Sample error 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 3 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 3) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 


# K = 5
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 5) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 


# K = 7 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 7) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 11 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 11) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 19 
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 19) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 60
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 60) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 

# K = 80
classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$age.group, 
                      k = 80) 
misClassError <- mean(classifier_knn != test_cl$age.group) 
print(paste('Accuracy =', 1-misClassError)) 

#kmeans
set.seed(123)


k_means_result <- kmeans(train_scale, centers = 3, nstart = 25)
print(k_means_result)

cluster_comparison <- table(train_cl$age.group, k_means_result$cluster)
print(cluster_comparison)

pca <- prcomp(train_scale, center = TRUE, scale. = TRUE)
pca_df <- data.frame(pca$x[, 1:2], Cluster = as.factor(k_means_result$cluster),
                     AgeGroup = train_cl$age.group)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, shape = AgeGroup)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on Abalone Data (PCA Projection)",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()





