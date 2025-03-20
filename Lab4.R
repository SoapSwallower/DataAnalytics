library(ggplot2)
library(class)
library(caret)
setwd("/Users/tungstentoothpick/Downloads")

wine <- read.csv("wine.data", header = FALSE)
colnames(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity", "Magnesium", 
                    "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", "Proanthocyanins", 
                    "Color_Intensity", "Hue", "OD280_OD315", "Proline")

#colnames(wine) <- read.csv("wine.names", header = FALSE)
print(colnames(wine))

wine_features <- wine[,-1]  # excluding type for pc stuff
wine_scaled <- scale(wine_features)
pca_res <- prcomp(wine_scaled)

pc_data <- data.frame(Type = as.factor(wine$Type),
                      PC1 = pca_res$x[, 1],
                      PC2 = pca_res$x[, 2])

ggplot(pc_data, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Wine Data Projected onto PC1 and PC2",
       x = "PC1", y = "PC2") +
  theme_minimal()

pc1_loadings <- pca_res$rotation[, 1]
loadings_abs <- abs(pc1_loadings)
sorted_loadings <- sort(loadings_abs, decreasing = TRUE)
print("absolute loadings PC1")
print(sorted_loadings)

threshold <- 0.2
vars_to_keep <- names(loadings_abs[loadings_abs > threshold])
print("kept variables")
print(vars_to_keep)

#pca re
wine_reduced <- wine_features[, vars_to_keep]
wine_reduced_scaled <- scale(wine_reduced)
pca_reduced <- prcomp(wine_reduced_scaled)
print("Summary PCA")
print(summary(pca_reduced))

#knn
set.seed(123)
train_index <- sample(1:nrow(wine), size = 0.7 * nrow(wine))
wine_train <- wine[train_index, ]
wine_test <- wine[-train_index, ]

train_means <- apply(wine_train[,-1], 2, mean)
train_sds <- apply(wine_train[,-1], 2, sd)
wine_train_scaled <- scale(wine_train[,-1], center = train_means, scale = train_sds)
wine_test_scaled <- scale(wine_test[,-1], center = train_means, scale = train_sds)


knn_pred_orig <- knn(train = wine_train_scaled,
                     test = wine_test_scaled,
                     cl = wine_train$Type,
                     k = 5)

cm_orig <- table(Predicted = knn_pred_orig, Actual = wine_test$Type)
print("confusion matrix")
print(cm_orig)


pca_train <- prcomp(wine_train_scaled)
train_pcs <- pca_train$x[, 1:3]
test_pcs <- predict(pca_train, newdata = wine_test_scaled)[, 1:3]
#knn to pca
knn_pred_pca <- knn(train = train_pcs,
                    test = test_pcs,
                    cl = wine_train$Type,
                    k = 5)


cm_pca <- table(Predicted = knn_pred_pca, Actual = wine_test$Type)
print("Confusion matrix kNN on PC")
print(cm_pca)


cm_orig_metrics <- confusionMatrix(knn_pred_orig, as.factor(wine_test$Type))
cm_pca_metrics <- confusionMatrix(knn_pred_pca, as.factor(wine_test$Type))

print("Performance metrics for kNN on original")
print(cm_orig_metrics)

print("Performance metrics for kNN on first 3 PC")
print(cm_pca_metrics)
