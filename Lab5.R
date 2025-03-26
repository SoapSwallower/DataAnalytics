setwd("/Users/tungstentoothpick/Downloads")
library(e1071) 
library(caret)  
library(ggplot2)

wine <- read.csv("wine.data", header = FALSE)
names(wine) <- c("Type", "Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", 
                 "Phenols", "Flavanoids", "Nonflavanoid", "Proanthocyanins", 
                 "ColorIntensity", "Hue", "OD", "Proline")

features <- c("Alcohol", "Flavanoids", "ColorIntensity")
wine_subset <- wine[, c("Type", features)]
wine_subset$Type <- as.factor(wine_subset$Type)


set.seed(123)
trainIndex <- createDataPartition(wine_subset$Type, p = 0.7, list = FALSE)
train_data <- wine_subset[trainIndex, ]
test_data  <- wine_subset[-trainIndex, ]




tune_linear <- tune.svm(Type ~ ., data = train_data, kernel = "linear", 
                        cost = 10^(-1:2))
best_linear <- tune_linear$best.model


tune_radial <- tune.svm(Type ~ ., data = train_data, kernel = "radial", 
                        cost = 10^(-1:2), gamma = c(0.5, 1, 2))
best_radial <- tune_radial$best.model


pred_linear <- predict(best_linear, newdata = test_data)
pred_radial <- predict(best_radial, newdata = test_data)


nb_model <- naiveBayes(Type ~ ., data = train_data)
pred_nb <- predict(nb_model, newdata = test_data)


precision_recall_f1 <- function(cm) {
  tab <- cm$table
  precisions <- diag(tab) / colSums(tab)
  recalls    <- diag(tab) / rowSums(tab)
  f1s        <- 2 * precisions * recalls / (precisions + recalls)
  
  list(precision = mean(precisions, na.rm = TRUE),
       recall    = mean(recalls, na.rm = TRUE),
       f1        = mean(f1s, na.rm = TRUE))
}


cm_linear <- confusionMatrix(pred_linear, test_data$Type)
cm_radial <- confusionMatrix(pred_radial, test_data$Type)
cm_nb     <- confusionMatrix(pred_nb, test_data$Type)


metrics_linear <- precision_recall_f1(cm_linear)
metrics_radial <- precision_recall_f1(cm_radial)
metrics_nb     <- precision_recall_f1(cm_nb)


cat("SVM (Linear Kernel) Performance:\n")
print(metrics_linear)
cat("\nSVM (Radial Kernel) Performance:\n")
print(metrics_radial)
cat("\nNaive Bayes Performance:\n")
print(metrics_nb)

ny_data <- read.csv("NY-House-Dataset.csv", header = TRUE)
head(ny_data)
svm_model <- svm(PRICE ~ PROPERTYSQFT, data = ny_data, type = "eps-regression")
lm_model <- lm(PRICE ~ PROPERTYSQFT, data = ny_data)


ny_data$svm_pred <- predict(svm_model, newdata = ny_data)
ny_data$lm_pred  <- predict(lm_model, newdata = ny_data)




p1 <- ggplot(ny_data, aes(x = PRICE, y = svm_pred)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "SVM Regression: Predicted vs. Real PRICE",
       x = "Real PRICE",
       y = "Predicted PRICE")


p2 <- ggplot(ny_data, aes(x = PRICE, y = lm_pred)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression: Predicted vs. Real PRICE",
       x = "Real PRICE",
       y = "Predicted PRICE")

ny_data$svm_resid <- ny_data$PRICE - ny_data$svm_pred
ny_data$lm_resid  <- ny_data$PRICE - ny_data$lm_pred


p3 <- ggplot(ny_data, aes(x = svm_pred, y = svm_resid)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "SVM Regression: Residuals vs. Fitted",
       x = "Fitted Values",
       y = "Residuals")

p4 <- ggplot(ny_data, aes(x = lm_pred, y = lm_resid)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression: Residuals vs. Fitted",
       x = "Fitted Values",
       y = "Residuals")

print(p1)
print(p2)
print(p3)
print(p4)
