# Load libraries
library(tidyverse)
library(caret)
library(Metrics)
library(e1071)        
library(randomForest)
setwd("/Users/tungstentoothpick/Downloads")
df <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv", stringsAsFactors = FALSE)
df <- df %>%
  mutate(GROSS.SQUARE.FEET = as.numeric(gsub(",", "", GROSS.SQUARE.FEET))) %>%
  filter(SALE.PRICE > 0, GROSS.SQUARE.FEET > 0)

# outliers
q_low <- quantile(df$SALE.PRICE, 0.01)
q_high <- quantile(df$SALE.PRICE, 0.99)
df <- df %>%
  filter(SALE.PRICE >= q_low, SALE.PRICE <= q_high)

# columns
df_model <- df %>%
  select(SALE.PRICE, GROSS.SQUARE.FEET) %>%
  drop_na()

# Train-test
set.seed(42)
trainIndex <- createDataPartition(df_model$SALE.PRICE, p = 0.8, list = FALSE)
train <- df_model[trainIndex, ]
test <- df_model[-trainIndex, ]

# -----------------------------
# lin regression
model_lm <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET, data = train)
pred_lm <- predict(model_lm, test)

# -----------------------------
# random forest
model_rf <- randomForest(SALE.PRICE ~ GROSS.SQUARE.FEET, data = train, ntree = 100)
pred_rf <- predict(model_rf, test)

# -----------------------------
# svr
model_svr <- svm(SALE.PRICE ~ GROSS.SQUARE.FEET, data = train)
pred_svr <- predict(model_svr, test)

#evaluate
evaluate_model <- function(true, predicted, name) {
  cat("\n📊", name, "\n")
  cat("MAE :", mae(true, predicted), "\n")
  cat("MSE :", mse(true, predicted), "\n")
  cat("RMSE:", rmse(true, predicted), "\n")
}
evaluate_model(test$SALE.PRICE, pred_lm, "Linear Regression")
evaluate_model(test$SALE.PRICE, pred_rf, "Random Forest")
evaluate_model(test$SALE.PRICE, pred_svr, "Support Vector Regression")
  

library(ggplot2)

# Create combined dataframe for plotting
plot_df <- data.frame(
  Actual = rep(test$SALE.PRICE, 3),
  Predicted = c(pred_lm, pred_rf, pred_svr),
  Model = factor(rep(c("Linear Regression", "Random Forest", "SVR"),
                     each = length(test$SALE.PRICE)))
)


ggplot(plot_df, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~Model, scales = "free") +
  labs(
    title = "Predicted vs Actual Price by Model",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
