library(caret)      # For model evaluation and cross-validation
library(rpart)      # For Decision Trees
library(tidyverse)  # For data manipulation and visualization
library(Metrics)    # For additional evaluation metrics
library(janitor)    # For cleaning column names
library(MASS)
library(class)      # For KNN
library(boot)

psx <- read_csv("data/psx.csv")

# Inspect the structure of the dataset
dim(psx)
glimpse(psx)

# Convert the `Date` column to proper Date format and clean column names
psx <- psx %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(-Date) %>%
  clean_names()

# Create `change_percent` as numeric and a binary `direction` column
psx <- psx %>%
  mutate(
    change_percent = as.numeric(str_remove(change_percent, "%")),
    direction = if_else(change_percent > 0, "Up", "Down")
  )

# Arrange by ascending date
psx <- psx %>% arrange(date)

# Generate lag variables and convert `vol` to numeric (account for M/K suffixes)
psx <- psx %>%
  mutate(
    lag1 = lag(change_percent, 1),
    lag2 = lag(change_percent, 2),
    lag3 = lag(change_percent, 3),
    lag4 = lag(change_percent, 4),
    lag5 = lag(change_percent, 5),
    vol = as.numeric(str_replace_all(vol, c("M" = "e6", "K" = "e3")))
  )

# Drop rows with missing values
psx_clean <- psx %>% drop_na()

# Convert `direction` to a factor for classification
psx_clean <- psx_clean %>%
  mutate(direction = factor(if_else(direction == "Up", 1, 0), levels = c(0, 1), labels = c("Down", "Up")))

# Define custom summary function to include confusion matrix and accuracy
logistic_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- confusionMatrix(data$pred, data$obs)
  out <- c(Accuracy = confusion$overall["Accuracy"])
  return(out)
}

# Define trainControl for different cross-validation methods
train_control_loocv <- trainControl(method = "LOOCV", summaryFunction = logistic_summary, classProbs = TRUE)
train_control_5fold <- trainControl(method = "cv", number = 5, summaryFunction = logistic_summary, classProbs = TRUE)
train_control_10fold <- trainControl(method = "cv", number = 10, summaryFunction = logistic_summary, classProbs = TRUE)

# Train logistic regression models
set.seed(123)
model_loocv <- train(
  direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
  data = psx_clean,
  method = "glm",
  family = binomial,
  trControl = train_control_loocv,
  metric = "Accuracy"
)

model_5fold <- train(
  direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
  data = psx_clean,
  method = "glm",
  family = binomial,
  trControl = train_control_5fold,
  metric = "Accuracy"
)

model_10fold <- train(
  direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
  data = psx_clean,
  method = "glm",
  family = binomial,
  trControl = train_control_10fold,
  metric = "Accuracy"
)

# Combine cross-validation results into a summary data frame
results_train <- data.frame(
  Method = c("LOOCV", "5-Fold CV", "10-Fold CV"),
  Training_Accuracy = c(
    model_loocv$results$Accuracy[1],
    model_5fold$results$Accuracy[1],
    model_10fold$results$Accuracy[1]
  )
)

print(results_train)

# Train-test split based on date
train_data <- psx_clean %>% filter(date < as.Date("2024-01-01"))
test_data <- psx_clean %>% filter(date >= as.Date("2024-01-01"))

# Predictions and accuracy on the test set
loocv_preds <- predict(model_loocv, newdata = test_data)
loocv_test_accuracy <- mean(loocv_preds == test_data$direction)

fold5_preds <- predict(model_5fold, newdata = test_data)
fold5_test_accuracy <- mean(fold5_preds == test_data$direction)

fold10_preds <- predict(model_10fold, newdata = test_data)
fold10_test_accuracy <- mean(fold10_preds == test_data$direction)

# Summarize training and testing accuracies
results <- data.frame(
  Method = c("LOOCV", "5-Fold CV", "10-Fold CV"),
  Training_Accuracy = results_train$Training_Accuracy,
  Test_Accuracy = c(loocv_test_accuracy, fold5_test_accuracy, fold10_test_accuracy)
)

print(results)

# Confusion matrices for the test set
confusionMatrix(data = loocv_preds, reference = test_data$direction, positive = "Up")
confusionMatrix(data = fold5_preds, reference = test_data$direction, positive = "Up")
confusionMatrix(data = fold10_preds, reference = test_data$direction, positive = "Up")
