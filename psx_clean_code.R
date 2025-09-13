# Load required libraries
library(quantmod)
library(tidyverse)
library(janitor)

# Read the data
psx <- read_csv("data/psx.csv")

# Inspect the data
dim(psx)
glimpse(psx)

# Convert the `Date` column to Date format (assuming "mm/dd/yyyy" format in the CSV)
psx <- psx %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(-Date) # Remove original Date column


names(psx
      )

glimpse(psx)
# Clean column names and inspect
psx <- psx %>%
  clean_names()

# Create a `direction` column based on `change_price` and `change_percent`
psx <- psx %>%
  mutate(

    change_percent = as.numeric(str_remove(change_percent, "%")),
    direction = if_else(change_percent > 0, "Up", "Down")
  )

# Arrange data by ascending order of date
psx <- psx %>%
  arrange(date)

# Create lag variables for `change_percent`
# Create lag1 to lag5 columns for `change_percent`
psx <- psx %>%
  mutate(
    lag1 = lag(change_percent, 1),
    lag2 = lag(change_percent, 2),
    lag3 = lag(change_percent, 3),
    lag4 = lag(change_percent, 4),
    lag5 = lag(change_percent, 5)
  )


# Convert `vol` to numeric (handling "M" for millions and "K" for thousands)
psx <- psx %>%
  mutate(vol = as.numeric(str_replace_all(vol, c("M" = "e6", "K" = "e3"))))

# Filter out missing values for logistic regression
psx_clean <- psx %>% drop_na()

# Convert direction to binary numeric values
psx_clean <- psx_clean %>%
  mutate(direction = if_else(direction == "Up", 1, 0))
# First ensure direction is properly encoded as numeric binary in psx_clean
psx_clean <- psx_clean %>%
  mutate(direction = as.numeric(direction))  # Should be 0 or 1

# Logistic regression model
glm_fit <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
               data = psx_clean, family = binomial)

# Model summary
summary(glm_fit)

# Predict probabilities and classify directions
psx_clean <- psx_clean %>%
  mutate(
    glm_probs = predict(glm_fit, type = "response"),
    glm_pred = as.numeric(glm_probs > 0.5)  # Convert to numeric 0/1
  )

# Evaluate model accuracy
confusion_matrix <- table(Predicted = psx_clean$glm_pred, Actual = psx_clean$direction)
accuracy <- mean(psx_clean$glm_pred == psx_clean$direction)

# Split data into train and test sets (based on date < 2024)
train <- psx_clean %>%
  filter(date < as.Date("2024-01-01"))

test <- psx_clean %>%
  filter(date >= as.Date("2024-01-01"))

# Train logistic regression on train data
glm_train <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
                 data = train, family = binomial)

# Predict on test data
test <- test %>%
  mutate(
    glm_probs = predict(glm_train, newdata = ., type = "response"),
    glm_pred = as.numeric(glm_probs > 0.5)  # Convert to numeric 0/1
  )

# Evaluate test accuracy
confusion_matrix_test <- table(Predicted = test$glm_pred, Actual = test$direction)
test_accuracy <- mean(test$glm_pred == test$direction)

# Create detailed evaluation metrics
train_metrics <- list(
  accuracy = accuracy,
  sensitivity = confusion_matrix[2,2] / sum(confusion_matrix[,2]),
  specificity = confusion_matrix[1,1] / sum(confusion_matrix[,1]),
  confusion_matrix = confusion_matrix
)

test_metrics <- list(
  accuracy = test_accuracy,
  sensitivity = confusion_matrix_test[2,2] / sum(confusion_matrix_test[,2]),
  specificity = confusion_matrix_test[1,1] / sum(confusion_matrix_test[,1]),
  confusion_matrix = confusion_matrix_test
)

# Output results
list(
  train_metrics = train_metrics,
  test_metrics = test_metrics
)





# LDA, QDA, KNN, Decision Trees -------------------------------------------

# Load required libraries

library(MASS)       # For LDA and QDA
library(class)      # For KNN
library(caret)      # For performance evaluation and train/test split
library(rpart)      # For Decision Trees


# Split data into train and test sets (based on date < 2024)

# Train-test split based on the date
train_data <- psx_clean %>%
  filter(date < as.Date("2024-01-01"))

test_data <- psx_clean %>%
  filter(date >= as.Date("2024-01-01"))

# Ensure direction is numeric (0 for Down, 1 for Up)
train_data <- train_data %>%
  mutate(direction = as.factor(direction))

test_data <- test_data %>%
  mutate(direction = as.factor(direction))


# Linear Discriminant Analysis (LDA)

# Fit LDA model

lda_fit <- lda(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
               data = train_data)

# Predict on test data

lda_pred <- predict(lda_fit, newdata = test_data)

# Evaluate LDA model

lda_accuracy <- mean(lda_pred$class == test_data$direction)

# Quadratic Discriminant Analysis (QDA)

# Fit QDA model

qda_fit <- qda(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
               data = train_data)

# Predict on test data

qda_pred <- predict(qda_fit, newdata = test_data)

# Evaluate QDA model

qda_accuracy <- mean(qda_pred$class == test_data$direction)


# K-Nearest Neighbors (KNN)

# Fit KNN model

knn_fit <- knn(train = train_data[, c("lag1", "lag2", "lag3", "lag4", "lag5", "vol")],
               test = test_data[, c("lag1", "lag2", "lag3", "lag4", "lag5", "vol")],
               cl = train_data$direction,
               k = 5)

# Evaluate KNN model

knn_accuracy <- mean(knn_fit == test_data$direction)

## Decision Trees

# Fit Decision Tree model

dt_fit <- rpart(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
                 data = train_data,
                 method = "class")

# Predict on test data

dt_pred <- predict(dt_fit, newdata = test_data, type = "class")


# Evaluate Decision Tree model

dt_accuracy <- mean(dt_pred == test_data$direction)


# Convert predictions and true labels to factors with the same levels
glm_pred <- factor(glm_pred, levels = c("0", "1"))
test_data$direction <- factor(test_data$direction, levels = c("0", "1"))

# Confusion Matrix and Accuracy
glm_cm <- confusionMatrix(glm_pred, test_data$direction)

# Extract Accuracy
glm_accuracy <- glm_cm$overall['Accuracy']

# Display results
glm_cm
glm_accuracy

# Output results

list(
  lda_accuracy = lda_accuracy,
  qda_accuracy = qda_accuracy,
  knn_accuracy = knn_accuracy,
  dt_accuracy = dt_accuracy
)
