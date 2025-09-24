# Load necessary libraries
#install.packages("caret")
library(caret)
library(MLmetrics)

# Load data (example: iris dataset)
data(iris)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Train a decision tree model
model <- train(Species ~ ., data = trainData, method = "rpart")

# Predict and evaluate the model
predictions <- predict(model, newdata = testData)
confusionMatrix(predictions, testData$Species)

# Hyperparameter tuning
grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))  # Example grid for 'cp'
model_tuned <- train(
  Species ~ .,
  data = trainData,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),  # Cross-validation
  tuneGrid = grid
)

# Print and plot feature importance
importance <- varImp(model_tuned)
print(importance)
plot(importance)

# Additional evaluation metric (example: F1 score)
F1_Score(predictions, testData$Species, positive = "setosa")  # Adjust positive class as needed

# Train and compare with another model (Random Forest)
rf_model <- train(
  Species ~ .,
  data = trainData,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

# Compare performance of different models
resamples_list <- resamples(list(DecisionTree = model_tuned, RandomForest = rf_model))
summary(resamples_list)

# Save and load the model
saveRDS(model_tuned, file = "model_tuned.rds")
loaded_model <- readRDS("model_tuned.rds")

