# Install required packages (if not already installed)
#install.packages(c("rpart", "rpart.plot", "caret", "dplyr"))

# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# Step 1: Load the HMDA data
library(haven)
# Read stata data

hmda_data <- read_dta("data/hmda_sw.dta")


# Step 2: Explore the dataset
str(hmda_data)
summary(hmda_data)
head(hmda_data)
colnames(hmda_data)

# Step 3: Data Cleaning and Preprocessing
# Convert target variable and relevant columns to appropriate types
hmda_data$loan_approved <- as.factor(hmda_data$loan_approved)
hmda_data$race <- as.factor(hmda_data$race)
hmda_data$income <- as.numeric(hmda_data$income)

# Optional: Handle missing values (remove rows with missing values)
hmda_data <- na.omit(hmda_data)

# Step 4: Split the Data into Training and Testing Sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(hmda_data$loan_approved, p = 0.7, list = FALSE)
train_data <- hmda_data[train_index, ]
test_data <- hmda_data[-train_index, ]

# Step 5: Build the CART Model
cart_model <- rpart(loan_approved ~ ., data = train_data, method = "class")

# Step 6: Visualize the Decision Tree
rpart.plot(cart_model, main="CART Model for HMDA Data", type=2, extra=104)

# Step 7: Make Predictions
predictions <- predict(cart_model, test_data, type = "class")

# Step 8: Evaluate the Model using Confusion Matrix
conf_matrix <- confusionMatrix(predictions, test_data$loan_approved)
print(conf_matrix)

# Step 9: Tune the CART Model (optional for better performance)
# Adjusting the complexity parameter (cp) to avoid overfitting
cart_tuned <- rpart(loan_approved ~ ., data = train_data, method = "class",
                    control = rpart.control(cp = 0.01))

# Visualize the pruned tree
rpart.plot(cart_tuned, main="Tuned CART Model")

# Step 10: Make Predictions with the Tuned Model
predictions_tuned <- predict(cart_tuned, test_data, type = "class")

# Evaluate the tuned model
conf_matrix_tuned <- confusionMatrix(predictions_tuned, test_data$loan_approved)
print(conf_matrix_tuned)
