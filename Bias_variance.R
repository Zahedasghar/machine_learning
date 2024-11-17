# Set seed for reproducibility
set.seed(123)

# Generate data: y = 3 + 2*x + noise
n <- 1000 # Number of samples
x <- rnorm(n, mean = 5, sd = 2)
y <- 3 + 2 * x + rnorm(n, mean = 0, sd = 1) # True linear model

# Create a function to estimate parameters and compute MSE, Variance, and Bias^2
compute_mse_decomposition <- function(n_samples, x, y, true_beta0, true_beta1) {
  predictions <- numeric(n)  # Store average predictions across all experiments
  mse_list <- numeric(1000)  # Store MSE for each experiment
  
  # Repeat the process multiple times to get empirical estimates
  for (i in 1:1000) {
    # Take a random sample
    indices <- sample(1:n, n_samples, replace = TRUE)
    x_sample <- x[indices]
    y_sample <- y[indices]
    
    # Fit a linear model
    model <- lm(y_sample ~ x_sample)
    beta_hat <- coef(model)
    
    # Estimate predictions based on sample
    y_pred <- beta_hat[1] + beta_hat[2] * x
    mse_list[i] <- mean((y_pred - (true_beta0 + true_beta1 * x))^2)  # MSE calculation
    predictions <- predictions + y_pred / 1000  # Average predictions across experiments
  }
  
  # True mean predictions
  y_true_mean <- true_beta0 + true_beta1 * x
  
  # Calculate final MSE (average over all experiments)
  mse <- mean(mse_list)
  
  # Bias (difference between expected predictions and true mean)
  bias_squared <- mean((predictions - y_true_mean) ^ 2)
  
  # Variance (average variance across experiments)
  variance <- mse - bias_squared  # Direct calculation to avoid discrepancies
  
  # Return the results
  return(list(MSE = mse, Variance = variance, Bias_Squared = bias_squared))
}

# True parameters
true_beta0 <- 3
true_beta1 <- 2

# Decomposition with a sample size of 50
result <- compute_mse_decomposition(n_samples = 50, x = x, y = y, true_beta0 = true_beta0, true_beta1 = true_beta1)
print(result)


# Install necessary packages if not already installed
# install.packages("plotly")

library(plotly)

# Set seed for reproducibility
set.seed(123)

# Generate data: y = 3 + 2*x + noise
n <- 1000 # Number of samples
x <- rnorm(n, mean = 5, sd = 2)
y <- 3 + 2 * x + rnorm(n, mean = 0, sd = 1) # True linear model

# Create a function to estimate parameters and compute MSE, Variance, and Bias^2
compute_mse_decomposition <- function(n_samples, x, y, true_beta0, true_beta1) {
  predictions <- numeric(n)  # Store average predictions across all experiments
  mse_list <- numeric(1000)  # Store MSE for each experiment
  
  # Repeat the process multiple times to get empirical estimates
  for (i in 1:1000) {
    # Take a random sample
    indices <- sample(1:n, n_samples, replace = TRUE)
    x_sample <- x[indices]
    y_sample <- y[indices]
    
    # Fit a linear model
    model <- lm(y_sample ~ x_sample)
    beta_hat <- coef(model)
    
    # Estimate predictions based on sample
    y_pred <- beta_hat[1] + beta_hat[2] * x
    mse_list[i] <- mean((y_pred - (true_beta0 + true_beta1 * x))^2)  # MSE calculation
    predictions <- predictions + y_pred / 1000  # Average predictions across experiments
  }
  
  # True mean predictions
  y_true_mean <- true_beta0 + true_beta1 * x
  
  # Calculate final MSE (average over all experiments)
  mse <- mean(mse_list)
  
  # Bias (difference between expected predictions and true mean)
  bias_squared <- mean((predictions - y_true_mean) ^ 2)
  
  # Variance (average variance across experiments)
  variance <- mse - bias_squared  # Direct calculation to avoid discrepancies
  
  # Return the results
  return(list(MSE = mse, Variance = variance, Bias_Squared = bias_squared))
}

# True parameters
true_beta0 <- 3
true_beta1 <- 2

# Create arrays to store results for different sample sizes
sample_sizes <- seq(10, 200, by = 10)
mse_results <- c()
variance_results <- c()
bias_squared_results <- c()

# Compute results for each sample size
for (size in sample_sizes) {
  result <- compute_mse_decomposition(n_samples = size, x = x, y = y, true_beta0 = true_beta0, true_beta1 = true_beta1)
  mse_results <- c(mse_results, result$MSE)
  variance_results <- c(variance_results, result$Variance)
  bias_squared_results <- c(bias_squared_results, result$Bias_Squared)
}

# Create interactive plot using plotly
fig <- plot_ly() %>%
  add_trace(x = sample_sizes, y = bias_squared_results, type = 'scatter', mode = 'lines+markers', name = 'Bias^2',
            line = list(color = 'blue')) %>%
  add_trace(x = sample_sizes, y = variance_results, type = 'scatter', mode = 'lines+markers', name = 'Variance',
            line = list(color = 'red')) %>%
  add_trace(x = sample_sizes, y = mse_results, type = 'scatter', mode = 'lines+markers', name = 'MSE',
            line = list(color = 'green')) %>%
  layout(title = 'Bias-Variance Tradeoff with Different Sample Sizes',
         xaxis = list(title = 'Sample Size'),
         yaxis = list(title = 'Error'),
         hovermode = 'x')

# Show the plot
fig




# Install necessary packages if not already installed
# install.packages("plotly")

library(plotly)

# Simulate Bias, Variance, and MSE for different model complexities
complexity <- seq(1, 10, length.out = 100)
bias_squared <- 1 / complexity  # Bias decreases as complexity increases
variance <- ((complexity - 1) ^ 2) / (max(complexity) ^ 2)  # Variance increases with complexity
mse <- bias_squared + variance  # MSE is the sum of Bias^2 and Variance

# Create interactive plot using plotly
fig <- plot_ly() %>%
  add_trace(x = complexity, y = bias_squared, type = 'scatter', mode = 'lines', name = 'Bias^2',
            line = list(color = 'blue')) %>%
  add_trace(x = complexity, y = variance, type = 'scatter', mode = 'lines', name = 'Variance',
            line = list(color = 'red')) %>%
  add_trace(x = complexity, y = mse, type = 'scatter', mode = 'lines', name = 'MSE',
            line = list(color = 'green')) %>%
  layout(title = 'Bias-Variance Tradeoff',
         xaxis = list(title = 'Model Complexity'),
         yaxis = list(title = 'Error'),
         hovermode = 'x')

# Show the plot
fig

