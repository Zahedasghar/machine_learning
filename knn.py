import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, cross_val_score # sklearn is a dependency
from sklearn.linear_model import LinearRegression
from sklearn.neighbors import KNeighborsRegressor
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

# Set random seed for reproducibility
np.random.seed(42)

# Generate synthetic data similar to ISLR2 examples
n_samples = 400
X = np.random.uniform(-3, 3, (n_samples, 1))

# Create non-linear relationship (similar to ISLR2 Figure 3.1)
true_function = lambda x: x**2 + 0.5*x + np.sin(2*x)
y = true_function(X.ravel()) + np.random.normal(0, 0.5, n_samples)

# Create DataFrame for easier handling
df = pd.DataFrame({'x': X.ravel(), 'y': y})

# Split the data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# 1. Linear Regression
linear_reg = LinearRegression()
linear_reg.fit(X_train, y_train)

# 2. KNN Regression with different k values
k_values = [1, 5, 10, 20, 50]
knn_models = {}
for k in k_values:
    knn_models[k] = KNeighborsRegressor(n_neighbors=k)
    knn_models[k].fit(X_train, y_train)

# Generate predictions for plotting
X_plot = np.linspace(-3, 3, 300).reshape(-1, 1)
y_true_plot = true_function(X_plot.ravel())

# Linear regression predictions
y_linear_pred = linear_reg.predict(X_plot)

# KNN predictions
knn_predictions = {}
for k in k_values:
    knn_predictions[k] = knn_models[k].predict(X_plot)

# Create visualization comparing methods
fig, axes = plt.subplots(2, 3, figsize=(18, 12))
fig.suptitle('KNN vs Linear Regression: ISLR2 Style Comparison', fontsize=16)

# Plot 1: Data and true function
ax = axes[0, 0]
ax.scatter(X_train, y_train, alpha=0.6, s=20, label='Training Data')
ax.plot(X_plot, y_true_plot, 'r-', linewidth=2, label='True Function')
ax.set_title('Training Data and True Function')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.legend()
ax.grid(True, alpha=0.3)

# Plot 2: Linear Regression
ax = axes[0, 1]
ax.scatter(X_train, y_train, alpha=0.6, s=20, color='lightblue', label='Training Data')
ax.plot(X_plot, y_true_plot, 'r-', linewidth=2, label='True Function')
ax.plot(X_plot, y_linear_pred, 'g--', linewidth=2, label='Linear Regression')
ax.set_title('Linear Regression')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.legend()
ax.grid(True, alpha=0.3)

# Plot 3: KNN with k=1 (high variance)
ax = axes[0, 2]
ax.scatter(X_train, y_train, alpha=0.6, s=20, color='lightblue', label='Training Data')
ax.plot(X_plot, y_true_plot, 'r-', linewidth=2, label='True Function')
ax.plot(X_plot, knn_predictions[1], 'orange', linewidth=2, label='KNN (k=1)')
ax.set_title('KNN Regression (k=1)')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.legend()
ax.grid(True, alpha=0.3)

# Plot 4: KNN with k=10 (moderate smoothing)
ax = axes[1, 0]
ax.scatter(X_train, y_train, alpha=0.6, s=20, color='lightblue', label='Training Data')
ax.plot(X_plot, y_true_plot, 'r-', linewidth=2, label='True Function')
ax.plot(X_plot, knn_predictions[10], 'purple', linewidth=2, label='KNN (k=10)')
ax.set_title('KNN Regression (k=10)')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.legend()
ax.grid(True, alpha=0.3)

# Plot 5: KNN with k=50 (high smoothing)
ax = axes[1, 1]
ax.scatter(X_train, y_train, alpha=0.6, s=20, color='lightblue', label='Training Data')
ax.plot(X_plot, y_true_plot, 'r-', linewidth=2, label='True Function')
ax.plot(X_plot, knn_predictions[50], 'brown', linewidth=2, label='KNN (k=50)')
ax.set_title('KNN Regression (k=50)')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.legend()
ax.grid(True, alpha=0.3)

# Plot 6: Comparison of all methods
ax = axes[1, 2]
ax.plot(X_plot, y_true_plot, 'r-', linewidth=3, label='True Function')
ax.plot(X_plot, y_linear_pred, 'g--', linewidth=2, label='Linear Regression')
ax.plot(X_plot, knn_predictions[1], 'orange', alpha=0.7, label='KNN (k=1)')
ax.plot(X_plot, knn_predictions[10], 'purple', linewidth=2, label='KNN (k=10)')
ax.plot(X_plot, knn_predictions[50], 'brown', linewidth=2, label='KNN (k=50)')
ax.set_title('All Methods Comparison')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.legend()
ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

# Calculate performance metrics
print("Model Performance Comparison (Test Set)")
print("="*50)

# Linear Regression
y_linear_test_pred = linear_reg.predict(X_test)
linear_mse = mean_squared_error(y_test, y_linear_test_pred)
linear_r2 = r2_score(y_test, y_linear_test_pred)

print(f"Linear Regression:")
print(f"  Test MSE: {linear_mse:.3f}")
print(f"  Test R²:  {linear_r2:.3f}")
print()

# KNN models
knn_results = {}
for k in k_values:
    y_knn_test_pred = knn_models[k].predict(X_test)
    knn_mse = mean_squared_error(y_test, y_knn_test_pred)
    knn_r2 = r2_score(y_test, y_knn_test_pred)
    knn_results[k] = {'mse': knn_mse, 'r2': knn_r2}
    
    print(f"KNN (k={k}):")
    print(f"  Test MSE: {knn_mse:.3f}")
    print(f"  Test R²:  {knn_r2:.3f}")
    print()

# Cross-validation comparison
print("Cross-Validation Results (5-fold)")
print("="*40)

cv_scores_linear = cross_val_score(LinearRegression(), X_train, y_train, 
                                  cv=5, scoring='neg_mean_squared_error')
print(f"Linear Regression CV MSE: {-cv_scores_linear.mean():.3f} (±{cv_scores_linear.std():.3f})")

for k in k_values:
    cv_scores_knn = cross_val_score(KNeighborsRegressor(n_neighbors=k), X_train, y_train,
                                   cv=5, scoring='neg_mean_squared_error')
    print(f"KNN (k={k}) CV MSE: {-cv_scores_knn.mean():.3f} (±{cv_scores_knn.std():.3f})")

# Bias-Variance Trade-off visualization
k_range = range(1, 51)
mse_values = []

for k in k_range:
    knn = KNeighborsRegressor(n_neighbors=k)
    cv_scores = cross_val_score(knn, X_train, y_train, cv=5, scoring='neg_mean_squared_error')
    mse_values.append(-cv_scores.mean())

plt.figure(figsize=(12, 6))

plt.subplot(1, 2, 1)
plt.plot(k_range, mse_values, 'b-', linewidth=2, marker='o', markersize=4)
plt.axhline(y=-cv_scores_linear.mean(), color='r', linestyle='--', 
            linewidth=2, label='Linear Regression')
plt.xlabel('Number of Neighbors (k)')
plt.ylabel('Cross-Validation MSE')
plt.title('KNN: Bias-Variance Trade-off')
plt.legend()
plt.grid(True, alpha=0.3)

# Find optimal k
optimal_k = k_range[np.argmin(mse_values)]
plt.axvline(x=optimal_k, color='g', linestyle=':', alpha=0.7, 
            label=f'Optimal k={optimal_k}')
plt.legend()

# Feature comparison plot
plt.subplot(1, 2, 2)
methods = ['Linear\nRegression'] + [f'KNN\n(k={k})' for k in [1, 5, 10, 20, 50]]
test_mse_values = [linear_mse] + [knn_results[k]['mse'] for k in [1, 5, 10, 20, 50]]

bars = plt.bar(methods, test_mse_values, color=['green', 'orange', 'purple', 'blue', 'red', 'brown'])
plt.ylabel('Test Set MSE')
plt.title('Test Set Performance Comparison')
plt.xticks(rotation=45)

# Highlight best performing method
best_idx = np.argmin(test_mse_values)
bars[best_idx].set_color('gold')
bars[best_idx].set_edgecolor('black')
bars[best_idx].set_linewidth(2)

plt.tight_layout()
plt.show()

# Summary insights
print("\nKey Insights (ISLR2 Style Analysis):")
print("="*50)
print("1. Flexibility vs. Interpretability:")
print("   - Linear Regression: Low flexibility, high interpretability")
print("   - KNN: High flexibility, low interpretability")
print()
print("2. Bias-Variance Trade-off:")
print("   - Small k (e.g., k=1): Low bias, high variance (overfitting)")
print("   - Large k (e.g., k=50): High bias, low variance (underfitting)")
print(f"   - Optimal k: {optimal_k} (best cross-validation performance)")
print()
print("3. Non-linear Relationships:")
print("   - Linear regression struggles with non-linear patterns")
print("   - KNN can capture non-linear relationships naturally")
print()
print("4. Computational Complexity:")
print("   - Linear Regression: O(p) for prediction (where p = features)")
print("   - KNN: O(n) for prediction (where n = training samples)")