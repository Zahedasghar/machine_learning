import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report
import matplotlib.pyplot as plt

# Load the data
psx = pd.read_csv("data/psx.csv")

# Convert date column FIRST (before cleaning column names)
psx['date_clean'] = pd.to_datetime(psx['Date'], format='%m/%d/%Y')

# Clean column names
psx.columns = psx.columns.str.lower().str.replace(' ', '_')

# Drop the original 'date' column (which was 'Date' before cleaning)
# Keep 'date_clean' and rename it to 'date'
if 'date' in psx.columns:
    psx = psx.drop('date', axis=1)
psx = psx.rename(columns={'date_clean': 'date'})

# Clean the 'change_%' column (remove % sign and convert to numeric)
psx['change_%'] = psx['change_%'].str.replace('%', '').astype(float)

# Clean the volume column (remove 'K', 'M' suffixes and convert)
def clean_volume(vol_str):
    if isinstance(vol_str, str):
        vol_str = vol_str.replace('-', '0')  # Handle missing values
        if 'K' in vol_str:
            return float(vol_str.replace('K', '')) * 1000
        elif 'M' in vol_str:
            return float(vol_str.replace('M', '')) * 1000000
        else:
            return float(vol_str)
    return vol_str

psx['vol.'] = psx['vol.'].apply(clean_volume)

# Sort by date (now it exists!)
psx = psx.sort_values('date').reset_index(drop=True)

# Create direction variable (1 for Up, 0 for Down)
psx['direction'] = (psx['change_%'] > 0).astype(int)

# Create lagged variables for prediction
psx['lag1'] = psx['change_%'].shift(1)
psx['lag2'] = psx['change_%'].shift(2)
psx['lag3'] = psx['change_%'].shift(3)
psx['lag4'] = psx['change_%'].shift(4)
psx['lag5'] = psx['change_%'].shift(5)

# Remove rows with missing values (due to lagging)
psx_clean = psx.dropna()

print("Data shape after cleaning:", psx_clean.shape)
print("Direction distribution:\n", psx_clean['direction'].value_counts())

# Split data into training and testing (pre-2024 vs 2024)
psx_clean['year'] = psx_clean['date'].dt.year
train_data = psx_clean[psx_clean['year'] < 2024]
test_data = psx_clean[psx_clean['year'] == 2024]

print(f"\nTraining data shape: {train_data.shape}")
print(f"Test data shape: {test_data.shape}")

# Prepare features and target
feature_cols = ['lag1', 'lag2', 'lag3', 'lag4', 'lag5']
X_train = train_data[feature_cols]
y_train = train_data['direction']
X_test = test_data[feature_cols]
y_test = test_data['direction']

# Fit logistic regression model
log_model = LogisticRegression(max_iter=1000)
log_model.fit(X_train, y_train)

# Make predictions
y_pred_train = log_model.predict(X_train)
y_pred_test = log_model.predict(X_test)

# Calculate accuracies
train_accuracy = accuracy_score(y_train, y_pred_train)
test_accuracy = accuracy_score(y_test, y_pred_test)

print(f"\nTraining Accuracy: {train_accuracy:.4f}")
print(f"Test Accuracy: {test_accuracy:.4f}")

# Confusion Matrix for test data
cm = confusion_matrix(y_test, y_pred_test)
print("\nConfusion Matrix (Test Data):")
print(cm)

# Classification Report
print("\nClassification Report (Test Data):")
print(classification_report(y_test, y_pred_test, target_names=['Down', 'Up']))

# Display model coefficients
print("\nModel Coefficients:")
for i, coef in enumerate(log_model.coef_[0]):
    print(f"Lag{i+1}: {coef:.4f}")
print(f"Intercept: {log_model.intercept_[0]:.4f}")

# Plot predictions vs actual
plt.figure(figsize=(12, 6))
plt.plot(test_data['date'], y_test.values, label='Actual Direction', alpha=0.7, marker='o')
plt.plot(test_data['date'], y_pred_test, label='Predicted Direction', alpha=0.7, marker='x')
plt.xlabel('Date')
plt.ylabel('Direction (0=Down, 1=Up)')
plt.title('PSX Direction: Actual vs Predicted (2024)')
plt.legend()
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# Display first few rows
print("\nFirst few rows of cleaned data:")
print(psx_clean.head())
