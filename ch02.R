library(ISLR2)
library(tidyverse)
library(MASS)

data("Boston")
?Boston

dim(Boston)

glimpse(Boston)

pairs(Boston)


cor(Boston)
cor(Boston) %>%
  corrplot::corrplot()

# Assuming Boston dataset is loaded and available
# Calculate the correlation matrix
cor_matrix <- cor(Boston)

# Extract the correlation of 'crim' with all other variables
crim_correlations <- cor_matrix["crim", ]

# Sort the correlations in descending order
sorted_crim_correlations <- sort(crim_correlations, decreasing = TRUE)

# Display the sorted correlations
print(sorted_crim_correlations)


Boston %>%
  dplyr::select(crim, tax, ptratio) %>%
  summary()

library(tidyverse)

# Assuming the Boston dataset is available
# Summarize the range and identify high values for 'crim', 'tax', and 'ptratio'
summary_stats <- Boston %>%
  summarise(
    crim_min = min(crim),
    crim_max = max(crim),
    tax_min = min(tax),
    tax_max = max(tax),
    ptratio_min = min(ptratio),
    ptratio_max = max(ptratio)
  )

# Identify high values based on chosen thresholds (e.g., top 5% as high)
high_values <- Boston %>%
  summarise(
    high_crim = quantile(crim, 0.95),
    high_tax = quantile(tax, 0.95),
    high_ptratio = quantile(ptratio, 0.95)
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "threshold")

# Find the census tracts with particularly high values
high_tracts <- Boston %>%
  filter(
    crim > high_values$threshold[high_values$variable == "high_crim"] |
    tax > high_values$threshold[high_values$variable == "high_tax"] |
    ptratio > high_values$threshold[high_values$variable == "high_ptratio"]
  ) %>%
  dplyr::select(crim, tax, ptratio)

# Print the range of each predictor
print("Summary of Ranges:")
print(summary_stats)

# Print the high value thresholds for each variable
print("Thresholds for High Values (Top 5%):")
print(high_values)

# Display census tracts with high values
print("Census Tracts with High Values for 'crim', 'tax', or 'ptratio':")
print(head(high_tracts))


Boston %>%
  filter(chas == 1) %>%
  nrow()

Boston |> summarise(median(ptratio))

Boston %>%
  summarise(median_ptratio = median(ptratio))

# Find the census tract with the lowest median value of owner-occupied homes
lowest_medv_tract <- Boston %>%
  filter(medv == min(medv)) %>%
  dplyr::select(everything())

# Summarize the range of each predictor in the dataset
overall_ranges <- Boston %>%
  summarise(across(everything(), list(min = min, max = max)))

# Display the census tract with the lowest 'medv'
print("Census Tract with the Lowest Median Value of Owner-Occupied Homes:")
print(lowest_medv_tract)

# Display the overall ranges for comparison
print("Overall Ranges for Each Predictor:")
print(overall_ranges)


# Count the number of census tracts with more than 7 rooms per dwelling
count_more_than_7 <- Boston %>%
  filter(rm > 7) %>%
  summarise(count = n())

# Count the number of census tracts with more than 8 rooms per dwelling
count_more_than_8 <- Boston %>%
  filter(rm > 8) %>%
  summarise(count = n())

# Display the results
print("Number of census tracts averaging more than 7 rooms per dwelling:")
print(count_more_than_7)

print("Number of census tracts averaging more than 8 rooms per dwelling:")
print(count_more_than_8)

# Display details of census tracts with more than 8 rooms per dwelling
tracts_more_than_8 <- Boston %>%
  filter(rm > 8) %>%
  dplyr::select(everything())

print("Details of census tracts averaging more than 8 rooms per dwelling:")
print(tracts_more_than_8)
