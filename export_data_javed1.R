# Direct ARDL Analysis with Guaranteed CSV Output
# Load required libraries
library(readr)
library(dplyr)
library(readxl)
library(janitor)
library(ARDL)

# Read excel file
export <- read_excel("D:/RepTemplates/freelancing/exports_and_impots_data_19-09-2025.xlsx",
                     sheet = "Exports data 19-07-2025")

# Clean column names
export <- clean_names(export)

# Read year as time and all other as numeric
export <- export %>%
  mutate(across(-year, as.numeric))

# Consider year as date
export$year <- as.Date(as.character(export$year), format = "%Y")

# Define dependent variables (columns 3- 10)

dep


dependent_vars <- c("anim_veg_oils_fats_processed",
                    "articles_of_artificial_plastic_mate",
                    "articles_of_paper_pulp_paperboard",
                    "cereal_preps_preps_of_flour_of_fr",
                    "chemical_materials_and_products_n")

# Define independent variables (last 4 columns)
independent_vars <- c("ipi_japan", "ipi_india", "ind_jap_bila_exch", "volatility")

# Remove rows with missing values in key variables
export_clean <- export[complete.cases(export[, c(dependent_vars, independent_vars)]), ]

cat("Working with", nrow(export_clean), "observations\n")

# Initialize the master results dataframe
results_master <- data.frame(
  Dependent_Variable = character(),
  ARDL_Specification = character(),
  AIC = numeric(),
  BIC = numeric(),

  # Intercept
  Intercept_Coeff = numeric(),
  Intercept_SE = numeric(),
  Intercept_tStat = numeric(),
  Intercept_pValue = numeric(),

  # IPI Japan
  IPI_Japan_Coeff = numeric(),
  IPI_Japan_SE = numeric(),
  IPI_Japan_tStat = numeric(),
  IPI_Japan_pValue = numeric(),

  # IPI India
  IPI_India_Coeff = numeric(),
  IPI_India_SE = numeric(),
  IPI_India_tStat = numeric(),
  IPI_India_pValue = numeric(),

  # Bilateral Exchange Rate
  Bilateral_ExchRate_Coeff = numeric(),
  Bilateral_ExchRate_SE = numeric(),
  Bilateral_ExchRate_tStat = numeric(),
  Bilateral_ExchRate_pValue = numeric(),

  # Volatility
  Volatility_Coeff = numeric(),
  Volatility_SE = numeric(),
  Volatility_tStat = numeric(),
  Volatility_pValue = numeric(),

  # Model diagnostics
  Cointegration_Status = character(),

  stringsAsFactors = FALSE
)

# Process each dependent variable
for (dep_var in dependent_vars) {

  cat("\n", rep("=", 60), "\n")
  cat("Processing:", dep_var, "\n")
  cat(rep("=", 60), "\n")

  # Prepare data
  ardl_data <- export_clean[, c(dep_var, independent_vars)]
  ardl_data <- ardl_data[complete.cases(ardl_data), ]

  # Convert to time series
  ardl_ts <- ts(ardl_data, start = c(1972), frequency = 1)

  # Create formula
  ardl_formula <- as.formula(paste(dep_var, "~", paste(independent_vars, collapse = " + ")))

  # Test different specifications
  specs <- list(
    "ARDL(1,1,1,1,1)" = c(1, 1, 1, 1, 1),
    "ARDL(2,1,1,1,1)" = c(2, 1, 1, 1, 1),
    "ARDL(1,2,2,2,2)" = c(1, 2, 2, 2, 2),
    "ARDL(2,2,2,2,2)" = c(2, 2, 2, 2, 2)
  )

  best_model <- NULL
  best_aic <- Inf
  best_spec <- ""

  # Find best model
  for (spec_name in names(specs)) {
    tryCatch({
      model <- ardl(ardl_formula, data = ardl_ts, order = specs[[spec_name]])
      current_aic <- AIC(model)

      cat(spec_name, "- AIC:", round(current_aic, 4), "\n")

      if (current_aic < best_aic) {
        best_aic <- current_aic
        best_model <- model
        best_spec <- spec_name
      }

    }, error = function(e) {
      cat(spec_name, "- Failed\n")
    })
  }

  # Initialize row with NAs
  new_row <- data.frame(
    Dependent_Variable = dep_var,
    ARDL_Specification = "Failed",
    AIC = NA,
    BIC = NA,

    Intercept_Coeff = NA, Intercept_SE = NA, Intercept_tStat = NA, Intercept_pValue = NA,
    IPI_Japan_Coeff = NA, IPI_Japan_SE = NA, IPI_Japan_tStat = NA, IPI_Japan_pValue = NA,
    IPI_India_Coeff = NA, IPI_India_SE = NA, IPI_India_tStat = NA, IPI_India_pValue = NA,
    Bilateral_ExchRate_Coeff = NA, Bilateral_ExchRate_SE = NA, Bilateral_ExchRate_tStat = NA, Bilateral_ExchRate_pValue = NA,
    Volatility_Coeff = NA, Volatility_SE = NA, Volatility_tStat = NA, Volatility_pValue = NA,

    Cointegration_Status = "Not Tested",
    stringsAsFactors = FALSE
  )

  if (!is.null(best_model)) {
    cat("\nBest model:", best_spec, "\n")

    # Get model statistics
    new_row$ARDL_Specification <- best_spec
    new_row$AIC <- round(best_aic, 4)
    new_row$BIC <- round(BIC(best_model), 4)

    # Extract long-run coefficients
    tryCatch({
      lr_coeffs <- multipliers(best_model)

      cat("Long-run coefficients extracted:\n")
      print(lr_coeffs)

      # Map coefficients to dataframe
      for (i in 1:nrow(lr_coeffs)) {
        term <- as.character(lr_coeffs$Term[i])
        coeff <- lr_coeffs$Estimate[i]
        se <- lr_coeffs$`Std. Error`[i]
        tstat <- lr_coeffs$`t value`[i]
        pval <- lr_coeffs$`Pr(>|t|)`[i]

        if (term == "(Intercept)") {
          new_row$Intercept_Coeff <- coeff
          new_row$Intercept_SE <- se
          new_row$Intercept_tStat <- tstat
          new_row$Intercept_pValue <- pval
        } else if (term == "ipi_japan") {
          new_row$IPI_Japan_Coeff <- coeff
          new_row$IPI_Japan_SE <- se
          new_row$IPI_Japan_tStat <- tstat
          new_row$IPI_Japan_pValue <- pval
        } else if (term == "ipi_india") {
          new_row$IPI_India_Coeff <- coeff
          new_row$IPI_India_SE <- se
          new_row$IPI_India_tStat <- tstat
          new_row$IPI_India_pValue <- pval
        } else if (term == "ind_jap_bila_exch") {
          new_row$Bilateral_ExchRate_Coeff <- coeff
          new_row$Bilateral_ExchRate_SE <- se
          new_row$Bilateral_ExchRate_tStat <- tstat
          new_row$Bilateral_ExchRate_pValue <- pval
        } else if (term == "volatility") {
          new_row$Volatility_Coeff <- coeff
          new_row$Volatility_SE <- se
          new_row$Volatility_tStat <- tstat
          new_row$Volatility_pValue <- pval
        }
      }

    }, error = function(e) {
      cat("Failed to extract long-run coefficients:", e$message, "\n")
    })

    # Test cointegration
    tryCatch({
      bounds_test <- bounds_f_test(best_model, case = 2)
      f_stat <- bounds_test$statistic
      upper_bound <- bounds_test$tab[1, 3]
      lower_bound <- bounds_test$tab[1, 2]

      if (f_stat > upper_bound) {
        new_row$Cointegration_Status <- "Cointegrated"
      } else if (f_stat < lower_bound) {
        new_row$Cointegration_Status <- "No Cointegration"
      } else {
        new_row$Cointegration_Status <- "Inconclusive"
      }

    }, error = function(e) {
      new_row$Cointegration_Status <- "Test Failed"
    })
  }

  # Add row to master results
  results_master <- rbind(results_master, new_row)

  cat("Row added to results\n")
}

# Display results
cat("\n", rep("=", 80), "\n")
cat("FINAL RESULTS SUMMARY\n")
cat(rep("=", 80), "\n")

print(results_master)

# Save main results
cat("\nSaving main results to CSV...\n")
write.csv(results_master, "ARDL_Complete_Results.csv", row.names = FALSE)

# Create simplified coefficient table
coeff_table <- results_master[, c("Dependent_Variable", "ARDL_Specification", "AIC",
                                  "IPI_Japan_Coeff", "IPI_Japan_pValue",
                                  "IPI_India_Coeff", "IPI_India_pValue",
                                  "Bilateral_ExchRate_Coeff", "Bilateral_ExchRate_pValue",
                                  "Volatility_Coeff", "Volatility_pValue")]

cat("Saving coefficient summary to CSV...\n")
write.csv(coeff_table, "ARDL_Coefficients_Summary.csv", row.names = FALSE)

# Create publication table with formatted coefficients
pub_table <- data.frame(
  Dependent_Variable = results_master$Dependent_Variable,
  ARDL_Spec = results_master$ARDL_Specification,
  AIC = results_master$AIC,

  IPI_Japan = paste0(
    round(results_master$IPI_Japan_Coeff, 4),
    ifelse(results_master$IPI_Japan_pValue < 0.01, "***",
           ifelse(results_master$IPI_Japan_pValue < 0.05, "**",
                  ifelse(results_master$IPI_Japan_pValue < 0.10, "*", ""))),
    " (", round(results_master$IPI_Japan_SE, 4), ")"
  ),

  IPI_India = paste0(
    round(results_master$IPI_India_Coeff, 4),
    ifelse(results_master$IPI_India_pValue < 0.01, "***",
           ifelse(results_master$IPI_India_pValue < 0.05, "**",
                  ifelse(results_master$IPI_India_pValue < 0.10, "*", ""))),
    " (", round(results_master$IPI_India_SE, 4), ")"
  ),

  Bilateral_ExchRate = paste0(
    round(results_master$Bilateral_ExchRate_Coeff, 4),
    ifelse(results_master$Bilateral_ExchRate_pValue < 0.01, "***",
           ifelse(results_master$Bilateral_ExchRate_pValue < 0.05, "**",
                  ifelse(results_master$Bilateral_ExchRate_pValue < 0.10, "*", ""))),
    " (", round(results_master$Bilateral_ExchRate_SE, 4), ")"
  ),

  Volatility = paste0(
    round(results_master$Volatility_Coeff, 4),
    ifelse(results_master$Volatility_pValue < 0.01, "***",
           ifelse(results_master$Volatility_pValue < 0.05, "**",
                  ifelse(results_master$Volatility_pValue < 0.10, "*", ""))),
    " (", round(results_master$Volatility_SE, 4), ")"
  ),

  Cointegration = results_master$Cointegration_Status,
  stringsAsFactors = FALSE
)

cat("Saving publication-ready table to CSV...\n")
write.csv(pub_table, "ARDL_Publication_Table.csv", row.names = FALSE)

# Check files were created
cat("\nFiles created:\n")
if (file.exists("ARDL_Complete_Results.csv")) {
  cat("✓ ARDL_Complete_Results.csv (", file.size("ARDL_Complete_Results.csv"), " bytes)\n")
} else {
  cat("✗ ARDL_Complete_Results.csv not created\n")
}

if (file.exists("ARDL_Coefficients_Summary.csv")) {
  cat("✓ ARDL_Coefficients_Summary.csv (", file.size("ARDL_Coefficients_Summary.csv"), " bytes)\n")
} else {
  cat("✗ ARDL_Coefficients_Summary.csv not created\n")
}

if (file.exists("ARDL_Publication_Table.csv")) {
  cat("✓ ARDL_Publication_Table.csv (", file.size("ARDL_Publication_Table.csv"), " bytes)\n")
} else {
  cat("✗ ARDL_Publication_Table.csv not created\n")
}

# Display quick summary
cat("\n", rep("-", 60), "\n")
cat("QUICK SUMMARY\n")
cat(rep("-", 60), "\n")

successful_models <- sum(!is.na(results_master$AIC))
cat("Successfully fitted models:", successful_models, "out of", nrow(results_master), "\n")

if (successful_models > 0) {
  cat("\nSignificant coefficients (p < 0.05):\n")

  vars <- c("IPI_Japan", "IPI_India", "Bilateral_ExchRate", "Volatility")
  for (var in vars) {
    pval_col <- paste0(var, "_pValue")
    sig_count <- sum(results_master[[pval_col]] < 0.05, na.rm = TRUE)
    total_count <- sum(!is.na(results_master[[pval_col]]))
    cat(var, ":", sig_count, "/", total_count, "\n")
  }
}

cat("\nAnalysis completed! Check the CSV files for detailed results.\n")

