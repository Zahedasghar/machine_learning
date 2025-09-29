# ============================================
# ARDL Export Demand: Baseline (no volatility)
# --------------------------------------------
# - Auto-detect macro column names
# - Handle non-syntactic column names safely
# - Loop over all numeric export series as dependents
# - Try multiple ARDL orders; robust fallbacks
# - Output: ARDL_Baseline_Results.csv + ARDL_Baseline_Publication_Table.csv
# ============================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ARDL)
})

# ---- Load Data ----
# Adjust path if needed
data_path <- "data/exports data 25-9.csv"
export <- read_csv(data_path, show_col_types = FALSE)

cat(sprintf("Data loaded: %d rows, %d columns\n", nrow(export), ncol(export)))

# ---- Helper: choose first existing column from candidate aliases ----
pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# ---- Auto-detect macro names (baseline only) ----
col_ipi_japan <- pick_col(export, c("ipi_japan", "ipi__japan", "ipi___japan"))
col_ipi_india <- pick_col(export, c("ipi_india", "ipi__india"))
col_exch      <- pick_col(export, c("ind_jap_bila_exch", "ind_jap__bila_exch", "ind_jap__Bila_exch"))

missing_macros <- c()
if (is.na(col_ipi_japan)) missing_macros <- c(missing_macros, "ipi_japan (e.g., ipi__japan)")
if (is.na(col_ipi_india)) missing_macros <- c(missing_macros, "ipi_india")
if (is.na(col_exch))      missing_macros <- c(missing_macros, "ind_jap_bila_exch (e.g., ind_jap__Bila_exch)")

if (length(missing_macros) > 0) {
  stop(paste0(
    "Missing required macro columns: ",
    paste(missing_macros, collapse = ", "),
    "\nAvailable columns are:\n",
    paste(names(export), collapse = ", ")
  ))
}

independent_vars <- c(col_ipi_japan, col_ipi_india, col_exch)
cat("Using macro columns (baseline):\n")
print(independent_vars)

# ---- Identify dependent variables ----
# Exclude year, macros, and known non-export columns
drop_cols <- c("year", independent_vars, "volatility", "ex+", "ex-")
dep_candidates <- setdiff(names(export), drop_cols)

# Keep only numeric columns (ARDL needs numeric)
dep_candidates <- dep_candidates[sapply(export[dep_candidates], is.numeric)]
if (length(dep_candidates) == 0) {
  stop("No numeric dependent variable candidates found after exclusions.")
}

cat(sprintf("Found %d dependent variables (numeric, excluding year/macros/vol/ex+/ex-)\n",
            length(dep_candidates)))

# ---- Determine start year for ts (if 'year' exists and is numeric) ----
if ("year" %in% names(export) && is.numeric(export$year)) {
  start_year <- suppressWarnings(min(export$year, na.rm = TRUE))
  if (is.finite(start_year)) {
    cat(sprintf("Detected numeric 'year' column. TS start set to %d.\n", start_year))
  } else {
    start_year <- 1
    cat("Numeric 'year' not usable; TS start set to 1.\n")
  }
} else {
  # fallback (you can change to 1972 if you prefer)
  start_year <- 1
  cat("No numeric 'year' column detected; TS start set to 1.\n")
}

# ---- ARDL specifications to try (p, q1, q2, q3) ----
specs <- list(
  "ARDL(1,1,1,1)" = c(1,1,1,1),
  "ARDL(2,1,1,1)" = c(2,1,1,1),
  "ARDL(1,2,2,2)" = c(1,2,2,2),
  "ARDL(2,2,2,2)" = c(2,2,2,2)
)

# ---- Initialize result containers ----
results_master <- data.frame(
  Dependent_Variable = character(),
  ARDL_Specification = character(),
  AIC = numeric(),
  BIC = numeric(),
  Intercept_Coeff = numeric(), Intercept_SE = numeric(), Intercept_tStat = numeric(), Intercept_pValue = numeric(),
  IPI_Japan_Coeff = numeric(), IPI_Japan_SE = numeric(), IPI_Japan_tStat = numeric(), IPI_Japan_pValue = numeric(),
  IPI_India_Coeff = numeric(), IPI_India_SE = numeric(), IPI_India_tStat = numeric(), IPI_India_pValue = numeric(),
  Bilateral_ExchRate_Coeff = numeric(), Bilateral_ExchRate_SE = numeric(), Bilateral_ExchRate_tStat = numeric(), Bilateral_ExchRate_pValue = numeric(),
  Cointegration_Status = character(),
  stringsAsFactors = FALSE
)

# ---- Main loop over dependent variables ----
for (dep_var in dep_candidates) {

  cat("\n", paste(rep("=", 68), collapse = ""), "\n", sep = "")
  cat("Processing: ", dep_var, "\n", sep = "")
  cat(paste(rep("=", 68), collapse = ""), "\n")

  # Build analysis frame: dependent + macros
  ardl_df <- export[, c(dep_var, independent_vars)]

  # If any of these are non-numeric, coerce (safely)
  for (nm in names(ardl_df)) {
    if (!is.numeric(ardl_df[[nm]])) {
      ardl_df[[nm]] <- suppressWarnings(as.numeric(ardl_df[[nm]]))
    }
  }

  # Remove rows with NAs across used columns
  ardl_df <- ardl_df[stats::complete.cases(ardl_df), , drop = FALSE]

  # Guard for minimal sample size (ARDL unstable with very few obs)
  if (nrow(ardl_df) < 15) {
    cat("  Skipped (insufficient observations after cleaning).\n")
    results_master <- rbind(results_master, data.frame(
      Dependent_Variable = dep_var,
      ARDL_Specification = "Insufficient N",
      AIC = NA, BIC = NA,
      Intercept_Coeff = NA, Intercept_SE = NA, Intercept_tStat = NA, Intercept_pValue = NA,
      IPI_Japan_Coeff = NA, IPI_Japan_SE = NA, IPI_Japan_tStat = NA, IPI_Japan_pValue = NA,
      IPI_India_Coeff = NA, IPI_India_SE = NA, IPI_India_tStat = NA, IPI_India_pValue = NA,
      Bilateral_ExchRate_Coeff = NA, Bilateral_ExchRate_SE = NA, Bilateral_ExchRate_tStat = NA, Bilateral_ExchRate_pValue = NA,
      Cointegration_Status = "Not Tested",
      stringsAsFactors = FALSE
    ))
    next
  }

  # Build ts
  ardl_ts <- ts(ardl_df, start = c(start_year), frequency = 1)

  # Safe formula (backticks for non-syntactic names)
  rhs_terms <- paste(sprintf("`%s`", independent_vars), collapse = " + ")
  fmla <- as.formula(sprintf("`%s` ~ %s", dep_var, rhs_terms))

  # Model selection by AIC across candidate orders
  best_model <- NULL
  best_aic   <- Inf
  best_spec  <- "Failed"

  for (spec_name in names(specs)) {
    ord <- specs[[spec_name]]
    # Length of order must be 1 + K (dep + regressors)
    if (length(ord) != (1 + length(independent_vars))) next
    try({
      model_i <- ardl(fmla, data = ardl_ts, order = ord)
      aic_i   <- AIC(model_i)
      cat(spec_name, " - AIC: ", round(aic_i, 4), "\n", sep = "")
      if (is.finite(aic_i) && aic_i < best_aic) {
        best_aic  <- aic_i
        best_model <- model_i
        best_spec <- spec_name
      }
    }, silent = TRUE)
  }

  # Prepare result row
  new_row <- data.frame(
    Dependent_Variable = dep_var,
    ARDL_Specification = best_spec,
    AIC = if (is.finite(best_aic)) round(best_aic, 4) else NA_real_,
    BIC = NA_real_,
    Intercept_Coeff = NA, Intercept_SE = NA, Intercept_tStat = NA, Intercept_pValue = NA,
    IPI_Japan_Coeff = NA, IPI_Japan_SE = NA, IPI_Japan_tStat = NA, IPI_Japan_pValue = NA,
    IPI_India_Coeff = NA, IPI_India_SE = NA, IPI_India_tStat = NA, IPI_India_pValue = NA,
    Bilateral_ExchRate_Coeff = NA, Bilateral_ExchRate_SE = NA, Bilateral_ExchRate_tStat = NA, Bilateral_ExchRate_pValue = NA,
    Cointegration_Status = "Not Tested",
    stringsAsFactors = FALSE
  )

  if (!is.null(best_model)) {
    # BIC
    new_row$BIC <- tryCatch(round(BIC(best_model), 4), error = function(e) NA_real_)

    # Long-run multipliers (robust)
    # Long-run multipliers (robust)
    lr <- tryCatch(multipliers(best_model), error = function(e) NULL)
    if (!is.null(lr) && is.data.frame(lr)) {
      # Map terms safely to our output columns
      term_map <- list(
        "(Intercept)"   = c("Intercept_Coeff","Intercept_SE","Intercept_tStat","Intercept_pValue"),
        col_ipi_japan   = c("IPI_Japan_Coeff","IPI_Japan_SE","IPI_Japan_tStat","IPI_Japan_pValue"),
        col_ipi_india   = c("IPI_India_Coeff","IPI_India_SE","IPI_India_tStat","IPI_India_pValue"),
        col_exch        = c("Bilateral_ExchRate_Coeff","Bilateral_ExchRate_SE","Bilateral_ExchRate_tStat","Bilateral_ExchRate_pValue")
      )

      for (tname in names(term_map)) {
        row_idx <- which(as.character(lr$Term) == tname)
        if (length(row_idx) == 1) {
          cols <- term_map[[tname]]
          new_row[[cols[1]]] <- lr$Estimate[row_idx]
          new_row[[cols[2]]] <- lr$`Std. Error`[row_idx]
          new_row[[cols[3]]] <- lr$`t value`[row_idx]
          new_row[[cols[4]]] <- lr$`Pr(>|t|)`[row_idx]
        }
      }
    } else {
      cat("  Note: Could not extract long-run multipliers for ", dep_var, "\n", sep = "")
    }

    # Bounds F-test (Case II: restricted intercept, no trend)
    # If your use-case prefers Case III, change to case = 3.
    cstat <- tryCatch({
      bt <- bounds_f_test(best_model, case = 2)
      f_stat <- as.numeric(bt$statistic[1])
      lb <- bt$tab[1, 2]  # I(0) bound at 10% row? (package prints a table; row 1 typically 10%, adjust if needed)
      ub <- bt$tab[1, 3]  # I(1) bound
      if (!is.finite(f_stat) || !is.finite(lb) || !is.finite(ub)) "Test Failed"
      else if (f_stat > ub) "Cointegrated"
      else if (f_stat < lb) "No Cointegration"
      else "Inconclusive"
    }, error = function(e) "Test Failed")
    new_row$Cointegration_Status <- cstat
  }

  results_master <- rbind(results_master, new_row)
  cat("  Row added.\n")
}

# ---- Save main results ----
out_main <- "ARDL_Baseline_Results.csv"
write.csv(results_master, out_main, row.names = FALSE)
cat("\n✓ Saved: ", out_main, " (", file.size(out_main), " bytes)\n", sep = "")

# ---- Publication-style table (stars + SE in parentheses) ----
starify <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}
fmt_coef <- function(est, se, p) {
  if (all(is.na(c(est, se, p)))) return(NA_character_)
  paste0(round(est, 4), starify(p), " (", round(se, 4), ")")
}

pub_table <- data.frame(
  Dependent_Variable = results_master$Dependent_Variable,
  ARDL_Spec = results_master$ARDL_Specification,
  AIC = results_master$AIC,
  BIC = results_master$BIC,
  IPI_Japan          = mapply(fmt_coef,
                              results_master$IPI_Japan_Coeff,
                              results_master$IPI_Japan_SE,
                              results_master$IPI_Japan_pValue),
  IPI_India          = mapply(fmt_coef,
                              results_master$IPI_India_Coeff,
                              results_master$IPI_India_SE,
                              results_master$IPI_India_pValue),
  Bilateral_ExchRate = mapply(fmt_coef,
                              results_master$Bilateral_ExchRate_Coeff,
                              results_master$Bilateral_ExchRate_SE,
                              results_master$Bilateral_ExchRate_pValue),
  Cointegration = results_master$Cointegration_Status,
  stringsAsFactors = FALSE
)

out_pub <- "ARDL_Baseline_Publication_Table.csv"
write.csv(pub_table, out_pub, row.names = FALSE)
cat("✓ Saved: ", out_pub, " (", file.size(out_pub), " bytes)\n", sep = "")

# ---- Quick summary ----
successful <- sum(!is.na(results_master$AIC))
cat("\n----------------------------------------\n")
cat("SUMMARY\n")
cat("----------------------------------------\n")
cat("Successful models: ", successful, " / ", nrow(results_master), "\n", sep = "")
if (successful > 0) {
  for (nm in c("IPI_Japan","IPI_India","Bilateral_ExchRate")) {
    pcol <- paste0(nm, "_pValue")
    sig <- sum(results_master[[pcol]] < 0.05, na.rm = TRUE)
    tot <- sum(!is.na(results_master[[pcol]]))
    cat(sprintf("%-20s: %d / %d (p<0.05)\n", nm, sig, tot))
  }
}
cat("\nDone.\n")
