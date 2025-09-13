# Minimal R package setup for the course.
pkgs <- c(
  "tidyverse","tidymodels","textrecipes","broom","glmnet","ranger","xgboost",
  "lightgbm","janitor","yardstick","vip","shapviz","arrow","sparklyr","quarto"
)
install_if_missing <- function(p){
  if(!requireNamespace(p, quietly = TRUE)) install.packages(p, dependencies = TRUE)
}
invisible(lapply(pkgs, install_if_missing))

message("If 'lightgbm' fails to build on your system, you may skip it.")
message("Consider using renv for reproducibility: install.packages('renv'); renv::init()")