ensure_dirs <- function(...) {
  dirs <- list(...)
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}