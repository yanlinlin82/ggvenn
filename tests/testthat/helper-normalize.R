# Helper function to normalize numeric data for cross-platform snapshot consistency
normalize_ggplot_data <- function(data, digits = 6) {
  if (is.list(data)) {
    # Recursively normalize list elements
    lapply(data, normalize_ggplot_data, digits = digits)
  } else if (is.data.frame(data)) {
    # Normalize numeric columns in data frames
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]] <- round(data[[col]], digits = digits)
      }
    }
    data
  } else if (is.numeric(data)) {
    # Normalize numeric vectors
    round(data, digits = digits)
  } else {
    # Return other types as-is
    data
  }
}

# Helper function to normalize ggplot_build output for snapshots
normalize_ggplot_build <- function(plot, digits = 6) {
  build_data <- ggplot2::ggplot_build(plot)$data
  normalize_ggplot_data(build_data, digits = digits)
}

# Helper function for ggvenn specific normalization
normalize_ggvenn_output <- function(plot, digits = 6) {
  normalize_ggplot_build(plot, digits = digits)
}
