#' Utility function for data type conversion.
#'
#' @name list_to_data_frame
#' @param x A list of sets.
#' @return A data.frame with logical columns representing sets.
#' @examples
#' a <- list(A = 1:5, B = 4:6)
#' print(a)
#' list_to_data_frame(a)
#' @export
list_to_data_frame <- function(x) {
  df <- tibble(key = unique(unlist(x)))
  for (name in names(x)) df[, name] <- df$key %in% x[[name]]
  return(df)
}
