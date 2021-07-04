#' Utility function for data type conversion.
#'
#' @name data_frame_to_list
#' @param x A data.frame with logical columns representing sets.
#' @return A list of sets.
#' @examples
#' d <- tibble(name = 1:6,
#'             A = c(rep(TRUE, 5), FALSE),
#'             B = rep(c(FALSE, TRUE), each = 3))
#' print(d)
#' data_frame_to_list(d)
#' @export
data_frame_to_list <- function(x) {
  col_names <- colnames(x)
  logical_names <- colnames(x %>% select_if(is.logical))
  rest_names <- col_names[!col_names %in% logical_names]
  keys <- 1:nrow(x)
  if (length(rest_names) >= 1) {
    keys <- x[[rest_names[1]]]
  }
  lst <- lapply(logical_names, function(i) keys[x[[i]]])
  names(lst) <- logical_names
  return(lst)
}
