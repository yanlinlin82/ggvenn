#' Utility functions for data type conversion between data.frame and list.
#'
#' @name data_conversion
#' @param x A data.frame with logical columns representing sets, or a list of sets.
#' @return A list of sets or a data.frame with logical columns representing sets.
#' @examples
#' # Convert data.frame to list
#' d <- dplyr::tibble(name = 1:6,
#'             A = c(rep(TRUE, 5), FALSE),
#'             B = rep(c(FALSE, TRUE), each = 3))
#' print(d)
#' data_frame_to_list(d)
#'
#' # Convert list to data.frame
#' a <- list(A = 1:5, B = 4:6)
#' print(a)
#' list_to_data_frame(a)
#'
#' # Round-trip conversion
#' identical(a, data_frame_to_list(list_to_data_frame(a)))  # TRUE
#' identical(d, list_to_data_frame(data_frame_to_list(d)))  # TRUE
#' @export
data_frame_to_list <- function(x) {
  col_names <- colnames(x)
  logical_names <- colnames(x %>% select_if(is.logical))
  rest_names <- col_names[!col_names %in% logical_names]
  keys <- seq_len(nrow(x))
  if (length(rest_names) >= 1) {
    keys <- x[[rest_names[1]]]
  }
  lst <- lapply(logical_names, function(i) keys[x[[i]]])
  names(lst) <- logical_names
  lst
}

#' @rdname data_conversion
#' @export
list_to_data_frame <- function(x) {
  df <- tibble(key = unique(unlist(x)))
  for (name in names(x)) {
    df[, name] <- df$key %in% x[[name]]
  }
  df
}
