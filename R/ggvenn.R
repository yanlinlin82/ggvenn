library(dplyr)
library(ggplot2)

#' Plot venn diagram as an independent function. It supports both data frame and list as input.
#'
#' @name ggvenn
#' @param data A data.frame or a list as input data.
#' @param columns A character vector use as index to select columns/elements.
#' @param show_elements Show set elements instead of count/percentage.
#' @param show_set_totals Show total count (c) and/or percentage (p) for each set.
#' Pass a string like "cp" to show both. Any other string like "none" to hide both.
#' @param show_stats Show count (c) and/or percentage (p) for each set.
#' @param show_counts Show count for each set.
#' @param show_percentage Show percentage for each set.
#' Pass a string like "cp" to show both. Any other string like "none" to hide both.
#' @param digits The desired number of digits after the decimal point
#' @param fill_color Filling colors in circles.
#' @param fill_alpha Transparency for filling circles.
#' @param stroke_color Stroke color for drawing circles.
#' @param stroke_alpha Transparency for drawing circles.
#' @param stroke_size Stroke size for drawing circles.
#' @param stroke_linetype Line type for drawing circles.
#' @param set_name_color Text color for set names.
#' @param set_name_size Text size for set names.
#' @param text_color Text color for intersect contents.
#' @param text_size Text size for intersect contents.
#' @param label_sep Separator character for displaying elements.
#' @param count_column Specify column for element repeat count.
#' @param show_outside Show outside elements (not belongs to any set).
#' @param auto_scale Allow automatically resizing circles according to element counts.
#' @param comma_sep Whether to use comma as separator for displaying numbers.
#' @param padding Padding for the plot. Change this to allow longer labels to be displayed.
#' @return The ggplot object to print or save to file.
#' @examples
#' library(ggvenn)
#'
#' # use list as input
#' a <- list(A = 1:5, B = 4:9, C = c(2:3, 8:12), D = c(1, 5, 9))
#' ggvenn(a, c("A", "B"))
#' ggvenn(a, c("A", "B", "C"))
#' ggvenn(a)
#'
#' # use data.frame as input
#' d <- dplyr::tibble(value   = c(1,     2,     3,     5,     6,     7,     8,     9),
#'             `Set 1` = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE),
#'             `Set 2` = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE),
#'             `Set 3` = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE),
#'             `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE))
#' ggvenn(d, c("Set 1", "Set 2"))
#' ggvenn(d, c("Set 1", "Set 2", "Set 3"))
#' ggvenn(d)
#'
#' # set fill color
#' ggvenn(d, c("Set 1", "Set 2"), fill_color = c("red", "blue"))
#'
#' # hide percentage
#' ggvenn(d, c("Set 1", "Set 2"), show_stats = "c")
#'
#' # change precision of percentages
#' ggvenn(d, c("Set 1", "Set 2"), digits = 2)
#'
#' # show elements instead of count/percentage
#' ggvenn(a, show_elements = TRUE)
#' ggvenn(d, show_elements = "value")
#' @seealso geom_venn
#' @importFrom dplyr tibble tribble as_tibble %>% select_if mutate count filter inner_join
#' @importFrom ggplot2 ggplot aes geom_polygon geom_segment geom_text scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_fill_manual guides coord_fixed theme_void
#' @importFrom ggplot2 layer scale_x_discrete scale_y_discrete expansion
#' @importFrom stats na.omit
#' @export
ggvenn <- function(
  data,
  columns = NULL,
  show_elements = FALSE,
  show_set_totals = "none",
  show_stats = c("cp", "c", "p"),
  show_counts = TRUE,
  show_percentage = TRUE,
  digits = 1,
  label_sep = ",",
  count_column = NULL,
  show_outside = c("auto", "none", "always"),
  auto_scale = FALSE,
  fill_color = default_color_list,
  fill_alpha = .5,
  stroke_color = "black",
  stroke_alpha = 1,
  stroke_size = 1,
  stroke_linetype = "solid",
  set_name_color = "black",
  set_name_size = 6,
  text_color = "black",
  text_size = 4,
  comma_sep = FALSE,
  padding = 0.2
) {
  show_outside <- match.arg(show_outside)

  if (!is.data.frame(data)) {
    if (is.list(data)) {
      data <- list_to_data_frame(data)
    } else {
      stop("data must be a list or a data.frame")
    }
  }

  set_names <- character(0)
  if (missing(columns)) {
    for (name in names(data)) {
      if (is.logical(data[[name]])) {
        set_names <- c(set_names, name)
      }
    }
  } else {
    for (name in columns) {
      stopifnot(name %in% names(data))
      stopifnot(is.logical(data[[name]]))
    }
    set_names <- columns
  }
  n_sets <- length(set_names)
  stopifnot(n_sets >= min_set_num && n_sets <= max_set_num)
  set_names <- as.list(set_names)
  names(set_names) <- LETTERS[seq_len(n_sets)]

  if (!missing(show_stats)) {
    show_stats <- match.arg(show_stats)
    if (show_stats == "cp") {
      show_counts <- TRUE
      show_percentage <- TRUE
    } else if (show_stats == "c") {
      show_counts <- TRUE
      show_percentage <- FALSE
    } else if (show_stats == "p") {
      show_counts <- FALSE
      show_percentage <- TRUE
    }
  } else {
    show_counts <- ifelse(missing(show_counts), TRUE, show_counts)
    show_percentage <- ifelse(missing(show_percentage), ifelse(n_sets >= 5, FALSE, TRUE), show_percentage)
    if (show_counts && show_percentage) {
      show_stats <- "cp"
    } else if (show_counts) {
      show_stats <- "c"
    } else if (show_percentage) {
      show_stats <- "p"
    }
  }
  stopifnot(show_counts || show_percentage)

  # Create aes mapping using modern ggplot2 syntax
  aes_list <- list()
  for (i in seq_along(set_names)) {
    aes_list[[LETTERS[i]]] <- sym(set_names[[i]])
  }

  if (is.logical(show_elements)) {
    if (show_elements) {
      show_elements <- "key"
      aes_list[["label"]] <- sym("key")
    }
  } else if (!is.character(show_elements)) {
    stop("show_elements must be a logical or a character vector")
  } else {
    stopifnot(show_elements %in% names(data))
    aes_list[["label"]] <- sym(show_elements)
  }

  the_aes <- do.call(aes, aes_list)

  g <- ggplot(data) +
    geom_venn(
      the_aes,
      show_stats = show_stats,
      show_counts = show_counts,
      show_percentage = show_percentage,
      digits = digits,
      label_sep = label_sep,
      count_column = count_column,
      show_outside = show_outside,
      auto_scale = auto_scale,
      fill_color = fill_color,
      fill_alpha = fill_alpha,
      stroke_color = stroke_color,
      stroke_alpha = stroke_alpha,
      stroke_size = stroke_size,
      stroke_linetype = stroke_linetype,
      set_name_color = set_name_color,
      set_name_size = set_name_size,
      text_color = text_color,
      text_size = text_size,
      comma_sep = comma_sep,
      padding = padding
    ) +
    coord_fixed() +
    theme_void()
  g
}
