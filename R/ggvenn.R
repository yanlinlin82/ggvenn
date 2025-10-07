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
  label_sep = ",",
  count_column = NULL,
  show_outside = c("auto", "none", "always"),
  auto_scale = FALSE,
  comma_sep = FALSE,
  padding = 0.2
) {
  show_outside <- match.arg(show_outside)

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
    show_percentage <- ifelse(missing(show_percentage), TRUE, show_percentage)
  }
  stopifnot(show_counts || show_percentage)

  venn_data <- prepare_venn_data(
    data, columns, show_elements, show_set_totals,
    show_counts, show_percentage, digits, label_sep, count_column,
    show_outside, auto_scale, comma_sep = comma_sep
  )

  if (length(stroke_color) > 1) {
    stroke_color <- stroke_color[venn_data$shapes$group]
  }
  if (length(stroke_size) > 1) {
    stroke_size <- stroke_size[venn_data$shapes$group]
  }
  if (length(stroke_alpha) > 1) {
    stroke_alpha <- stroke_alpha[venn_data$shapes$group]
  }
  if (length(stroke_linetype) > 1) {
    stroke_linetype <- stroke_linetype[venn_data$shapes$group]
  }

  g <- venn_data$shapes %>%
    dplyr::mutate(group = LETTERS[group]) %>%
    ggplot() +
    geom_polygon(
      aes(x = x, y = y, group = group, fill = group),
      alpha = fill_alpha
    ) +
    geom_polygon(
      aes(x = x, y = y, group = group),
      fill = NA,
      color = stroke_color,
      linewidth = stroke_size,
      alpha = stroke_alpha,
      linetype = stroke_linetype
    )

  if (nrow(venn_data$labels) > 0) {
    g <- g +
      geom_text(
        data = venn_data$labels,
        aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
        color = set_name_color,
        size = set_name_size
      )
  }

  if (nrow(venn_data$texts) > 0) {
    g <- g +
      geom_text(
        data = venn_data$texts,
        aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
        color = text_color,
        size = text_size
      )
  }

  if (nrow(venn_data$segs) > 0) {
    g <- g +
      geom_segment(
        data = venn_data$segs,
        aes(x = x, y = y, xend = xend, yend = yend),
        color = text_color,
        linewidth = 0.5
      )
  }
  set_names <- get_set_names(columns, data)
  fill_color <- fix_fill_color(fill_color, set_names)
  g <- g +
    scale_fill_manual(values = fill_color) +
    scale_x_discrete(expand = expansion(mult = c(padding, padding))) +
    scale_y_discrete(expand = expansion(mult = c(padding, padding))) +
    guides(fill = "none") +
    coord_fixed() +
    theme_void()
  g
}

get_set_names <- function(columns, data) {
  set_names <- columns
  if (is.null(set_names)) {
    set_names <- names(data)
  }
  set_names
}

fix_fill_color <- function(fill_color, set_names) {
  if (!is.null(names(fill_color))) {
    for (i in seq_along(fill_color)) {
      if (names(fill_color)[i] %in% set_names) {
        names(fill_color)[i] <- LETTERS[which(set_names == names(fill_color)[i])]
      }
    }
  }
  fill_color
}
