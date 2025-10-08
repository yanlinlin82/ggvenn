# Conversion factor for points to other units (same as ggplot2:::.pt)
.pt <- 72.27 / 25.4

#' Plot venn diagram as a ggplot layer object. It supports only data frame as input.
#'
#' @name geom_venn
#' @inheritParams ggplot2::stat_identity
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param data A data.frame or a list as input data.
#' @param set_names Set names, use column names if omitted.
#' @param element_column A single character value use as column name to select elements.
#' @param show_elements Show set elements instead of count/percentage.
#' @param show_set_totals Show total count (c) and/or percentage (p) for each set.
#' Pass a string like "cp" to show both. Any other string like "none" to hide both.
#' @param show_stats Show count (c) and/or percentage (p) for each set.
#' Pass a string like "cp" to show both.
#' @param show_counts Show count for each set.
#' @param show_percentage Show percentage for each set.
#' @param digits The desired number of digits after the decimal point.
#' @param label_sep Separator character for displaying elements.
#' @param count_column Specify column for element repeat count.
#' @param show_outside Show outside elements (not belongs to any set). Options: "auto", "none", "always".
#' @param auto_scale Allow automatically resizing circles according to element counts (only for 2-set diagrams).
#' @param fill_color Filling colors in circles. Can be a single color or a vector of colors for each set.
#' @param fill_alpha Transparency for filling circles. Can be a single value or a vector for each set.
#' @param stroke_color Stroke color for drawing circles. Can be a single color or a vector of colors for each set.
#' @param stroke_alpha Transparency for drawing circles. Can be a single value or a vector for each set.
#' @param stroke_size Stroke size for drawing circles. Can be a single value or a vector for each set.
#' @param stroke_linetype Line type for drawing circles. Can be a single value or a vector for each set.
#' @param set_name_color Text color for set names.
#' @param set_name_size Text size for set names.
#' @param text_color Text color for intersect contents.
#' @param text_size Text size for intersect contents.
#' @param comma_sep Whether to use comma as separator for displaying numbers.
#' @param padding Padding for the plot. Change this to allow longer labels to be displayed.
#' @param max_elements Maximum number of elements to display when show_elements=TRUE.
#' @param text_truncate Whether to truncate text when elements exceed max_elements.
#' @return A ggplot layer object to add to a ggplot.
#' @examples
#' library(ggvenn)
#'
#' # use data.frame as input
#' d <- dplyr::tibble(
#'   value   = c(1,     2,     3,     5,     6,     7,     8,     9),
#'   `Set 1` = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE),
#'   `Set 2` = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE),
#'   `Set 3` = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE),
#'   `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE)
#' )
#'
#' # ggplot gramma
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`)) +
#'   coord_fixed() +
#'   theme_void()
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`, C = `Set 3`)) +
#'   coord_fixed() +
#' theme_void()
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`)) +
#'   coord_fixed() +
#'   theme_void()
#'
#' # set fill color
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`), fill_color = c("red", "blue")) +
#'   coord_fixed() +
#'   theme_void()
#'
#' # hide percentage
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`), show_stats = "c") +
#'   coord_fixed() +
#'   theme_void()
#'
#' # change precision of percentages
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`), digits = 2) +
#'   coord_fixed() +
#'   theme_void()
#'
#' # show elements instead of count/percentage
#' ggplot(d) +
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`, label = value)) +
#'   coord_fixed() +
#'   theme_void()
#' @seealso ggvenn
#' @importFrom dplyr as_label
#' @export
geom_venn <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  set_names = NULL,
  element_column = NULL,
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
  padding = 1,
  max_elements = 6,
  text_truncate = TRUE
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

  geom_venn_obj <- ggplot2::ggproto(
    "GeomVenn",
    ggplot2::Geom,
    required_aes = c("A", "B"),
    optional_aes = c("C", "D", "E", "F", "G", "H", "label"),
    extra_params = c("na.rm"),
    setup_data = function(self, data, params) {
      sets <- c("A", "B", "C", "D", "E", "F", "G", "H")
      sets <- sets[sets %in% names(data)]

      # Determine element_column from mapping if not provided
      if (is.null(element_column) && "label" %in% names(data)) {
        element_column <- "label"
      }

      self$venn_data <- prepare_venn_data(
        data, sets, element_column,
        show_elements, show_set_totals, show_counts, show_percentage,
        digits, label_sep, count_column,
        show_outside, auto_scale, comma_sep,
        max_elements, text_truncate
      )

      all_x <- c(
        self$venn_data$shapes$x,
        self$venn_data$texts$x,
        self$venn_data$labels$x,
        self$venn_data$segs$x
      )
      all_y <- c(
        self$venn_data$shapes$y,
        self$venn_data$texts$y,
        self$venn_data$labels$y,
        self$venn_data$segs$y
      )
      data %>%
        dplyr::mutate(
          xmin = min(all_x) - padding,
          xmax = max(all_x) + padding,
          ymin = min(all_y) - padding,
          ymax = max(all_y) + padding
        )
    },
    draw_panel = function(self, data, panel_params, coord, ...) {
      attr <- self$customize_attributes
      venn <- self$venn_data

      d0 <- ggplot2::coord_munch(coord, venn$shapes, panel_params)
      d <- d0 %>%
        dplyr::filter(!duplicated(group))

      update_column <- function(d, column, value) {
        if (length(value) == 1) {
          d[[column]] <- value
        } else {
          # For multiple values, assign based on group index
          d[[column]] <- value[d$group]
        }
        d
      }
      d <- update_column(d, "fill_color", attr$fill_color)
      d <- update_column(d, "fill_alpha", attr$fill_alpha)
      d <- update_column(d, "stroke_color", attr$stroke_color)
      d <- update_column(d, "stroke_alpha", attr$stroke_alpha)
      d <- update_column(d, "stroke_size", attr$stroke_size)
      d <- update_column(d, "stroke_linetype", attr$stroke_linetype)

      gl <- grid::gList(
        grid::polygonGrob(
          id = d0$group,
          d0$x, d0$y, default.units = "native",
          gp = grid::gpar(
            col = NA,
            fill = scales::alpha(d$fill_color, d$fill_alpha)
          )
        ),
        grid::polygonGrob(
          id = d0$group,
          d0$x, d0$y, default.units = "native",
          gp = grid::gpar(
            col = scales::alpha(d$stroke_color, d$stroke_alpha),
            fill = NA,
            lwd = d$stroke_size * .pt,
            lty = d$stroke_linetype
          )
        )
      )

      if (nrow(venn$labels) > 0) {
        # Replace set names in labels
        set_names <- self$set_names
        label_texts <- venn$labels$text
        updated_labels <- character(length = length(label_texts))
        for (i in seq_along(label_texts)) {
          updated_labels[[i]] <- gsub(paste0('^', names(set_names)[[i]]), set_names[[i]], label_texts[[i]])
        }
        d1 <- ggplot2::coord_munch(coord, venn$labels, panel_params)
        gl <- grid::gList(
          gl,
          grid::textGrob(
            updated_labels,
            d1$x, d1$y, default.units = "native",
            hjust = d1$hjust, vjust = d1$vjust,
            gp = grid::gpar(
              col = attr$set_name_color,
              fontsize = attr$set_name_size * .pt
            )
          )
        )
      }

      if (show_elements && nrow(venn$texts) > 0) {
        d2 <- ggplot2::coord_munch(coord, venn$texts, panel_params)
        gl <- grid::gList(
          gl,
          grid::textGrob(
            d2$text,
            d2$x, d2$y, default.units = "native",
            hjust = d2$hjust, vjust = d2$vjust,
            gp = grid::gpar(col = attr$text_color, fontsize = attr$text_size * .pt)
          )
        )
      }

      if (!is.null(venn$segs) && nrow(venn$segs) > 0) {
        d3 <- ggplot2::coord_munch(coord, venn$segs, panel_params)
        gl <- grid::gList(
          gl,
          grid::segmentsGrob(
            d3$x, d3$y, d3$xend, d3$yend,
            default.units = "native",
            gp = grid::gpar(col = attr$text_color, lwd = attr$text_size * .pt)
          )
        )
      }

      grid::grobTree(gl, name = "geom_venn")
    }
  )

  the_layer <- ggplot2::layer(
    mapping = mapping,
    data = data,
    geom = geom_venn_obj,
    stat = stat,
    position = position,
    params = list(na.rm = TRUE, ...)
  )

  old_compute_aesthetics <- the_layer$compute_aesthetics

  the_layer$compute_aesthetics <- function(self, data, plot) {
    if (is.null(set_names)) {
      self$geom$set_names <- character()
      for (name in names(plot$mapping)) {
        self$geom$set_names[name] <- dplyr::as_label(plot$mapping[[name]])
      }
      for (name in names(self$mapping)) {
        self$geom$set_names[name] <- dplyr::as_label(self$mapping[[name]])
      }
    } else {
      self$geom$set_names <- set_names
    }
    self$geom$customize_attributes <- list(
      show_counts = show_counts,
      show_percentage = show_percentage,
      show_set_totals = show_set_totals,
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
      text_size = text_size
    )
    old_compute_aesthetics(data, plot)
  }
  the_layer
}
