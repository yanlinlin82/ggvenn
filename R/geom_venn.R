#' Plot venn diagram as a ggplot layer object. It supports only data frame as input.
#'
#' @name geom_venn
#' @inheritParams ggplot2::stat_identity
#' @param stat The statistical transformation to use on the data for this layer, as a string.

#' @param data A data.frame or a list as input data.
#' @param set_names Set names, use column names if omitted.
#' @param show_set_totals Show total count (c) and/or percentage (p) for each set.
#' Pass a string like "cp" to show both. Any other string like "none" to hide both.
#' @param show_stats Show count (c) and/or percentage (p) for each set.
#' Pass a string like "cp" to show both.
#' @param show_percentage Show percentage for each set. Deprecated, use show_stats instead.
#' @param digits The desired number of digits after the decimal point
#' @param label_sep separator character for displaying elements.
#' @param count_column Specify column for element repeat count.
#' @param show_outside Show outside elements (not belongs to any set).
#' @param auto_scale Allow automatically resizing circles according to element counts.
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
#' @return The ggplot object to print or save to file.
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
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`), show_stats = 'c') +
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
  show_set_totals = "none",
  show_stats = "cp",
  show_percentage = deprecated(),
  digits = 1,
  label_sep = ",",
  count_column = NULL,
  show_outside = c("auto", "none", "always"),
  auto_scale = FALSE,
  fill_color = c("blue", "yellow", "green", "red"),
  fill_alpha = .5,
  stroke_color = "black",
  stroke_alpha = 1,
  stroke_size = 1,
  stroke_linetype = "solid",
  set_name_color = "black",
  set_name_size = 6,
  text_color = "black",
  text_size = 4
) {
  show_outside <- match.arg(show_outside)

  geom_venn_obj <- ggplot2::ggproto(
    "GeomVenn",
    ggplot2::Geom,
    required_aes = c("A", "B"),
    optional_aes = c("C", "D", "label"),
    extra_params = c("na.rm"),
    setup_data = function(self, data, params) {
      data %>% dplyr::mutate(xmin = -2, xmax = 2, ymin = -2, ymax = 2)
    },
    draw_panel = function(self, data, panel_params, coord, ...) {
      attr <- self$customize_attributes
      sets <- c("A", "B", "C", "D")
      sets <- sets[sets %in% names(data)]
      show_elements <- FALSE
      if ("label" %in% names(data)) {
        show_elements <- "label"
      }
      show_set_totals <- attr$show_set_totals
      show_stats <- attr$show_stats
      digits <- attr$digits
      label_sep <- attr$label_sep
      count_column <- attr$count_column
      show_outside <- attr$show_outside
      auto_scale <- attr$auto_scale
      venn <- prepare_venn_data(
        data, sets,
        show_elements, show_set_totals, show_stats,
        digits, label_sep, count_column,
        show_outside, auto_scale
      )

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

      if (nrow(venn$texts) > 0) {
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
      if (nrow(venn$segs) > 0) {
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
      ggplot2:::ggname("geom_venn", grid::grobTree(gl))
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
      show_stats = show_stats,
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
