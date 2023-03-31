#' Plot venn diagram as a ggplot layer object. It supports only data frame as input.
#'
#' @name geom_venn
#' @inheritParams ggplot2::stat_identity
#' @param stat The statistical transformation to use on the data for this layer, as a string.

#' @param data A data.frame or a list as input data.
#' @param set_names Set names, use column names if omitted.
#' @param show_percentage Show percentage for each set.
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
#' d <- tibble(value   = c(1,     2,     3,     5,     6,     7,     8,     9),
#'             `Set 1` = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE),
#'             `Set 2` = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE),
#'             `Set 3` = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE),
#'             `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE))
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
#'   geom_venn(aes(A = `Set 1`, B = `Set 2`), show_percentage = FALSE) +
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
geom_venn <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      set_names = NULL,
                      show_percentage = TRUE,
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
                      text_size = 4) {
  show_outside <- match.arg(show_outside)
  l <- layer(mapping = mapping, data = data,
             geom = GeomVenn, stat = stat, position = position,
             params = list(na.rm = TRUE, ...))
  old_compute_aesthetics <- l$compute_aesthetics
  l$compute_aesthetics <- function(self, data, plot) {
    if (is.null(set_names)) {
      self$geom$set_names <- character()
      for (name in names(plot$mapping)) {
        self$geom$set_names[name] <- as_label(plot$mapping[[name]])
      }
      for (name in names(self$mapping)) {
        self$geom$set_names[name] <- as_label(self$mapping[[name]])
      }
    } else {
      self$geom$set_names <- set_names
    }
    self$geom$customize_attributes <- list(show_percentage = show_percentage,
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
                                           text_size = text_size)
    old_compute_aesthetics(data, plot)
  }
  l
}

#' @importFrom grid grobTree polygonGrob textGrob gpar
GeomVenn <- ggproto("GeomVenn", Geom,
                    required_aes = c("A", "B"),
                    optional_aes = c("C", "D", "label"),
                    extra_params = c("na.rm"),
                    setup_data = function(self, data, params) {
                      data %>% mutate(xmin = -2, xmax = 2, ymin = -2, ymax = 2)
                    },
                    draw_panel = function(self, data, panel_params, coord, ...) {
                      attr <- self$customize_attributes
                      sets <- c("A", "B", "C", "D")
                      sets <- sets[sets %in% names(data)]
                      show_elements <- FALSE
                      if ("label" %in% names(data)) {
                        show_elements <- "label"
                      }
                      show_percentage <- attr$show_percentage
                      digits <- attr$digits
                      label_sep <- attr$label_sep
                      count_column <- attr$count_column
                      show_outside <- attr$show_outside
                      auto_scale <- attr$auto_scale
                      venn <- prepare_venn_data(data, sets, show_elements, show_percentage, digits,
                                                label_sep, count_column, show_outside, auto_scale)
                      d0 <- coord_munch(coord, venn$shapes, panel_params)
                      d <- d0 %>%
                        filter(!duplicated(group)) %>%
                        mutate(fill_color = attr$fill_color[group],
                               fill_alpha = attr$fill_alpha,
                               stroke_color = attr$stroke_color,
                               stroke_alpha = attr$stroke_alpha,
                               stroke_size = attr$stroke_size,
                               stroke_linetype = attr$stroke_linetype)

                      gl <- gList(polygonGrob(id = d0$group,
                                              d0$x, d0$y, default.units = "native",
                                              gp = gpar(col = NA,
                                                        fill = alpha(d$fill_color, d$fill_alpha))),
                                  polygonGrob(id = d0$group,
                                              d0$x, d0$y, default.units = "native",
                                              gp = gpar(col = alpha(d$stroke_color, d$stroke_alpha),
                                                        fill = NA,
                                                        lwd = d$stroke_size * .pt,
                                                        lty = d$stroke_linetype)))
                      if (nrow(venn$labels) > 0) {
                        d1 <- coord_munch(coord, venn$labels, panel_params)
                        gl <- gList(gl,
                                    textGrob(self$set_names,
                                             d1$x, d1$y, default.units = "native",
                                             hjust = d1$hjust, vjust = d1$vjust,
                                             gp = gpar(col = attr$set_name_color,
                                                       fontsize = attr$set_name_size * .pt)))
                      }
                      if (nrow(venn$texts) > 0) {
                        d2 <- coord_munch(coord, venn$texts, panel_params)
                        gl <- gList(gl,
                                    textGrob(d2$text,
                                             d2$x, d2$y, default.units = "native",
                                             hjust = d2$hjust, vjust = d2$vjust,
                                             gp = gpar(col = attr$text_color,
                                                       fontsize = attr$text_size * .pt)))
                      }
                      if (nrow(venn$segs) > 0) {
                        d3 <- coord_munch(coord, venn$segs, panel_params)
                        gl <- gList(gl,
                                    segmentsGrob(d3$x, d3$y, d3$xend, d3$yend,
                                                 default.units = "native",
                                                 gp = gpar(col = attr$text_color,
                                                           size = attr$text_size * .pt)))
                      }
                      ggplot2:::ggname("geom_venn", grobTree(gl))
                    }
)
