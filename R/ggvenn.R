#' Venn Diagram by ggplot
#'
#' @name ggvenn
#' @aliases geom_venn, GeomVenn
#' @export
#' @param data     A data.frame or a list as input data.
#' @param columns  A character vector use as index to select columns/elements.
#' @return The ggplot object to print or save to file
#' @examples
#' library(ggvenn)
#'
#' # use list as input
#' a <- list(`Set 1` = c(1, 3, 5, 7, 9),
#'           `Set 2` = c(1, 5, 9, 13),
#'           `Set 3` = c(1, 2, 8, 9),
#'           `Set 4` = c(6, 7, 10, 12))
#' ggvenn(a, c("Set 1", "Set 2"))
#' ggvenn(a, c("Set 1", "Set 2", "Set 3"))
#' ggvenn(a)
#'
#' # use data.frame as input
#' d <- tibble(value   = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 12, 13),
#'             `Set 1` = c(T, F, T, T, F, T, F, T, F,  F,  F),
#'             `Set 2` = c(T, F, F, T, F, F, F, T, F,  F,  T),
#'             `Set 3` = c(T, T, F, F, F, F, T, T, F,  F,  F),
#'             `Set 4` = c(F, F, F, F, T, T, F, F, T,  T,  F))
#' ggvenn(d, c("Set 1", "Set 2"))
#' ggvenn(d, c("Set 1", "Set 2", "Set 3"))
#' ggvenn(d)
#'
#' # ggplot gramma
#' ggplot(d) + geom_venn(aes(A = `Set 1`, B = `Set 2`)) + theme_void()
#' ggplot(d) + geom_venn(aes(A = `Set 1`, B = `Set 2`, C = `Set 3`)) + theme_void()
#' ggplot(d) + geom_venn(aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`)) + theme_void()
ggvenn <- function(data, columns = NULL) {
  venn <- prepare_venn_data(data, columns)
  venn$shapes %>% mutate(group = LETTERS[group]) %>%
    ggplot() +
    geom_polygon(aes(x = x, y = y, group = group, fill = group), alpha = .5) +
    geom_polygon(aes(x = x, y = y, group = group, fill = NA), size = 1, color = "black") +
    scale_x_continuous(limits = c(-2, 2)) +
    scale_y_continuous(limits = c(-2, 2)) +
    geom_text(data = venn$texts, aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust)) +
    geom_text(data = venn$labels, aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust), size = 6) +
    scale_fill_manual(values = c("blue", "yellow", "green", "red")) +
    guides(fill = FALSE) +
    theme_void()
}

gen_circle <- function(group, x_offset = 0, y_offset = 0, radius = 1,
                       radius_b = radius, theta_offset = 0, length.out = 100) {
  tibble(group = group,
         theta = seq(0, 2 * pi, length.out = length.out)) %>%
  mutate(x_raw = radius * cos(theta),
         y_raw = radius_b * sin(theta),
         x = x_offset + x_raw * cos(theta_offset) - y_raw * sin(theta_offset),
         y = y_offset + x_raw * sin(theta_offset) + y_raw * cos(theta_offset))
}

gen_circle_2 <- function() {
  rbind(gen_circle(1L, -2/3, 0, 1),
        gen_circle(2L, 2/3, 0, 1))
}
gen_text_pos_2 <- function() {
  tribble(~name, ~x,   ~y, ~hjust, ~vjust, ~A,    ~B,
          "A",   -0.8, 0,  0.5,    0.5,    TRUE,  FALSE,
          "B",    0.8, 0,  0.5,    0.5,    FALSE, TRUE,
          "AB",   0,   0,  0.5,    0.5,    TRUE,  TRUE)
}
gen_label_pos_2 <- function() {
  tribble(~name, ~x,   ~y,  ~hjust, ~vjust,
          "A",   -0.8, 1.2, 0.5,    0,
          "B",    0.8, 1.2, 0.5,    0)
}

gen_circle_3 <- function() {
  rbind(gen_circle(1L, -2/3, (sqrt(3) + 2) / 6, 1),
        gen_circle(2L, 2/3,(sqrt(3) + 2) / 6, 1),
        gen_circle(3L, 0, -(sqrt(3) + 2) / 6, 1))
}
gen_text_pos_3 <- function() {
  tribble(~name, ~x,    ~y,   ~hjust, ~vjust, ~A,    ~B,    ~C,
          "A",   -0.8,  0.62, 0.5,    0.5,    TRUE,  FALSE, FALSE,
          "B",    0.8,  0.62, 0.5,    0.5,    FALSE, TRUE,  FALSE,
          "C",    0,   -0.62, 0.5,    0.5,    FALSE, FALSE, TRUE,
          "AB",   0,    0.8,  0.5,    0.5,    TRUE,  TRUE,  FALSE,
          "AC",  -0.5,  0,    0.5,    0.5,    TRUE,  FALSE, TRUE,
          "BC",   0.5,  0,    0.5,    0.5,    FALSE, TRUE,  TRUE,
          "ABC",  0,    0.2,  0.5,    0.5,    TRUE,  TRUE,  TRUE)
}
gen_label_pos_3 <- function() {
  tribble(~name, ~x,    ~y,  ~hjust, ~vjust,
          "A",   -0.8,  1.8, 0.5,    0,
          "B",    0.8,  1.8, 0.5,    0,
          "C",    0,   -1.8, 0.5,    1)
}

gen_circle_4 <- function() {
  rbind(gen_circle(1L, -.7, -1/2, .75, 1.5, pi/4),
        gen_circle(2L, -.72+2/3, -1/6, .75, 1.5, pi/4),
        gen_circle(3L, .72-2/3, -1/6, .75, 1.5, -pi/4),
        gen_circle(4L, .7, -1/2, .75, 1.5, -pi/4))
}
gen_text_pos_4 <- function() {
  tribble(~name, ~x,    ~y,  ~hjust, ~vjust, ~A,   ~B,    ~C,    ~D,
          "A",   -1.5,  0,   0.5,    0.5,    TRUE,  FALSE, FALSE, FALSE,
          "B",   -0.6,  0.7, 0.5,    0.5,    FALSE, TRUE,  FALSE, FALSE,
          "C",    0.6,  0.7, 0.5,    0.5,    FALSE, FALSE, TRUE,  FALSE,
          "D",    1.5,  0,   0.5,    0.5,    FALSE, FALSE, FALSE, TRUE,
          "AB",  -0.9,  0.3, 0.5,    0.5,    TRUE,  TRUE,  FALSE, FALSE,
          "BC",   0,    0.4, 0.5,    0.5,    FALSE, TRUE,  TRUE,  FALSE,
          "CD",   0.9,  0.3, 0.5,    0.5,    FALSE, FALSE, TRUE,  TRUE,
          "AC",  -0.8, -0.9, 0.5,    0.5,    TRUE,  FALSE, TRUE,  FALSE,
          "BD",   0.8, -0.9, 0.5,    0.5,    FALSE, TRUE,  FALSE, TRUE,
          "AD",   0,   -1.4, 0.5,    0.5,    TRUE,  FALSE, FALSE, TRUE,
          "ABC", -0.5, -0.2, 0.5,    0.5,    TRUE,  TRUE,  TRUE,  FALSE,
          "BCD",  0.5, -0.2, 0.5,    0.5,    FALSE, TRUE,  TRUE,  TRUE,
          "ACD", -0.3, -1.1, 0.5,    0.5,    TRUE,  FALSE, TRUE,  TRUE,
          "BCD",  0.3, -1.1, 0.5,    0.5,    FALSE, TRUE,  TRUE,  TRUE,
          "ABCD", 0,   -0.7, 0.5,    0.5,    TRUE,  TRUE,  TRUE,  TRUE)
}
gen_label_pos_4 <- function() {
  tribble(~name, ~x,   ~y,   ~hjust, ~vjust,
          "A",   -1.5, -1.3, 1,      1,
          "B",   -0.8,  1.2, 0.5,    0,
          "C",    0.8,  1.2, 0.5,    0,
          "D",    1.5, -1.3, 0,      1)
}

#' @rdname ggvenn
#' @inheritParams ggplot2::stat_identity
#' @export
geom_venn <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity", ...) {
  l <- layer(mapping = mapping, data = data,
        geom = GeomVenn, stat = stat, position = position,
        params = list(na.rm = TRUE, ...))
  old_compute_aesthetics <- l$compute_aesthetics
  l$compute_aesthetics <- function(self, data, plot) {
    self$geom$set_names <- character()
    for (name in names(plot$mapping)) {
      self$geom$set_names[name] <- as_label(plot$mapping[[name]])
    }
    for (name in names(self$mapping)) {
      self$geom$set_names[name] <- as_label(self$mapping[[name]])
    }
    old_compute_aesthetics(data, plot)
  }
  l
}

#' @rdname ggvenn
#' @export
GeomVenn <- ggproto("GeomVenn", Geom,
  required_aes = c("A", "B"),
  optional_aes = c("C", "D"),
  default_aes = aes(color = "black", fill = NA, alpha = .8, size = 1, linetype = "solid"),
  extra_params = c("na.rm"),
  setup_data = function(self, data, params) {
    data %>% mutate(xmin = -2, xmax = 2, ymin = -2, ymax = 2)
  },
  draw_panel = function(self, data, panel_params, coord, ...) {
    sets <- c("A", "B", "C", "D")
    sets <- sets[sets %in% names(data)]
    venn <- prepare_venn_data(data, sets)
    colors <- c("blue", "yellow", "green", "red")
    d0 <- coord_munch(coord, venn$shapes, panel_params)
    d <- d0 %>%
      filter(!duplicated(group)) %>%
      mutate(color = "black", fill = colors[group], alpha = .5,
             size = 1, linetype = "solid")
    d1 <- coord_munch(coord, venn$texts, panel_params)
    d2 <- coord_munch(coord, venn$labels, panel_params)
    ggplot2:::ggname("geom_venn",
      grobTree(
        polygonGrob(
          d0$x, d0$y, default.units = "native", id = d0$group,
          gp = gpar(col = NA, fill = alpha(d$fill, d$alpha))),
        polygonGrob(
          d0$x, d0$y, default.units = "native", id = d0$group,
          gp = gpar(col = d$color, fill = NA, lwd = d$size * .pt, lty = d$linetype)),
        textGrob(
          d1$text, d1$x, d1$y, default.units = "native",
          hjust = d1$hjust, vjust = d1$vjust,
          gp = gpar(col = "black")),
        textGrob(
          self$set_names, d2$x, d2$y, default.units = "native",
          hjust = d1$hjust, vjust = d2$vjust,
          gp = gpar(col = "black", fontsize = 6 * .pt))
      )
    )
  }
)

prepare_venn_data <- function(data, columns = NULL) {
  if (is.data.frame(data)) {
    if (is.null(columns)) {
      columns = data %>% select_if(is.logical) %>% names
    }
    if (length(columns) == 2) {
      stopifnot(is.logical(data[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(data[,columns[[2]], drop = TRUE]))
      d <- gen_circle_2()
      d1 <- gen_text_pos_2() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], data[,columns[[1]]])) &
                         (!xor(d1$B[[i]], data[,columns[[2]]])))
      }
      d2 <- gen_label_pos_2()
    } else if (length(columns) == 3) {
      stopifnot(is.logical(data[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(data[,columns[[2]], drop = TRUE]))
      stopifnot(is.logical(data[,columns[[3]], drop = TRUE]))
      d <- gen_circle_3()
      d1 <- gen_text_pos_3() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], data[,columns[[1]]])) &
                         (!xor(d1$B[[i]], data[,columns[[2]]])) &
                         (!xor(d1$C[[i]], data[,columns[[3]]])))
      }
      d2 <- gen_label_pos_3()
    } else if (length(columns) == 4) {
      stopifnot(is.logical(data[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(data[,columns[[2]], drop = TRUE]))
      stopifnot(is.logical(data[,columns[[3]], drop = TRUE]))
      stopifnot(is.logical(data[,columns[[4]], drop = TRUE]))
      d <- gen_circle_4()
      d1 <- gen_text_pos_4() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], data[,columns[[1]]])) &
                         (!xor(d1$B[[i]], data[,columns[[2]]])) &
                         (!xor(d1$C[[i]], data[,columns[[3]]])) &
                         (!xor(d1$D[[i]], data[,columns[[4]]])))
      }
      d2 <- gen_label_pos_4()
    } else {
      stop("logical columns in data.frame `data` or vector `columns` should be length between 2 and 4")
    }
    d2 <- d2 %>% mutate(text = columns)
  } else if (is.list(data)) {
    if (is.null(columns)) {
      columns <- names(data) %>% head(4)
    }
    a2 <- unique(unlist(data[columns]))
    if (length(columns) == 2) {
      d <- gen_circle_2()
      d1 <- gen_text_pos_2() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], a2 %in% data[[columns[[1]]]])) &
                         (!xor(d1$B[[i]], a2 %in% data[[columns[[2]]]])))
      }
      d2 <- gen_label_pos_2()
    } else if (length(columns) == 3) {
      d <- gen_circle_3()
      d1 <- gen_text_pos_3() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], a2 %in% data[[columns[[1]]]])) &
                         (!xor(d1$B[[i]], a2 %in% data[[columns[[2]]]])) &
                         (!xor(d1$C[[i]], a2 %in% data[[columns[[3]]]])))
      }
      d2 <- gen_label_pos_3()
    } else if (length(columns) == 4) {
      d <- gen_circle_4()
      d1 <- gen_text_pos_4() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], a2 %in% data[[columns[[1]]]])) &
                         (!xor(d1$B[[i]], a2 %in% data[[columns[[2]]]])) &
                         (!xor(d1$C[[i]], a2 %in% data[[columns[[3]]]])) &
                         (!xor(d1$D[[i]], a2 %in% data[[columns[[4]]]])))
      }
      d2 <- gen_label_pos_4()
    } else {
      stop("list `data` or vector `column` should be length between 2 and 4")
    }
    d2 <- d2 %>% mutate(text = columns)
  } else {
    stop("`data` should be a list")
  }
  d1 <- d1 %>% mutate(text = sprintf("%d\n(%.1f%%)", n, 100 * n / sum(n)))
  list(shapes = d, texts = d1, labels = d2)
}
