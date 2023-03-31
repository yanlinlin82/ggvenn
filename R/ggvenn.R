#' Plot venn diagram as an independent function. It supports both data frame and list as input.
#'
#' @name ggvenn
#' @param data A data.frame or a list as input data.
#' @param columns A character vector use as index to select columns/elements.
#' @param show_elements Show set elements instead of count/percentage.
#' @param show_percentage Show percentage for each set.
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
#' @return The ggplot object to print or save to file.
#' @examples
#' library(ggvenn)
#'
#' # use list as input
#' a <- list(`Set 1` = c(1, 3, 5, 7),
#'           `Set 2` = c(1, 5, 9),
#'           `Set 3` = c(1, 2, 8),
#'           `Set 4` = c(6, 7))
#' ggvenn(a, c("Set 1", "Set 2"))
#' ggvenn(a, c("Set 1", "Set 2", "Set 3"))
#' ggvenn(a)
#'
#' # use data.frame as input
#' d <- tibble(value   = c(1,     2,     3,     5,     6,     7,     8,     9),
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
#' ggvenn(d, c("Set 1", "Set 2"), show_percentage = FALSE)
#'
#' # change precision of percentages
#' ggvenn(d, c("Set 1", "Set 2"), digits = 2)
#'
#' # show elements instead of count/percentage
#' ggvenn(a, show_elements = TRUE)
#' ggvenn(d, show_elements = "value")
#' @seealso geom_venn
#' @importFrom dplyr tibble tribble as_tibble %>% select_if mutate count filter inner_join
#' @importFrom ggplot2 ggplot aes geom_polygon geom_segment geom_text scale_x_continuous scale_y_continuous scale_fill_manual guides coord_fixed theme_void layer
#' @importFrom stats na.omit
#' @export
ggvenn <- function(data, columns = NULL,
                   show_elements = FALSE,
                   show_percentage = TRUE,
                   digits = 1,
                   fill_color = c("blue", "yellow", "green", "red"),
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
                   auto_scale = FALSE) {
  show_outside <- match.arg(show_outside)
  venn <- prepare_venn_data(data, columns, show_elements, show_percentage, digits,
                            label_sep, count_column, show_outside, auto_scale)
  g <- venn$shapes %>%
    mutate(group = LETTERS[group]) %>%
    ggplot() +
    geom_polygon(aes(x = x, y = y, group = group, fill = group),
                 alpha = fill_alpha) +
    geom_polygon(aes(x = x, y = y, group = group),
                 fill = NA,
                 color = stroke_color,
                 size = stroke_size,
                 alpha = stroke_alpha,
                 linetype = stroke_linetype)
  if (nrow(venn$labels) > 0) {
    g <- g +
      geom_text(data = venn$labels,
                aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
                color = set_name_color,
                size = set_name_size)
  }
  if (nrow(venn$texts) > 0) {
    g <- g +
      geom_text(data = venn$texts,
                aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
                color = text_color,
                size = text_size)
  }
  if (nrow(venn$segs) > 0) {
    g <- g +
      geom_segment(data = venn$segs,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = text_color,
                   size = 0.5)
  }
  g <- g +
    scale_fill_manual(values = fill_color) +
    guides(fill = "none") +
    coord_fixed() +
    theme_void()
  return(g)
}

gen_element_df_2 <- function() {
  df <- tribble(~name, ~A,    ~B,
                "A",   TRUE,  FALSE,
                "B",   FALSE, TRUE,
                "AB",  TRUE,  TRUE,
                "-",   FALSE, FALSE)
  stopifnot(all((df %>% dplyr::count(A, B) %>% with(n)) == 1))
  return(df %>% mutate(n = 0, text = ""))
}
gen_element_df_3 <- function() {
  df <- tribble(~name, ~A,    ~B,    ~C,
                "A",   TRUE,  FALSE, FALSE,
                "B",   FALSE, TRUE,  FALSE,
                "C",   FALSE, FALSE, TRUE,
                "AB",  TRUE,  TRUE,  FALSE,
                "AC",  TRUE,  FALSE, TRUE,
                "BC",  FALSE, TRUE,  TRUE,
                "ABC", TRUE,  TRUE,  TRUE,
                "-",   FALSE, FALSE, FALSE)
  stopifnot(all((df %>% dplyr::count(A, B, C) %>% with(n)) == 1))
  return(df %>% mutate(n = 0, text = ""))
}
gen_element_df_4 <- function() {
  df <- tribble(~name, ~A,    ~B,    ~C,    ~D,
                "A",   TRUE,  FALSE, FALSE, FALSE,
                "B",   FALSE, TRUE,  FALSE, FALSE,
                "C",   FALSE, FALSE, TRUE,  FALSE,
                "D",   FALSE, FALSE, FALSE, TRUE,
                "AB",  TRUE,  TRUE,  FALSE, FALSE,
                "BC",  FALSE, TRUE,  TRUE,  FALSE,
                "CD",  FALSE, FALSE, TRUE,  TRUE,
                "AC",  TRUE,  FALSE, TRUE,  FALSE,
                "BD",  FALSE, TRUE,  FALSE, TRUE,
                "AD",  TRUE,  FALSE, FALSE, TRUE,
                "ABC", TRUE,  TRUE,  TRUE,  FALSE,
                "BCD", FALSE, TRUE,  TRUE,  TRUE,
                "ACD", TRUE,  FALSE, TRUE,  TRUE,
                "ABD", TRUE,  TRUE,  FALSE, TRUE,
                "ABCD",TRUE,  TRUE,  TRUE,  TRUE,
                "-",   FALSE, FALSE, FALSE, FALSE)
  stopifnot(all((df %>% dplyr::count(A, B, C, D) %>% with(n)) == 1))
  return(df %>% mutate(n = 0, text = ""))
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

calc_scale_info_2 <- function(auto_scale, n_sets, max_scale_diff = 5) {
  if (auto_scale) {
    stopifnot(length(n_sets) == 4)
    if (n_sets[[1]] == 0 && n_sets[[2]] == 0 && n_sets[[3]] == 0) { # both sets are empty
      a_radius <- 1
      b_radius <- 1
      overlap_size <- -0.2
    } else if (n_sets[[1]] + n_sets[[3]] == 0) { # set A is empty
      a_radius <- 1 / max_scale_diff
      b_radius <- 1
      overlap_size <- -0.2
    } else if (n_sets[[2]] + n_sets[[3]] == 0) { # set B is empty
      a_radius <- 1
      b_radius <- 1 / max_scale_diff
      overlap_size <- -0.2
    } else if (n_sets[[1]] >= n_sets[[2]]) { # set A is larger than or equal to set B
      a_radius <- 1
      b_radius <- (n_sets[[2]] + n_sets[[3]]) / (n_sets[[1]] + n_sets[[3]])
      overlap_size <- ifelse(n_sets[[3]] == 0, -0.2, n_sets[[3]] / (n_sets[[1]] + n_sets[[3]]))
      if (b_radius < 1 / max_scale_diff) {
        b_radius <- 1 / max_scale_diff
        if (overlap_size > 0) {
          overlap_size <- b_radius * (n_sets[[3]] / (n_sets[[2]] + n_sets[[3]]))
        }
      }
    } else { # set A is smaller than set B
      a_radius <- (n_sets[[1]] + n_sets[[3]]) / (n_sets[[2]] + n_sets[[3]])
      b_radius <- 1
      overlap_size <- ifelse(n_sets[[3]] == 0, -0.2, n_sets[[3]] / (n_sets[[2]] + n_sets[[3]]))
      if (a_radius < 1 / max_scale_diff) {
        a_radius <- 1 / max_scale_diff
        if (overlap_size > 0) {
          overlap_size <- a_radius * (n_sets[[3]] / (n_sets[[1]] + n_sets[[3]]))
        }
      }
    }
  } else {
    a_radius = 1
    b_radius = 1
    overlap_size = 1/3
  }
  return(c(auto_scale = auto_scale,
           a_radius = a_radius,
           b_radius = b_radius,
           overlap_size = overlap_size))
}
calc_scale_info_3 <- function(auto_scale, n_sets, max_scale_diff = 5) {
  if (auto_scale) {
    stop("Error: 'auto_scale' parameter is supported for only two set venn so far.")
  }
  return(NULL)
}
calc_scale_info_4 <- function(auto_scale, n_sets, max_scale_diff = 5) {
  if (auto_scale) {
    stop("Error: 'auto_scale' parameter is supported for only two set venn so far.")
  }
  return(NULL)
}

min_overlap_for_text <- 0.2

gen_circle_2 <- function(scale_info) {
  x_dist <- (scale_info['a_radius'] + scale_info['b_radius'] - scale_info['overlap_size'] * 2) / 2
  rbind(gen_circle(1L, -x_dist, 0, scale_info['a_radius']),
        gen_circle(2L, x_dist, 0, scale_info['b_radius']))
}
gen_text_pos_2 <- function(scale_info) {
  df <- tribble(~name, ~x,    ~y,  ~hjust, ~vjust,
                "A",   -0.8,  0,   0.5,    0.5,
                "B",    0.8,  0,   0.5,    0.5,
                "AB",   0,    0,   0.5,    0.5,
                "-",    0,   -1.2, 0.5,    0.5)
  if (scale_info['auto_scale']) {
    x_dist <- (scale_info['a_radius'] + scale_info['b_radius'] - scale_info['overlap_size'] * 2) / 2
    if (scale_info['overlap_size'] <= 0) {
      df$x[[1]] <- -x_dist
      df$x[[2]] <- x_dist
      df <- df %>% filter(name != "AB")
    } else {
      if (scale_info['overlap_size'] < min_overlap_for_text) {
        df$x[[1]] <- -x_dist - scale_info['overlap_size']
        df$x[[2]] <- x_dist + scale_info['overlap_size']
        if (scale_info['a_radius'] < min_overlap_for_text) {
          df$x[[3]] <- -x_dist + (scale_info['a_radius'] - scale_info['overlap_size']) / 2
          df$y[[3]] <- -1.5 * scale_info['a_radius']
        } else if (scale_info['b_radius'] < min_overlap_for_text) {
          df$x[[3]] <- x_dist - (scale_info['a_radius'] - scale_info['overlap_size']) / 2
          df$y[[3]] <- -1.5 * scale_info['b_radius']
        } else {
          df$x[[3]] <- -x_dist + scale_info['a_radius'] - scale_info['overlap_size']
          df$y[[3]] <- -1.2
        }
        df$x[[4]] <- -x_dist - scale_info['a_radius']
        df$y[[4]] <- -1.6
        df$hjust[[4]] <- 0
      } else {
        df$x[[1]] <- -x_dist - scale_info['overlap_size']
        df$x[[2]] <- x_dist + scale_info['overlap_size']
        df$x[[3]] <- -x_dist + scale_info['a_radius'] - scale_info['overlap_size']
      }
      if (scale_info['a_radius'] <= scale_info['overlap_size']) {
        df <- df %>% filter(name != "A")
      } else if (scale_info['b_radius'] <= scale_info['overlap_size']) {
        df <- df %>% filter(name != "B")
      }
    }
  }
  return(df)
}
gen_seg_pos_2 <- function(scale_info) {
  df <- tibble(x = 0, y = 0, xend = 0, yend = 0)[-1,]
  if (scale_info['overlap_size'] > 0 && scale_info['auto_scale']) {
    x_dist <- (scale_info['a_radius'] + scale_info['b_radius'] - scale_info['overlap_size'] * 2) / 2
    if (scale_info['overlap_size'] < min_overlap_for_text) {
      x_pos <- -x_dist + scale_info['a_radius'] - scale_info['overlap_size']
      if (scale_info['a_radius'] < min_overlap_for_text) {
        x2_pos <- -x_dist + 1.2 * (scale_info['a_radius'] - scale_info['overlap_size']) / 2
        df <- tibble(x = x_pos, y = 0, xend = x2_pos, yend = -1.2 * scale_info['a_radius'])
      } else if (scale_info['b_radius'] < min_overlap_for_text) {
        x2_pos <- x_dist - 1.2 * (scale_info['a_radius'] - scale_info['overlap_size']) / 2
        df <- tibble(x = x_pos, y = 0, xend = x2_pos, yend = -1.2 * scale_info['a_radius'])
      } else {
        df <- tibble(x = x_pos, y = 0, xend = x_pos, yend = -1)
      }
    }
  }
  return(df)
}
gen_label_pos_2 <- function(scale_info) {
  df <- tribble(~name, ~x,   ~y,  ~hjust, ~vjust,
                "A",   -0.8, 1.2, 0.5,    0,
                "B",    0.8, 1.2, 0.5,    0)
  if (scale_info['auto_scale']) {
  }
  return(df)
}

gen_circle_3 <- function() {
  rbind(gen_circle(1L, -2/3, (sqrt(3) + 2) / 6, 1),
        gen_circle(2L, 2/3,(sqrt(3) + 2) / 6, 1),
        gen_circle(3L, 0, -(sqrt(3) + 2) / 6, 1))
}
gen_text_pos_3 <- function() {
  tribble(~name, ~x,    ~y,   ~hjust, ~vjust,
          "A",   -0.8,  0.62, 0.5,    0.5,
          "B",    0.8,  0.62, 0.5,    0.5,
          "C",    0,   -0.62, 0.5,    0.5,
          "AB",   0,    0.8,  0.5,    0.5,
          "AC",  -0.5,  0,    0.5,    0.5,
          "BC",   0.5,  0,    0.5,    0.5,
          "ABC",  0,    0.2,  0.5,    0.5,
          "-",    1.2, -0.8,  0,      0.5)
}
gen_seg_pos_3 <- function(scale_info) {
  df <- tibble(x = 0, y = 0, xend = 0, yend = 0)[-1,]
  return(df)
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
  tribble(~name, ~x,    ~y,  ~hjust, ~vjust,
          "A",   -1.5,  0,   0.5,    0.5,
          "B",   -0.6,  0.7, 0.5,    0.5,
          "C",    0.6,  0.7, 0.5,    0.5,
          "D",    1.5,  0,   0.5,    0.5,
          "AB",  -0.9,  0.3, 0.5,    0.5,
          "BC",   0,    0.4, 0.5,    0.5,
          "CD",   0.9,  0.3, 0.5,    0.5,
          "AC",  -0.8, -0.9, 0.5,    0.5,
          "BD",   0.8, -0.9, 0.5,    0.5,
          "AD",   0,   -1.4, 0.5,    0.5,
          "ABC", -0.5, -0.2, 0.5,    0.5,
          "BCD",  0.5, -0.2, 0.5,    0.5,
          "ACD", -0.3, -1.1, 0.5,    0.5,
          "ABD",  0.3, -1.1, 0.5,    0.5,
          "ABCD", 0,   -0.7, 0.5,    0.5,
          "-",    0,   -1.9, 0.5,    0.5)
}
gen_seg_pos_4 <- function(scale_info) {
  df <- tibble(x = 0, y = 0, xend = 0, yend = 0)[-1,]
  return(df)
}
gen_label_pos_4 <- function() {
  tribble(~name, ~x,   ~y,   ~hjust, ~vjust,
          "A",   -1.5, -1.3, 1,      1,
          "B",   -0.8,  1.2, 0.5,    0,
          "C",    0.8,  1.2, 0.5,    0,
          "D",    1.5, -1.3, 0,      1)
}

prepare_venn_data <- function(data, columns = NULL,
                              show_elements = FALSE, show_percentage = TRUE, digits = 1,
                              label_sep = ",", count_column = NULL,
                              show_outside = c("auto", "none", "always"),
                              auto_scale = FALSE) {
  show_outside <- match.arg(show_outside)
  if (is.data.frame(data)) {
    if (is.null(columns)) {
      columns = data %>% select_if(is.logical) %>% names
    }
    if (!identical(show_elements, FALSE)) {
      if (!{
        if (is.character(show_elements)) {
          show_elements <- show_elements[[1]]
          show_elements %in% names(data)
          } else { FALSE }}) {
        stop("Value ", deparse(show_elements), 
             " in `show_elements` does not correspond to any column name of the data frame.",
             call. = FALSE)
      }
    }
    if (length(columns) == 2) {
      stopifnot(is.logical(as_tibble(data)[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[2]], drop = TRUE]))
      df_element <- gen_element_df_2()
      for (i in 1:nrow(df_element)) {
        idx <- ((!xor(df_element$A[[i]], as_tibble(data)[,columns[[1]]])) &
                  (!xor(df_element$B[[i]], as_tibble(data)[,columns[[2]]])))
        if (is.null(count_column)) {
          df_element$n[[i]] <- sum(idx)
        } else {
          df_element$n[[i]] <- sum(as_tibble(data)[,count_column][idx,])
        }
        if (!identical(show_elements, FALSE)) {
          df_element$text[[i]] <- paste(unlist(as_tibble(data)[idx,show_elements]), collapse = label_sep)
        }
      }
      scale_info <- calc_scale_info_2(auto_scale, df_element$n)
      df_shape <- gen_circle_2(scale_info)
      df_text <- gen_text_pos_2(scale_info) %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_2(scale_info)
      df_seg <- gen_seg_pos_2(scale_info)
    } else if (length(columns) == 3) {
      stopifnot(is.logical(as_tibble(data)[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[2]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[3]], drop = TRUE]))
      df_element <- gen_element_df_3()
      for (i in 1:nrow(df_element)) {
        idx <- ((!xor(df_element$A[[i]], as_tibble(data)[,columns[[1]]])) &
                  (!xor(df_element$B[[i]], as_tibble(data)[,columns[[2]]])) &
                  (!xor(df_element$C[[i]], as_tibble(data)[,columns[[3]]])))
        if (is.null(count_column)) {
          df_element$n[[i]] <- sum(idx)
        } else {
          df_element$n[[i]] <- sum(as_tibble(data)[,count_column][idx,])
        }
        if (!identical(show_elements, FALSE)) {
          df_element$text[[i]] <- paste(unlist(as_tibble(data)[idx,show_elements]), collapse = label_sep)
        }
      }
      scale_info <- calc_scale_info_3(auto_scale, df_element$n)
      df_shape <- gen_circle_3()
      df_text <- gen_text_pos_3() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_3()
      df_seg <- gen_seg_pos_3(scale_info)
    } else if (length(columns) == 4) {
      stopifnot(is.logical(as_tibble(data)[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[2]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[3]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[4]], drop = TRUE]))
      df_element <- gen_element_df_4()
      for (i in 1:nrow(df_element)) {
        idx <- ((df_element$A[[i]] == as_tibble(data)[,columns[[1]], drop = TRUE]) &
                  (df_element$B[[i]] == as_tibble(data)[,columns[[2]], drop = TRUE]) &
                  (df_element$C[[i]] == as_tibble(data)[,columns[[3]], drop = TRUE]) &
                  (df_element$D[[i]] == as_tibble(data)[,columns[[4]], drop = TRUE]))
        if (is.null(count_column)) {
          df_element$n[[i]] <- sum(idx)
        } else {
          df_element$n[[i]] <- sum(as_tibble(data)[,count_column][idx,])
        }
        if (!identical(show_elements, FALSE)) {
          df_element$text[[i]] <- paste(unlist(as_tibble(data)[idx,show_elements]), collapse = label_sep)
        }
      }
      scale_info <- calc_scale_info_4(auto_scale, df_element$n)
      df_shape <- gen_circle_4()
      df_text <- gen_text_pos_4() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_4()
      df_seg <- gen_seg_pos_4(scale_info)
    } else {
      stop("logical columns in data.frame `data` or vector `columns` should be length between 2 and 4")
    }
    df_label <- df_label %>% mutate(text = columns)
    show_elements <- !identical(show_elements, FALSE)
  } else if (is.list(data)) {
    if (is.null(columns)) {
      columns <- names(data) %>% head(4)
    }
    a2 <- na.omit(unique(unlist(data[columns])))
    if (length(columns) == 2) {
      df_element <- gen_element_df_2()
      for (i in 1:nrow(df_element)) {
        idx <- ((!xor(df_element$A[[i]], a2 %in% data[[columns[[1]]]])) &
                  (!xor(df_element$B[[i]], a2 %in% data[[columns[[2]]]])))
        df_element$n[[i]] <- sum(idx)
        df_element$text[[i]] <- paste(a2[idx], collapse = label_sep)
      }
      scale_info <- calc_scale_info_2(auto_scale, df_element$n)
      df_shape <- gen_circle_2(scale_info)
      df_text <- gen_text_pos_2(scale_info) %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_2(scale_info)
      df_seg <- gen_seg_pos_2(scale_info)
    } else if (length(columns) == 3) {
      df_element <- gen_element_df_3()
      for (i in 1:nrow(df_element)) {
        idx <- ((!xor(df_element$A[[i]], a2 %in% data[[columns[[1]]]])) &
                  (!xor(df_element$B[[i]], a2 %in% data[[columns[[2]]]])) &
                  (!xor(df_element$C[[i]], a2 %in% data[[columns[[3]]]])))
        df_element$n[[i]] <- sum(idx)
        df_element$text[[i]] <- paste(a2[idx], collapse = label_sep)
      }
      scale_info <- calc_scale_info_3(auto_scale, df_element$n)
      df_shape <- gen_circle_3()
      df_text <- gen_text_pos_3() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_3()
      df_seg <- gen_seg_pos_3(scale_info)
    } else if (length(columns) == 4) {
      df_element <- gen_element_df_4()
      for (i in 1:nrow(df_element)) {
        idx <- ((!xor(df_element$A[[i]], a2 %in% data[[columns[[1]]]])) &
                  (!xor(df_element$B[[i]], a2 %in% data[[columns[[2]]]])) &
                  (!xor(df_element$C[[i]], a2 %in% data[[columns[[3]]]])) &
                  (!xor(df_element$D[[i]], a2 %in% data[[columns[[4]]]])))
        df_element$n[[i]] <- sum(idx)
        df_element$text[[i]] <- paste(a2[idx], collapse = label_sep)
      }
      scale_info <- calc_scale_info_4(auto_scale, df_element$n)
      df_shape <- gen_circle_4()
      df_text <- gen_text_pos_4() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_4()
      df_seg <- gen_seg_pos_4(scale_info)
    } else {
      stop("list `data` or vector `column` should be length between 2 and 4")
    }
    df_label <- df_label %>% mutate(text = columns)
  } else {
    stop("`data` should be either a list or a data.frame")
  }
  if ((show_outside == "none") || (show_outside == "auto" && df_text$n[[nrow(df_text)]] == 0)) {
    if (df_text$n[[nrow(df_text)]] > 0)
      warning("Although not display in plot, outside elements are still count in percentages.")
    df_text <- df_text[-nrow(df_text), ]
  }
  if (!show_elements) {
    fmt <- sprintf("%%d\n(%%.%df%%%%)", digits)
    if (show_percentage) {
      df_text <- df_text %>% mutate(text = sprintf(fmt, n, 100 * n / sum(n)))
    } else {
      df_text <- df_text %>% mutate(text = sprintf("%d", n))
    }
  }
  list(shapes = df_shape, texts = df_text, labels = df_label, segs = df_seg)
}

