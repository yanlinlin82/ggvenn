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
#' @param label_sep separator character for displaying elements.
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
#' @importFrom dplyr tibble tribble as_tibble %>% select_if mutate count
#' @importFrom ggplot2 ggplot aes geom_polygon geom_text scale_x_continuous scale_y_continuous scale_fill_manual guides coord_fixed theme_void layer
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
                   label_sep = ",") {
  venn <- prepare_venn_data(data, columns, show_elements, show_percentage, digits, label_sep)
  venn$shapes %>%
    mutate(group = LETTERS[group]) %>%
    ggplot() +
    geom_polygon(aes(x = x, y = y, group = group, fill = group),
                 alpha = fill_alpha) +
    geom_polygon(aes(x = x, y = y, group = group, fill = NA),
                 color = stroke_color,
                 size = stroke_size,
                 alpha = stroke_alpha,
                 linetype = stroke_linetype) +
    geom_text(data = venn$labels,
              aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
              color = set_name_color,
              size = set_name_size) +
    geom_text(data = venn$texts,
              aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
              color = text_color,
              size = text_size) +
    scale_x_continuous(limits = c(-2, 2)) +
    scale_y_continuous(limits = c(-2, 2)) +
    scale_fill_manual(values = fill_color) +
    guides(fill = FALSE) +
    coord_fixed() +
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
          "ABD",  0.3, -1.1, 0.5,    0.5,    TRUE,  TRUE,  FALSE, TRUE,
          "ABCD", 0,   -0.7, 0.5,    0.5,    TRUE,  TRUE,  TRUE,  TRUE)
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
                              label_sep = ",") {
  if (is.data.frame(data)) {
    if (is.null(columns)) {
      columns = data %>% select_if(is.logical) %>% names
    }
    if (!identical(show_elements, FALSE)) {
      stopifnot(is.character(show_elements))
      show_elements <- show_elements[[1]]
      if (!(show_elements %in% names(data))) {
        stop("`show_elements` should be one column name of the data frame")
      }
    }
    if (length(columns) == 2) {
      stopifnot(is.logical(as_tibble(data)[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[2]], drop = TRUE]))
      d <- gen_circle_2()
      d1 <- gen_text_pos_2() %>% mutate(n = 0, text = "")
      stopifnot((d1 %>% count(A, B, wt = 1) %>% with(n)) == 1)
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], as_tibble(data)[,columns[[1]]])) &
                  (!xor(d1$B[[i]], as_tibble(data)[,columns[[2]]])))
        d1$n[[i]] <- sum(idx)
        if (!identical(show_elements, FALSE)) {
          d1$text[[i]] <- paste(unlist(as_tibble(data)[idx,show_elements]), collapse = label_sep)
        }
      }
      d2 <- gen_label_pos_2()
    } else if (length(columns) == 3) {
      stopifnot(is.logical(as_tibble(data)[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[2]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[3]], drop = TRUE]))
      d <- gen_circle_3()
      d1 <- gen_text_pos_3() %>% mutate(n = 0, text = "")
      stopifnot((d1 %>% count(A, B, C, wt = 1) %>% with(n)) == 1)
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], as_tibble(data)[,columns[[1]]])) &
                  (!xor(d1$B[[i]], as_tibble(data)[,columns[[2]]])) &
                  (!xor(d1$C[[i]], as_tibble(data)[,columns[[3]]])))
        d1$n[[i]] <- sum(idx)
        if (!identical(show_elements, FALSE)) {
          d1$text[[i]] <- paste(unlist(as_tibble(data)[idx,show_elements]), collapse = label_sep)
        }
      }
      d2 <- gen_label_pos_3()
    } else if (length(columns) == 4) {
      stopifnot(is.logical(as_tibble(data)[,columns[[1]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[2]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[3]], drop = TRUE]))
      stopifnot(is.logical(as_tibble(data)[,columns[[4]], drop = TRUE]))
      d <- gen_circle_4()
      d1 <- gen_text_pos_4() %>% mutate(n = 0, text = "")
      stopifnot((d1 %>% count(A, B, C, D, wt = 1) %>% with(n)) == 1)
      for (i in 1:nrow(d1)) {
        idx <- ((d1$A[[i]] == as_tibble(data)[,columns[[1]], drop = TRUE]) &
                  (d1$B[[i]] == as_tibble(data)[,columns[[2]], drop = TRUE]) &
                  (d1$C[[i]] == as_tibble(data)[,columns[[3]], drop = TRUE]) &
                  (d1$D[[i]] == as_tibble(data)[,columns[[4]], drop = TRUE]))
        d1$n[[i]] <- sum(idx)
        if (!identical(show_elements, FALSE)) {
          d1$text[[i]] <- paste(unlist(as_tibble(data)[idx,show_elements]), collapse = label_sep)
        }
      }
      d2 <- gen_label_pos_4()
    } else {
      stop("logical columns in data.frame `data` or vector `columns` should be length between 2 and 4")
    }
    d2 <- d2 %>% mutate(text = columns)
    show_elements <- !identical(show_elements, FALSE)
  } else if (is.list(data)) {
    if (is.null(columns)) {
      columns <- names(data) %>% head(4)
    }
    a2 <- unique(unlist(data[columns]))
    if (length(columns) == 2) {
      d <- gen_circle_2()
      d1 <- gen_text_pos_2() %>% mutate(n = 0, text = "")
      stopifnot((d1 %>% count(A, B, wt = 1) %>% with(n)) == 1)
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], a2 %in% data[[columns[[1]]]])) &
                  (!xor(d1$B[[i]], a2 %in% data[[columns[[2]]]])))
        d1$n[[i]] <- sum(idx)
        d1$text[[i]] <- paste(a2[idx], collapse = label_sep)
      }
      d2 <- gen_label_pos_2()
    } else if (length(columns) == 3) {
      d <- gen_circle_3()
      d1 <- gen_text_pos_3() %>% mutate(n = 0, text = "")
      stopifnot((d1 %>% count(A, B, C, wt = 1) %>% with(n)) == 1)
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], a2 %in% data[[columns[[1]]]])) &
                  (!xor(d1$B[[i]], a2 %in% data[[columns[[2]]]])) &
                  (!xor(d1$C[[i]], a2 %in% data[[columns[[3]]]])))
        d1$n[[i]] <- sum(idx)
        d1$text[[i]] <- paste(a2[idx], collapse = label_sep)
      }
      d2 <- gen_label_pos_3()
    } else if (length(columns) == 4) {
      d <- gen_circle_4()
      d1 <- gen_text_pos_4() %>% mutate(n = 0, text = "")
      stopifnot((d1 %>% count(A, B, C, D, wt = 1) %>% with(n)) == 1)
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], a2 %in% data[[columns[[1]]]])) &
                  (!xor(d1$B[[i]], a2 %in% data[[columns[[2]]]])) &
                  (!xor(d1$C[[i]], a2 %in% data[[columns[[3]]]])) &
                  (!xor(d1$D[[i]], a2 %in% data[[columns[[4]]]])))
        d1$n[[i]] <- sum(idx)
        d1$text[[i]] <- paste(a2[idx], collapse = label_sep)
      }
      d2 <- gen_label_pos_4()
    } else {
      stop("list `data` or vector `column` should be length between 2 and 4")
    }
    d2 <- d2 %>% mutate(text = columns)
  } else {
    stop("`data` should be a list")
  }
  if (!show_elements) {
    if (show_percentage) {
      fmt <- sprintf("%%d\n(%%.%df%%%%)", digits)
      d1 <- d1 %>% mutate(text = sprintf(fmt, n, 100 * n / sum(n)))
    } else {
      d1 <- d1 %>% mutate(text = sprintf("%d", n, 100 * n / sum(n)))
    }
  }
  list(shapes = d, texts = d1, labels = d2)
}
