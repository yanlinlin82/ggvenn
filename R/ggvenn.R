library(dplyr)
library(ggplot2)

# Example:
#
# a <- list(`List 1` = c(1, 3, 5, 7, 9),
#           `List 2` = c(1, 5, 9, 13),
#           `List 3` = c(1, 2, 8, 9),
#           `List 4` = c(6, 7, 10, 12))
# ggvenn(a, c("List 1", "List 2"))
# ggvenn(a, c("List 1", "List 2", "List 3")))
# ggvenn(a)
#
# d <- data.frame(value = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 12, 13),
#                 `List 1` = c(T, F, T, T, F, T, F, T, F,  F,  F),
#                 `List 2` = c(T, F, F, T, F, F, F, T, F,  F,  T),
#                 `List 3` = c(T, T, F, F, F, F, T, T, F,  F,  F),
#                 `List 4` = c(F, F, F, F, T, T, F, F, T,  T,  F))
# ggvenn(d, c("List 1"))
# ggvenn(d, c("List 1", "List 2", "List 3"))
# ggvenn(d)

gen_circle <- function(name, x_offset = 0, y_offset = 0, radius = 1,
                       radius_b = radius, theta_offset = 0, length.out = 100) {
  tibble(group = name,
         theta = seq(0, 2 * pi, length.out = length.out)) %>%
  mutate(x_raw = radius * cos(theta),
         y_raw = radius_b * sin(theta),
         x = x_offset + x_raw * cos(theta_offset) - y_raw * sin(theta_offset),
         y = y_offset + x_raw * sin(theta_offset) + y_raw * cos(theta_offset))
}

gen_circle_2 <- function() {
  rbind(gen_circle("A", -2/3, 0, 1),
        gen_circle("B", 2/3, 0, 1))
}
gen_text_pos_2 <- function() {
  tribble(~name, ~x,   ~y, ~A,    ~B,
          "A",   -0.8, 0,  TRUE,  FALSE,
          "B",    0.8, 0,  FALSE, TRUE,
          "AB",   0,   0,  TRUE,  TRUE)
}
gen_label_pos_2 <- function() {
  tribble(~name, ~x,   ~y,
          "A",   -0.8, 1.2,
          "B",    0.8, 1.2)
}

gen_circle_3 <- function() {
  rbind(gen_circle("A", -2/3, (sqrt(3) + 2) / 6, 1),
        gen_circle("B", 2/3,(sqrt(3) + 2) / 6, 1),
        gen_circle("C", 0, -(sqrt(3) + 2) / 6, 1))
}
gen_text_pos_3 <- function() {
  tribble(~name, ~x,    ~y,   ~A,    ~B,    ~C,
          "A",   -0.8,  0.62, TRUE,  FALSE, FALSE,
          "B",    0.8,  0.62, FALSE, TRUE,  FALSE,
          "C",    0,   -0.62, FALSE, FALSE, TRUE,
          "AB",   0,    0.8,  TRUE,  TRUE,  FALSE,
          "AC",  -0.5,  0,    TRUE,  FALSE, TRUE,
          "BC",   0.5,  0,    FALSE, TRUE,  TRUE,
          "ABC",  0,    0.2,  TRUE,  TRUE,  TRUE)
}
gen_label_pos_3 <- function() {
  tribble(~name, ~x,    ~y,
          "A",   -0.8,  1.8,
          "B",    0.8,  1.8,
          "C",    0,   -1.8)
}

gen_circle_4 <- function() {
  rbind(gen_circle("A", -.7, -1/2, .75, 1.5, pi/4),
        gen_circle("B", -.72+2/3, -1/6, .75, 1.5, pi/4),
        gen_circle("C", .72-2/3, -1/6, .75, 1.5, -pi/4),
        gen_circle("D", .7, -1/2, .75, 1.5, -pi/4))
}
gen_text_pos_4 <- function() {
  tribble(~name, ~x,    ~y,  ~A,   ~B,    ~C,    ~D,
          "A",   -1.5,  0,   TRUE,  FALSE, FALSE, FALSE,
          "B",   -0.6,  0.7, FALSE, TRUE,  FALSE, FALSE,
          "C",    0.6,  0.7, FALSE, FALSE, TRUE,  FALSE,
          "D",    1.5,  0,   FALSE, FALSE, FALSE, TRUE,
          "AB",  -0.9,  0.3, TRUE,  TRUE,  FALSE, FALSE,
          "BC",   0,    0.4, FALSE, TRUE,  TRUE,  FALSE,
          "CD",   0.9,  0.3, FALSE, FALSE, TRUE,  TRUE,
          "AC",  -0.8, -0.9, TRUE,  FALSE, TRUE,  FALSE,
          "BD",   0.8, -0.9, FALSE, TRUE,  FALSE, TRUE,
          "AD",   0,   -1.4, TRUE,  FALSE, FALSE, TRUE,
          "ABC", -0.5, -0.2, TRUE,  TRUE,  TRUE,  FALSE,
          "BCD",  0.5, -0.2, FALSE, TRUE,  TRUE,  TRUE,
          "ACD", -0.3, -1.1, TRUE,  FALSE, TRUE,  TRUE,
          "BCD",  0.3, -1.1, FALSE, TRUE,  TRUE,  TRUE,
          "ABCD", 0,   -0.7, TRUE,  TRUE,  TRUE,  TRUE)
}
gen_label_pos_4 <- function() {
  tribble(~name, ~x,   ~y,
          "A",   -1.5, -1.1,
          "B",   -0.8,  1.2,
          "C",    0.8,  1.2,
          "D",    1.5, -1.1)
}

ggvenn <- function(data, columns = NULL) {

  if (is.data.frame(data)) {
    if (is.null(columns)) {
      columns = d %>% select_if(is.logical) %>% names
    }
    if (length(columns) == 2) {
      d <- gen_circle_2()
      d1 <- gen_text_pos_2() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], data[,columns[[1]]])) &
                         (!xor(d1$B[[i]], data[,columns[[2]]])))
      }
      d2 <- gen_label_pos_2()
    } else if (length(columns) == 3) {
      d <- gen_circle_3()
      d1 <- gen_text_pos_3() %>% mutate(n = 0)
      for (i in 1:nrow(d1)) {
        d1$n[[i]] <- sum((!xor(d1$A[[i]], data[,columns[[1]]])) &
                         (!xor(d1$B[[i]], data[,columns[[2]]])) &
                         (!xor(d1$C[[i]], data[,columns[[3]]])))
      }
      d2 <- gen_label_pos_3()
    } else if (length(columns) == 4) {
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

  d %>%
    ggplot() +
    geom_polygon(aes(x = x, y = y, group = group, fill = group), alpha = .5) +
    geom_polygon(aes(x = x, y = y, group = group, fill = NA), size = 1, color = "black") +
    scale_x_continuous(limits = c(-2, 2)) +
    scale_y_continuous(limits = c(-2, 2)) +
    geom_text(data = d1, aes(x = x, y = y, label = text)) +
    geom_text(data = d2, aes(x = x, y = y, label = text), size = 6) +
    scale_fill_manual(values = c("blue", "yellow", "green", "red")) +
    guides(fill = FALSE) +
    theme_void()
}
