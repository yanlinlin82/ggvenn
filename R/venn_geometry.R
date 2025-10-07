library(tibble)
library(dplyr)
library(ggplot2)

gen_circle <- function(
  group, x_offset = 0, y_offset = 0, radius = 1,
  radius_b = radius, theta_offset = 0, length.out = 100
) {
  d <- tibble(
    group = group,
    theta = seq(0, 2 * pi, length.out = length.out)
  )
  d <- d %>%
    mutate(
      x_raw = radius * cos(theta),
      y_raw = radius_b * sin(theta),
      x = x_offset + x_raw * cos(theta_offset) - y_raw * sin(theta_offset),
      y = y_offset + x_raw * sin(theta_offset) + y_raw * cos(theta_offset)
    )
  d
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

try_plot <- function(
  n,
  center_radius = 1,
  ellipse_a = 1.5,
  ellipse_b = 1,
  start_angle = pi / 2,
  rotation_offset = -pi / 6
) {
  d <- list()
  for (i in seq_len(n)) {
    theta <- start_angle + 2 * pi * (i - 1) / n
    x <- center_radius * cos(theta)
    y <- center_radius * sin(theta)
    c <- gen_circle(i, x, y, ellipse_a, ellipse_b, theta + rotation_offset)
    d[[i]] <- c
  }
  d <- do.call(rbind, d)
  d$group <- as.character(d$group)
  g <- ggplot(d) +
    geom_polygon(
      aes(x = x, y = y, group = group, fill = group),
      alpha = 0.5
    ) +
    scale_fill_manual(
      values = c(
        "red", "blue", "green", "yellow", "purple",
        "orange", "pink", "brown", "gray"
      )) +
    geom_polygon(
      aes(x = x, y = y, group = group),
      color = "black",
      linewidth = 0.5,
      alpha = 0
    ) +
    coord_fixed() +
    theme_void()
  print(g)
  d
}

#try_plot(2, ellipse_b = 1.5, start_angle = pi)
#try_plot(2, ellipse_b = 1.5)
#
#try_plot(3, ellipse_b = 1.5, start_angle = pi / 2)
#try_plot(3, ellipse_b = 1.5, start_angle = pi * 2 / 3)
#try_plot(3, ellipse_b = 1.5, start_angle = pi * 5 / 6)
#try_plot(3, ellipse_b = 1.5, start_angle = pi)
#
#try_plot(4, ellipse_b = 1.5, rotation_offset = 0)
#try_plot(4, ellipse_b = 1.5, start_angle = pi * 3 / 4)
#try_plot(4, ellipse_b = 1)
#
#try_plot(5, ellipse_b = 1.5, rotation_offset = 0)
#try_plot(5, ellipse_b = 1)
#
#try_plot(6, ellipse_b = 1.5, rotation_offset = 0)
#try_plot(6, ellipse_b = 1)
#
#try_plot(7, ellipse_b = 1.5, rotation_offset = 0)
#try_plot(7, ellipse_b = 1)
#
#try_plot(8, ellipse_b = 1.5, rotation_offset = 0)
#try_plot(8, ellipse_b = 1, rotation_offset = -pi / 6)
