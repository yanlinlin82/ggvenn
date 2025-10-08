library(tibble)
library(dplyr)
library(ggplot2)

#==========================================================#

min_set_num <- 2
max_set_num <- 5

default_color_list <- c(
  "blue", "yellow", "green", "red",
  "purple", "orange", "pink", "brown"
)

#==========================================================#

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

gen_circle_list <- function(
  n,
  center_radius = 1,
  ellipse_a = 1.5,
  ellipse_b = 1,
  start_angle = pi / 2 + pi / 12,
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
  d
}

gen_label_pos_list <- function(name_list, radius = 2.6, start_angle = pi / 2) {
  n <- length(name_list)
  idx <- seq_len(n)
  d <- data.frame(name = name_list)
  d$x <- radius * cos(start_angle + (idx - 1) * 2 * pi / n)
  d$y <- radius * sin(start_angle + (idx - 1) * 2 * pi / n)
  d$hjust <- 0.5
  d$vjust <- 0.5
  d
}

#==========================================================#

gen_circle_2 <- function(scale_info) {
  if (is.null(scale_info)) {
    # Default values when auto_scale is FALSE
    a_radius <- 1
    b_radius <- 1
    overlap_size <- 1 / 3
  } else {
    a_radius <- scale_info["a_radius"]
    b_radius <- scale_info["b_radius"]
    overlap_size <- scale_info["overlap_size"]
  }
  x_dist <- (a_radius + b_radius - overlap_size * 2) / 2
  rbind(gen_circle(1L, -x_dist, 0, a_radius),
        gen_circle(2L, x_dist, 0, b_radius))
}

gen_text_pos_2 <- function(scale_info, min_overlap_for_text = 0.2) {
  df <- tribble(
    ~name, ~x,    ~y,  ~hjust, ~vjust,
    "A",   -0.8,  0,   0.5,    0.5,
    "B",    0.8,  0,   0.5,    0.5,
    "AB",   0,    0,   0.5,    0.5,
    "-",    0,   -1.2, 0.5,    0.5
  )
  if (!is.null(scale_info) && scale_info["auto_scale"]) {
    a_radius <- scale_info["a_radius"]
    b_radius <- scale_info["b_radius"]
    overlap_size <- scale_info["overlap_size"]
    x_dist <- (a_radius + b_radius - overlap_size * 2) / 2
    if (overlap_size <= 0) {
      df$x[[1]] <- -x_dist
      df$x[[2]] <- x_dist
      df <- df %>% filter(name != "-")
    } else {
      if (overlap_size < min_overlap_for_text) {
        df$x[[1]] <- -x_dist - overlap_size
        df$x[[2]] <- x_dist + overlap_size
        if (a_radius < min_overlap_for_text) {
          df$x[[3]] <- -x_dist + (a_radius - overlap_size) / 2
          df$y[[3]] <- -1.5 * a_radius
        } else if (b_radius < min_overlap_for_text) {
          df$x[[3]] <- x_dist - (a_radius - overlap_size) / 2
          df$y[[3]] <- -1.5 * b_radius
        } else {
          df$x[[3]] <- -x_dist + a_radius - overlap_size
          df$y[[3]] <- -1.2
        }
        df$x[[4]] <- -x_dist - a_radius
        df$y[[4]] <- -1.6
        df$hjust[[4]] <- 0
      } else {
        df$x[[1]] <- -x_dist - overlap_size
        df$x[[2]] <- x_dist + overlap_size
        df$x[[3]] <- -x_dist + a_radius - overlap_size
      }
      # Don't filter out individual sets even if radius <= overlap_size
      # This ensures that sets with elements are always displayed
      # if (a_radius <= overlap_size) {
      #   df <- df %>% filter(name != "A")
      # } else if (b_radius <= overlap_size) {
      #   df <- df %>% filter(name != "B")
      # }
    }
  }
  df
}

gen_seg_pos_2 <- function(scale_info, min_overlap_for_text = 0.2) {
  df <- tibble(x = 0, y = 0, xend = 0, yend = 0)[-1, ]
  if (!is.null(scale_info)) {
    a_radius <- scale_info["a_radius"]
    b_radius <- scale_info["b_radius"]
    overlap_size <- scale_info["overlap_size"]
    if (overlap_size > 0 && scale_info["auto_scale"]) {
      x_dist <- (a_radius + b_radius - overlap_size * 2) / 2
      if (scale_info["overlap_size"] < min_overlap_for_text) {
        x_pos <- -x_dist + a_radius - overlap_size
        if (a_radius < min_overlap_for_text) {
          x2_pos <- -x_dist + 1.2 * (a_radius - overlap_size) / 2
          df <- tibble(x = x_pos, y = 0, xend = x2_pos, yend = -1.2 * a_radius)
        } else if (b_radius < min_overlap_for_text) {
          x2_pos <- x_dist - 1.2 * (a_radius - overlap_size) / 2
          df <- tibble(x = x_pos, y = 0, xend = x2_pos, yend = -1.2 * a_radius)
        } else {
          df <- tibble(x = x_pos, y = 0, xend = x_pos, yend = -1)
        }
      }
    }
  }
  return(df)
}

gen_label_pos_2 <- function(scale_info) {
  df <- tribble(
    ~name, ~x,   ~y,  ~hjust, ~vjust,
    "A",   -0.8, 1.2, 0.5,    0,
    "B",    0.8, 1.2, 0.5,    0
  )
  if (!is.null(scale_info) && scale_info["auto_scale"]) {
  }
  df
}

#==========================================================#

gen_circle_3 <- function(scale_info) {
  rbind(
    gen_circle(1L, -2/3, (sqrt(3) + 2) / 6, 1),
    gen_circle(2L, 2/3,(sqrt(3) + 2) / 6, 1),
    gen_circle(3L, 0, -(sqrt(3) + 2) / 6, 1)
  )
}

gen_text_pos_3 <- function(scale_info, min_overlap_for_text = 0.2) {
  tribble(
    ~name, ~x,    ~y,   ~hjust, ~vjust,
    "A",   -0.8,  0.62, 0.5,    0.5,
    "B",    0.8,  0.62, 0.5,    0.5,
    "C",    0,   -0.62, 0.5,    0.5,
    "AB",   0,    0.8,  0.5,    0.5,
    "AC",  -0.5,  0,    0.5,    0.5,
    "BC",   0.5,  0,    0.5,    0.5,
    "ABC",  0,    0.2,  0.5,    0.5,
    "-",    1.2, -0.8,  0,      0.5
  )
}

gen_label_pos_3 <- function(scale_info) {
  tribble(
    ~name, ~x,    ~y,  ~hjust, ~vjust,
    "A",   -0.8,  1.8, 0.5,    0,
    "B",    0.8,  1.8, 0.5,    0,
    "C",    0,   -1.8, 0.5,    1
  )
}

#==========================================================#

gen_circle_4 <- function(scale_info) {
  rbind(
    gen_circle(1L, -.7, -1/2, .75, 1.5, pi/4),
    gen_circle(2L, -.72+2/3, -1/6, .75, 1.5, pi/4),
    gen_circle(3L, .72-2/3, -1/6, .75, 1.5, -pi/4),
    gen_circle(4L, .7, -1/2, .75, 1.5, -pi/4)
  )
}

gen_text_pos_4 <- function(scale_info, min_overlap_for_text = 0.2) {
  tribble(
    ~name, ~x,    ~y,  ~hjust, ~vjust,
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
    "-",    0,   -1.9, 0.5,    0.5
  )
}

gen_label_pos_4 <- function(scale_info) {
  tribble(
    ~name, ~x,   ~y,   ~hjust, ~vjust,
    "A",   -1.5, -1.3, 1,      1,
    "B",   -0.8,  1.2, 0.5,    0,
    "C",    0.8,  1.2, 0.5,    0,
    "D",    1.5, -1.3, 0,      1
  )
}

#==========================================================#

try_plot <- function(
  n,
  center_radius = 1,
  ellipse_a = 1.5,
  ellipse_b = 1,
  start_angle = pi / 2,
  rotation_offset = -pi / 6
) {
  d <- gen_circle_list(
    n, center_radius, ellipse_a, ellipse_b,
    start_angle, rotation_offset
  )
  d$group <- LETTERS[d$group]
  g <- ggplot(d) +
    geom_polygon(
      aes(x = x, y = y, group = group, fill = group),
      alpha = 0.5
    ) +
    scale_fill_manual(
      values = default_color_list[(seq_len(n) %% length(default_color_list)) + 1]
    ) +
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

#==========================================================#

gen_circle_5 <- function(scale_info) {
  gen_circle_list(5, 1, 3.1, 1.5, pi * 0.45, pi * 0.1)
}

gen_text_pos_5 <- function(scale_info, min_overlap_for_text = 0.2) {
  rbind(
    data.frame(
      name = "ABCDE", x = 0, y = 0, hjust = 0.5, vjust = 0.5
    ),
    gen_label_pos_list(
      c("BCDE", "ACDE", "ABDE", "ABCE", "ABCD"),
      radius = 1.42, start_angle = pi * 1.12
    ),
    gen_label_pos_list(
      c("CDE", "ADE", "ABE", "ABC", "BCD"),
      radius = 1.55, start_angle = pi * 1.33
    ),
    gen_label_pos_list(
      c("BCE", "ACD", "BDE", "ACE", "ABD"),
      radius = 1.88, start_angle = pi * 1.13
    ),
    gen_label_pos_list(
      c("AC", "BD", "CE", "AD", "BE"),
      radius = 1.98, start_angle = pi * 0.44
    ),
    gen_label_pos_list(
      c("AB", "BC", "CD", "DE", "AE"),
      radius = 2.05, start_angle = pi * 0.68
    ),
    gen_label_pos_list(
      c("A", "B", "C", "D", "E"),
      radius = 3, start_angle = pi * 0.52
    ),
    data.frame(
      name = "-", x = 0, y = -4, hjust = 0.5, vjust = 0.5
    )
  )
}

gen_label_pos_5 <- function(scale_info) {
  gen_label_pos_list(LETTERS[seq_len(5)], radius = 4.5, start_angle = pi / 2)
}

#==========================================================#

gen_circle_6 <- function(scale_info) {
  gen_circle_list(6)
}

gen_text_pos_6 <- function(scale_info, min_overlap_for_text = 0.2) {
  tribble(
    ~name, ~x,    ~y,  ~hjust, ~vjust,
    "A",   -0.8,  0.62, 0.5,    0.5,
    "B",    0.8,  0.62, 0.5,    0.5,
    "C",    0,   -0.62, 0.5,    0.5,
    "D",    0.8, -0.62, 0.5,    0.5,
    "E",   -0.8, -0.62, 0.5,    0.5,
    "AB",   0,    0.8,  0.5,    0.5,
    "AC",  -0.5,  0,    0.5,    0.5,
    "AD",   0.5,  0,    0.5,    0.5,
    "AE",  -0.5,  0,    0.5,    0.5,
    "BC",   0,    0.4, 0.5,    0.5,
    "BD",   0.5,  0.4, 0.5,    0.5,
    "BE",  -0.5,  0.4, 0.5,    0.5,
    "CD",   0.5, -0.4, 0.5,    0.5,
    "CE",  -0.5, -0.4, 0.5,    0.5,
    "DE",   0,   -0.4, 0.5,    0.5,
    "ABC", -0.5, -0.2, 0.5,    0.5,
    "ABD",  0.5, -0.2, 0.5,    0.5,
    "ABE",  -0.5, -0.2, 0.5,    0.5,
    "ACD",  0.5, -0.2, 0.5,    0.5,
    "ACE",  -0.5, -0.2, 0.5,    0.5,
    "ADE",  0.5, -0.2, 0.5,    0.5,
    "BCD",  0.5, -0.2, 0.5,    0.5,
    "BCE",  -0.5, -0.2, 0.5,    0.5,
    "BDE",  0.5, -0.2, 0.5,    0.5,
    "CDE",  -0.5, -0.2, 0.5,    0.5,
    "ABCDE", 0,   -0.2, 0.5,    0.5,
    "-",    0,   -1.9, 0.5,    0.5
  )
}

gen_label_pos_6 <- function(scale_info) {
  gen_label_pos_list(LETTERS[seq_len(6)])
}

#==========================================================#

gen_circle_7 <- function(scale_info) {
  gen_circle_list(7)
}

gen_text_pos_7 <- function(scale_info, min_overlap_for_text = 0.2) {
  tribble(
    ~name, ~x,    ~y,  ~hjust, ~vjust,
    "A",   -0.8,  0.62, 0.5,    0.5,
    "B",    0.8,  0.62, 0.5,    0.5,
    "C",    0,   -0.62, 0.5,    0.5,
    "D",    0.8, -0.62, 0.5,    0.5,
    "E",   -0.8, -0.62, 0.5,    0.5,
    "AB",   0,    0.8,  0.5,    0.5,
    "AC",  -0.5,  0,    0.5,    0.5,
    "AD",   0.5,  0,    0.5,    0.5,
    "AE",  -0.5,  0,    0.5,    0.5,
    "BC",   0,    0.4, 0.5,    0.5,
    "BD",   0.5,  0.4, 0.5,    0.5,
    "BE",  -0.5,  0.4, 0.5,    0.5,
    "CD",   0.5, -0.4, 0.5,    0.5,
    "CE",  -0.5, -0.4, 0.5,    0.5,
    "DE",   0,   -0.4, 0.5,    0.5,
    "ABC", -0.5, -0.2, 0.5,    0.5,
    "ABD",  0.5, -0.2, 0.5,    0.5,
    "ABE",  -0.5, -0.2, 0.5,    0.5,
    "ACD",  0.5, -0.2, 0.5,    0.5,
    "ACE",  -0.5, -0.2, 0.5,    0.5,
    "ADE",  0.5, -0.2, 0.5,    0.5,
    "BCD",  0.5, -0.2, 0.5,    0.5,
    "BCE",  -0.5, -0.2, 0.5,    0.5,
    "BDE",  0.5, -0.2, 0.5,    0.5,
    "CDE",  -0.5, -0.2, 0.5,    0.5,
    "ABCDE", 0,   -0.2, 0.5,    0.5,
    "-",    0,   -1.9, 0.5,    0.5
  )
}

gen_label_pos_7 <- function(scale_info) {
  gen_label_pos_list(LETTERS[seq_len(7)])
}

#==========================================================#

gen_circle_8 <- function(scale_info) {
  gen_circle_list(8)
}

gen_text_pos_8 <- function(scale_info, min_overlap_for_text = 0.2) {
  tribble(
    ~name, ~x,    ~y,  ~hjust, ~vjust,
    "A",   -0.8,  0.62, 0.5,    0.5,
    "B",    0.8,  0.62, 0.5,    0.5,
    "C",    0,   -0.62, 0.5,    0.5,
    "D",    0.8, -0.62, 0.5,    0.5,
    "E",   -0.8, -0.62, 0.5,    0.5,
    "AB",   0,    0.8,  0.5,    0.5,
    "AC",  -0.5,  0,    0.5,    0.5,
    "AD",   0.5,  0,    0.5,    0.5,
    "AE",  -0.5,  0,    0.5,    0.5,
    "BC",   0,    0.4, 0.5,    0.5,
    "BD",   0.5,  0.4, 0.5,    0.5,
    "BE",  -0.5,  0.4, 0.5,    0.5,
    "CD",   0.5, -0.4, 0.5,    0.5,
    "CE",  -0.5, -0.4, 0.5,    0.5,
    "DE",   0,   -0.4, 0.5,    0.5,
    "ABC", -0.5, -0.2, 0.5,    0.5,
    "ABD",  0.5, -0.2, 0.5,    0.5,
    "ABE",  -0.5, -0.2, 0.5,    0.5,
    "ACD",  0.5, -0.2, 0.5,    0.5,
    "ACE",  -0.5, -0.2, 0.5,    0.5,
    "ADE",  0.5, -0.2, 0.5,    0.5,
    "BCD",  0.5, -0.2, 0.5,    0.5,
    "BCE",  -0.5, -0.2, 0.5,    0.5,
    "BDE",  0.5, -0.2, 0.5,    0.5,
    "CDE",  -0.5, -0.2, 0.5,    0.5,
    "ABCDE", 0,   -0.2, 0.5,    0.5,
    "-",    0,   -1.9, 0.5,    0.5
  )
}

gen_label_pos_8 <- function(scale_info) {
  gen_label_pos_list(LETTERS[seq_len(8)])
}

#==========================================================#
