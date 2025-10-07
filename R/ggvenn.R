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
#' @param show_percentage Show percentage for each set. Deprecated, use show_stats instead.
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
#' ggvenn(d, c("Set 1", "Set 2"), show_stats = 'c')
#'
#' # change precision of percentages
#' ggvenn(d, c("Set 1", "Set 2"), digits = 2)
#'
#' # show elements instead of count/percentage
#' ggvenn(a, show_elements = TRUE)
#' ggvenn(d, show_elements = "value")
#' @seealso geom_venn
#' @importFrom dplyr tibble tribble as_tibble %>% select_if mutate count filter inner_join
#' @importFrom ggplot2 ggplot aes geom_polygon geom_segment geom_text scale_x_continuous scale_y_continuous scale_fill_manual guides coord_fixed theme_void layer scale_x_discrete scale_y_discrete expansion
#' @importFrom stats na.omit
#' @export
ggvenn <- function(
  data,
  columns = NULL,
  show_elements = FALSE,
  show_set_totals = "none",
  show_stats = "cp",
  show_percentage = lifecycle::deprecated(),
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
  auto_scale = FALSE,
  comma_sep = FALSE,
  padding = 0.2
) {
  show_outside <- match.arg(show_outside)
  if (lifecycle::is_present(show_percentage)) {
    lifecycle::deprecate_soft(
      "0.1.11",
      "ggvenn::ggvenn(show_percentage = )",
      "ggvenn::ggvenn(show_stats = )"
    )
    show_stats <- if (show_percentage) "cp" else "c"
  }
  venn_data <- prepare_venn_data(
    data, columns, show_elements, show_set_totals,
    show_stats, digits, label_sep, count_column,
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

# Helper function to generate element data frames for different set counts
generate_element_df <- function(n_sets) {
  sets <- LETTERS[1:n_sets]

  # Generate all possible combinations
  combinations <- expand.grid(rep(list(c(TRUE, FALSE)), n_sets))
  names(combinations) <- sets

  # Create names for combinations
  combination_names <- apply(combinations, 1, function(row) {
    active_sets <- sets[row]
    if (length(active_sets) == 0) {
      "-"
    } else {
      paste(active_sets, collapse = "")
    }
  })

  # Create data frame
  df <- combinations
  df$name <- combination_names

  # Verify uniqueness
  count_cols <- sets
  count_result <- df %>% dplyr::count(!!!syms(count_cols))
  stopifnot(all(count_result$n == 1))

  df %>% mutate(n = 0, text = "")
}

gen_element_df_2 <- function() generate_element_df(2)
gen_element_df_3 <- function() generate_element_df(3)
gen_element_df_4 <- function() generate_element_df(4)

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
    a_radius <- 1
    b_radius <- 1
    overlap_size <- 1 / 3
  }
  c(
    auto_scale = auto_scale,
    a_radius = a_radius,
    b_radius = b_radius,
    overlap_size = overlap_size
  )
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

# Helper function to process data frame elements
process_data_frame_elements <- function(data, columns, n_sets, count_column, show_elements, label_sep) {
  # Validate logical columns
  for (i in seq_len(n_sets)) {
    stopifnot(is.logical(as_tibble(data)[, columns[[i]], drop = TRUE]))
  }

  # Generate element data frame
  if (n_sets == 2) {
    df_element <- gen_element_df_2()
  } else if (n_sets == 3) {
    df_element <- gen_element_df_3()
  } else if (n_sets == 4) {
    df_element <- gen_element_df_4()
  }

  # Process each combination
  for (i in seq_len(nrow(df_element))) {
    # Create index based on combination
    if (n_sets == 2) {
      idx <- ((!xor(df_element$A[[i]], as_tibble(data)[, columns[[1]]])) &
              (!xor(df_element$B[[i]], as_tibble(data)[, columns[[2]]])))
    } else if (n_sets == 3) {
      idx <- ((!xor(df_element$A[[i]], as_tibble(data)[, columns[[1]]])) &
              (!xor(df_element$B[[i]], as_tibble(data)[, columns[[2]]])) &
              (!xor(df_element$C[[i]], as_tibble(data)[, columns[[3]]])))
    } else if (n_sets == 4) {
      idx <- ((df_element$A[[i]] == as_tibble(data)[, columns[[1]], drop = TRUE]) &
              (df_element$B[[i]] == as_tibble(data)[, columns[[2]], drop = TRUE]) &
              (df_element$C[[i]] == as_tibble(data)[, columns[[3]], drop = TRUE]) &
              (df_element$D[[i]] == as_tibble(data)[, columns[[4]], drop = TRUE]))
    }

    # Count elements
    if (is.null(count_column)) {
      df_element$n[[i]] <- sum(idx)
    } else {
      df_element$n[[i]] <- sum(as_tibble(data)[, count_column][idx, ])
    }

    # Add text if showing elements
    if (!identical(show_elements, FALSE)) {
      df_element$text[[i]] <- paste(unlist(as_tibble(data)[idx, show_elements]), collapse = label_sep)
    }
  }

  df_element
}

# Helper function to process list elements
process_list_elements <- function(data, columns, all_elements, n_sets, label_sep) {
  # Generate element data frame
  if (n_sets == 2) {
    df_element <- gen_element_df_2()
  } else if (n_sets == 3) {
    df_element <- gen_element_df_3()
  } else if (n_sets == 4) {
    df_element <- gen_element_df_4()
  }

  # Process each combination
  for (i in seq_len(nrow(df_element))) {
    # Create index based on combination
    if (n_sets == 2) {
      idx <- ((!xor(df_element$A[[i]], all_elements %in% data[[columns[[1]]]])) &
              (!xor(df_element$B[[i]], all_elements %in% data[[columns[[2]]]])))
    } else if (n_sets == 3) {
      idx <- ((!xor(df_element$A[[i]], all_elements %in% data[[columns[[1]]]])) &
              (!xor(df_element$B[[i]], all_elements %in% data[[columns[[2]]]])) &
              (!xor(df_element$C[[i]], all_elements %in% data[[columns[[3]]]])))
    } else if (n_sets == 4) {
      idx <- ((!xor(df_element$A[[i]], all_elements %in% data[[columns[[1]]]])) &
              (!xor(df_element$B[[i]], all_elements %in% data[[columns[[2]]]])) &
              (!xor(df_element$C[[i]], all_elements %in% data[[columns[[3]]]])) &
              (!xor(df_element$D[[i]], all_elements %in% data[[columns[[4]]]])))
    }

    df_element$n[[i]] <- sum(idx)
    df_element$text[[i]] <- paste(all_elements[idx], collapse = label_sep)
  }

  df_element
}

prepare_venn_data <- function(
  data,
  columns = NULL,
  show_elements = FALSE,
  show_set_totals = "",
  show_stats = "cp",
  digits = 1,
  label_sep = ",",
  count_column = NULL,
  show_outside = c("auto", "none", "always"),
  auto_scale = FALSE,
  comma_sep = FALSE
) {
  show_outside <- match.arg(show_outside)
  if (is.data.frame(data)) {
    if (is.null(columns)) {
      columns <- data %>% select_if(is.logical) %>% names()
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
      df_element <- process_data_frame_elements(data, columns, 2, count_column, show_elements, label_sep)
      scale_info <- calc_scale_info_2(auto_scale, df_element$n)
      df_shape <- gen_circle_2(scale_info)
      df_text <- gen_text_pos_2(scale_info) %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_2(scale_info)
      df_seg <- gen_seg_pos_2(scale_info)
    } else if (length(columns) == 3) {
      df_element <- process_data_frame_elements(data, columns, 3, count_column, show_elements, label_sep)
      scale_info <- calc_scale_info_3(auto_scale, df_element$n)
      df_shape <- gen_circle_3()
      df_text <- gen_text_pos_3() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_3()
      df_seg <- gen_seg_pos_3(scale_info)
    } else if (length(columns) == 4) {
      df_element <- process_data_frame_elements(data, columns, 4, count_column, show_elements, label_sep)
      scale_info <- calc_scale_info_4(auto_scale, df_element$n)
      df_shape <- gen_circle_4()
      df_text <- gen_text_pos_4() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_4()
      df_seg <- gen_seg_pos_4(scale_info)
    } else {
      stop("logical columns in data.frame `data` or vector `columns` should be length between 2 and 4")
    }
    df_label <- df_label %>%
      mutate(
        text = calculate_totals(data, columns, show_set_totals, digits, comma_sep),
        hjust = 0.5
      )
    show_elements <- !identical(show_elements, FALSE)
  } else if (is.list(data)) {
    if (is.null(columns)) {
      columns <- names(data) %>% head(4)
    }
    all_elements <- na.omit(unique(unlist(data[columns])))
    if (length(columns) == 2) {
      df_element <- process_list_elements(data, columns, all_elements, 2, label_sep)
      scale_info <- calc_scale_info_2(auto_scale, df_element$n)
      df_shape <- gen_circle_2(scale_info)
      df_text <- gen_text_pos_2(scale_info) %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_2(scale_info)
      df_seg <- gen_seg_pos_2(scale_info)
    } else if (length(columns) == 3) {
      df_element <- process_list_elements(data, columns, all_elements, 3, label_sep)
      scale_info <- calc_scale_info_3(auto_scale, df_element$n)
      df_shape <- gen_circle_3()
      df_text <- gen_text_pos_3() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_3()
      df_seg <- gen_seg_pos_3(scale_info)
    } else if (length(columns) == 4) {
      df_element <- process_list_elements(data, columns, all_elements, 4, label_sep)
      scale_info <- calc_scale_info_4(auto_scale, df_element$n)
      df_shape <- gen_circle_4()
      df_text <- gen_text_pos_4() %>% inner_join(df_element, by = "name")
      df_label <- gen_label_pos_4()
      df_seg <- gen_seg_pos_4(scale_info)
    } else {
      stop("list `data` or vector `column` should be length between 2 and 4")
    }
    df_label <- df_label %>%
      mutate(
        text = calculate_totals(data, columns, show_set_totals, digits, comma_sep),
        hjust = 0.5
      )
  } else {
    stop("`data` should be either a list or a data.frame")
  }

  if (!show_elements) {
    fmt_count <- "%d"
    fmt_percentage <- sprintf("%%.%df%%%%", digits)
    fmt_both <- sprintf("%%d\n(%%.%df%%%%)", digits)

    if (comma_sep) {
      fmt_count <- "%s"
      fmt_percentage <- sprintf("%%.%df%%%%", digits)
      fmt_both <- sprintf("%%s\n(%%.%df%%%%)", digits)
    }

    total_count <- sum(df_text$n)

    df_text <- df_text %>% mutate(text = dplyr::case_when(
      show_stats == "c" ~ {
        if (comma_sep) {
          sprintf(fmt_count, scales::label_comma()(n))
        } else {
          sprintf(fmt_count, n)
        }
      },
      show_stats == "p" ~ sprintf(fmt_percentage, 100 * n / total_count),
      show_stats == "cp" ~ {
        if (comma_sep) {
          sprintf(fmt_both, scales::label_comma()(n), 100 * n / total_count)
        } else {
          sprintf(fmt_both, n, 100 * n / total_count)
        }
      },
      TRUE ~ ""
    ))
  }
  if ((show_outside == "none") || (show_outside == "auto" && df_text$n[[nrow(df_text)]] == 0)) {
    if (df_text$n[[nrow(df_text)]] > 0)
      warning("Although not display in plot, outside elements are still count in percentages.")
    df_text <- df_text[-nrow(df_text), ]
  }

  list(shapes = df_shape, texts = df_text, labels = df_label, segs = df_seg)
}

# Function to calculate set totals
calculate_totals <- function(data, columns, show_set_totals, digits, comma_sep) {
  # Calculate counts for each set
  set_counts <- sapply(columns, function(column) {
    if (inherits(data, "data.frame")){
      sum(data[[column]])
    } else {
      length(data[[column]])
    }
  })
  names(set_counts) <- columns

  # Calculate total number of elements
  total_elements <- if (inherits(data, "data.frame")) {
    nrow(data)
  } else {
    nrow(list_to_data_frame(data))
  }
  set_percentages <- set_counts / total_elements * 100

  if (comma_sep) {
    fmt_count <- "%s"
    set_counts <- sapply(set_counts, function(.x) sprintf(fmt_count, scales::label_comma()(.x)))
  }

  if (show_set_totals == "c") {
    return(
      paste0(names(set_counts), "\n", set_counts)
    )
  } else if (show_set_totals == "p") {
    return(
      paste0(names(set_counts), "\n", round(set_percentages, digits), "%")
    )
  } else if (show_set_totals == "cp") {
    return(
      paste0(names(set_counts), "\n", set_counts, " (", round(set_percentages, digits), "%)")
    )
  }

  columns
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
