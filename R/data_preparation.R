library(tibble)
library(dplyr)

#==========================================================#

#' Utility functions for data type conversion between data.frame and list.
#'
#' @name data_conversion
#' @param x A data.frame with logical columns representing sets, or a list of sets.
#' @return A list of sets or a data.frame with logical columns representing sets.
#' @examples
#' # Convert data.frame to list
#' d <- dplyr::tibble(name = 1:6,
#'             A = c(rep(TRUE, 5), FALSE),
#'             B = rep(c(FALSE, TRUE), each = 3))
#' print(d)
#' data_frame_to_list(d)
#'
#' # Convert list to data.frame
#' a <- list(A = 1:5, B = 4:6)
#' print(a)
#' list_to_data_frame(a)
#'
#' # Round-trip conversion
#' identical(a, data_frame_to_list(list_to_data_frame(a)))  # TRUE
#' identical(d, list_to_data_frame(data_frame_to_list(d)))  # TRUE
#' @export
data_frame_to_list <- function(x) {
  col_names <- colnames(x)
  logical_names <- colnames(x %>% select_if(is.logical))
  rest_names <- col_names[!col_names %in% logical_names]
  keys <- seq_len(nrow(x))
  if (length(rest_names) >= 1) {
    keys <- x[[rest_names[1]]]
  }
  lst <- lapply(logical_names, function(i) keys[x[[i]]])
  names(lst) <- logical_names
  lst
}

#' @rdname data_conversion
#' @export
list_to_data_frame <- function(x) {
  df <- tibble(key = unique(unlist(x)))
  for (name in names(x)) {
    df[, name] <- df$key %in% x[[name]]
  }
  df
}

#==========================================================#

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
