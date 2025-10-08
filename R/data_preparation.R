library(tibble)
library(dplyr)

#==========================================================#

# Helper function to truncate text when showing elements
truncate_element_text <- function(elements, max_elements, label_sep, text_truncate = TRUE) {
  if (!text_truncate || length(elements) <= max_elements) {
    return(paste(elements, collapse = label_sep))
  }

  if (max_elements <= 0) {
    return("")
  }

  # Show first max_elements elements and add "..."
  visible_elements <- elements[1:max_elements]
  text <- paste(visible_elements, collapse = label_sep)

  if (length(elements) > max_elements) {
    text <- paste0(text, label_sep, "... (+", length(elements) - max_elements, " more)")
  }

  text
}

#' Utility functions for data type conversion between data.frame and list.
#'
#' @name data_preparation
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

#' @rdname data_preparation
#' @export
list_to_data_frame <- function(x) {
  df <- tibble("_key" = unique(unlist(x)))
  for (name in names(x)) {
    df[, name] <- df$`_key` %in% x[[name]]
  }
  df
}

#==========================================================#

# Function to calculate set totals
calculate_totals <- function(data, columns, show_set_totals, digits, comma_sep) {
  # Calculate counts for each set
  set_counts <- sapply(columns, function(column) {
    if (inherits(data, "data.frame")) {
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
#' @importFrom rlang syms
generate_element_df <- function(n_sets) {
  sets <- LETTERS[seq_len(n_sets)]

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
  count_result <- df %>% dplyr::count(!!!rlang::syms(count_cols))
  stopifnot(all(count_result$n == 1))

  df <- df %>% mutate(n = 0, text = "")
  df
}

calc_scale_info_2 <- function(auto_scale, n_sets, max_scale_diff = 5, spacing_size = 0.2) {
  if (auto_scale) {
    stopifnot(length(n_sets) == 4)
    ab_shared <- n_sets[[1]]
    b_specific <- n_sets[[2]]
    a_specific <- n_sets[[3]]
    rest <- n_sets[[4]]
    if (ab_shared == 0 && b_specific == 0 && a_specific == 0) { # both sets are empty
      a_radius <- 1
      b_radius <- 1
      overlap_size <- -spacing_size
    } else if (ab_shared + a_specific == 0) { # set A is empty
      a_radius <- 1 / max_scale_diff
      b_radius <- 1
      overlap_size <- -spacing_size
    } else if (ab_shared + b_specific == 0) { # set B is empty
      a_radius <- 1
      b_radius <- 1 / max_scale_diff
      overlap_size <- -spacing_size
    } else if (ab_shared == 0) {
      if (a_specific > b_specific) {
        a_radius <- 1
        b_radius <- max(sqrt(b_specific / a_specific), 1 / max_scale_diff)
        overlap_size <- -spacing_size
      } else {
        a_radius <- max(sqrt(a_specific / b_specific), 1 / max_scale_diff)
        b_radius <- 1
        overlap_size <- -spacing_size
      }
    } else if (a_specific + b_specific == 0) { # both sets are empty
      a_radius <- 1
      b_radius <- 1
      overlap_size <- 0.99
    } else if (a_specific == 0) { # B contains A
      a_radius <- max(sqrt(ab_shared / (ab_shared + b_specific)), 1 / max_scale_diff)
      b_radius <- 1
      overlap_size <- a_radius
    } else if (b_specific == 0) { # A contains B
      a_radius <- 1
      b_radius <- max(sqrt(ab_shared / (ab_shared + a_specific)), 1 / max_scale_diff)
      overlap_size <- b_radius
    } else {
      if (a_specific > b_specific) { # A is larger than B
        a_radius <- 1
        b_radius <- max(sqrt((ab_shared + b_specific) / (ab_shared + a_specific)), 1 / max_scale_diff)
        overlap_size <- b_radius * (ab_shared / (ab_shared + b_specific))
      } else { # B is larger than A
        a_radius <- max(sqrt((ab_shared + a_specific) / (ab_shared + b_specific)), 1 / max_scale_diff)
        b_radius <- 1
        overlap_size <- a_radius * (ab_shared / (ab_shared + a_specific))
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

# Helper function to process data frame elements
process_data_frame_elements <- function(
  data,
  columns,
  n_sets,
  count_column,
  show_elements,
  label_sep,
  max_elements,
  text_truncate,
  element_column
) {
  # Validate logical columns
  for (i in seq_len(n_sets)) {
    stopifnot(is.logical(as_tibble(data)[, columns[[i]], drop = TRUE]))
  }

  # Generate element data frame
  df_element <- generate_element_df(n_sets)

  # Process each combination
  for (i in seq_len(nrow(df_element))) {
    # Create index based on combination
    idx <- NULL
    for (j in seq_len(n_sets)) {
      set_name <- LETTERS[j]
      current_idx <- (!xor(df_element[[set_name]][[i]], as_tibble(data)[, columns[[j]]]))
      if (is.null(idx)) {
        idx <- current_idx
      } else {
        idx <- idx & current_idx
      }
    }

    # Count elements
    if (is.null(count_column)) {
      df_element$n[[i]] <- sum(idx)
    } else {
      df_element$n[[i]] <- sum(as_tibble(data)[, count_column][idx, ])
    }

    if (!is.null(element_column) && element_column %in% names(data)) {
      df_element$values[[i]] <- sort(as.vector(unlist(as_tibble(data)[idx, element_column])))

      if (show_elements) {
        elements <- unlist(as_tibble(data)[idx, element_column])
        df_element$text[[i]] <- truncate_element_text(elements, max_elements, label_sep, text_truncate)
      }
    } else {
      df_element$values[[i]] <- character(0)
    }
  }

  df_element
}

prepare_venn_data <- function(
  data,
  columns = NULL,
  element_column = NULL,
  show_elements = FALSE,
  show_set_totals = "",
  show_counts = TRUE,
  show_percentage = TRUE,
  digits = 1,
  label_sep = ",",
  count_column = NULL,
  show_outside = c("auto", "none", "always"),
  auto_scale = FALSE,
  comma_sep = FALSE,
  max_elements = 6,
  text_truncate = TRUE
) {
  show_outside <- match.arg(show_outside)

  stopifnot(is.data.frame(data))

  set_names <- character(0)
  if (missing(columns)) {
    for (name in names(data)) {
      if (is.logical(data[[name]])) {
        set_names <- c(set_names, name)
      }
    }
  } else {
    for (name in columns) {
      stopifnot(name %in% names(data))
      stopifnot(is.logical(data[[name]]))
    }
    set_names <- columns
  }
  n_sets <- length(set_names)
  stopifnot(n_sets >= min_set_num && n_sets <= max_set_num)

  if (!missing(element_column) && !is.null(element_column)) {
    stopifnot(is.character(element_column))
    stopifnot(length(element_column) == 1)
    stopifnot(element_column %in% names(data))
  }

  get_venn_funcs <- function(n_sets) {
    funcs <- NULL
    if (n_sets == 2) {
      funcs <- list(
        "calc_scale_info" = calc_scale_info_2,
        "gen_circle" = gen_circle_2,
        "gen_text_pos" = gen_text_pos_2,
        "gen_label_pos" = gen_label_pos_2,
        "gen_seg_pos" = gen_seg_pos_2
      )
    } else if (n_sets == 3) {
      funcs <- list(
        "calc_scale_info" = NULL,
        "gen_circle" = gen_circle_3,
        "gen_text_pos" = gen_text_pos_3,
        "gen_label_pos" = gen_label_pos_3,
        "gen_seg_pos" = NULL
      )
    } else if (n_sets == 4) {
      funcs <- list(
        "calc_scale_info" = NULL,
        "gen_circle" = gen_circle_4,
        "gen_text_pos" = gen_text_pos_4,
        "gen_label_pos" = gen_label_pos_4,
        "gen_seg_pos" = NULL
      )
    } else if (n_sets == 5) {
      funcs <- list(
        "calc_scale_info" = NULL,
        "gen_circle" = gen_circle_5,
        "gen_text_pos" = gen_text_pos_5,
        "gen_label_pos" = gen_label_pos_5,
        "gen_seg_pos" = NULL
      )
    } else if (n_sets == 6) {
      funcs <- list(
        "calc_scale_info" = NULL,
        "gen_circle" = gen_circle_6,
        "gen_text_pos" = gen_text_pos_6,
        "gen_label_pos" = gen_label_pos_6,
        "gen_seg_pos" = NULL
      )
    } else if (n_sets == 7) {
      funcs <- list(
        "calc_scale_info" = NULL,
        "gen_circle" = gen_circle_7,
        "gen_text_pos" = gen_text_pos_7,
        "gen_label_pos" = gen_label_pos_7,
        "gen_seg_pos" = NULL
      )
    } else if (n_sets == 8) {
      funcs <- list(
        "calc_scale_info" = NULL,
        "gen_circle" = gen_circle_8,
        "gen_text_pos" = gen_text_pos_8,
        "gen_label_pos" = gen_label_pos_8,
        "gen_seg_pos" = NULL
      )
    } else {
      stop("logical columns in data.frame `data` or vector `columns` should be length between 2 and 5")
    }
    funcs
  }

  venn_funcs <- get_venn_funcs(n_sets)
  if (is.null(venn_funcs)) {
    stop("logical columns in data.frame `data` or vector `columns` should be length between 2 and 4")
  }

  df_element <- process_data_frame_elements(
    data, columns, length(columns), count_column, show_elements, label_sep, max_elements, text_truncate, element_column
  )

  if (auto_scale) {
    if (is.null(venn_funcs[["calc_scale_info"]])) {
      stop("Error: 'auto_scale' parameter is supported for only two set venn so far.")
    }
    scale_info <- venn_funcs[["calc_scale_info"]](auto_scale, df_element$n)
  } else {
    scale_info <- NULL
  }

  df_shape <- venn_funcs[["gen_circle"]](scale_info)

  df_text <- venn_funcs[["gen_text_pos"]](scale_info)
  df_text <- df_text %>% inner_join(df_element, by = "name")

  df_label <- venn_funcs[["gen_label_pos"]](scale_info)

  df_seg <- NULL
  if (!is.null(venn_funcs[["gen_seg_pos"]])) {
    df_seg <- venn_funcs[["gen_seg_pos"]](scale_info)
  }

  df_label <- df_label %>%
    mutate(
      text = calculate_totals(data, columns, show_set_totals, digits, comma_sep),
      hjust = 0.5
    )
  show_elements <- !identical(show_elements, FALSE)

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
      show_counts && show_percentage ~ {
        if (comma_sep) {
          sprintf(fmt_both, scales::label_comma()(n), 100 * n / total_count)
        } else {
          sprintf(fmt_both, n, 100 * n / total_count)
        }
      },
      show_counts ~ {
        if (comma_sep) {
          sprintf(fmt_count, scales::label_comma()(n))
        } else {
          sprintf(fmt_count, n)
        }
      },
      show_percentage ~ sprintf(fmt_percentage, 100 * n / total_count),
      TRUE ~ ""
    ))
  }
  if ((show_outside == "none") || (show_outside == "auto" && df_text$n[[nrow(df_text)]] == 0)) {
    if (df_text$n[[nrow(df_text)]] > 0)
      warning("Although not display in plot, outside elements are still count in percentages.")
    df_text <- df_text[-nrow(df_text), ]
  }

  list(
    shapes = df_shape,
    texts = df_text,
    labels = df_label,
    segs = df_seg,
    elements = df_element
  )
}
