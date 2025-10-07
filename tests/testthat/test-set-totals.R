test_that("stats", {
  a <- list(
    "Set 1" = c(1, 3, 5, 7, 9),
    "Set 2" = c(1, 5, 9, 13),
    "Set 3" = c(1, 2, 8, 9),
    "Set 4" = c(6, 7, 10, 12)
  )

  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(
        a,
        show_percentage = FALSE,
        show_elements = FALSE,
        text_size = 6
      )
    )
  )

  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(
        a,
        show_set_totals = "c",
        show_elements = FALSE,
        text_size = 6
      )
    )
  )

  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(
        a,
        show_set_totals = "p",
        show_elements = FALSE,
        text_size = 6
      )
    )
  )

  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(
        a,
        show_set_totals = "cp",
        show_elements = FALSE,
        text_size = 6
      )
    )
  )
})
