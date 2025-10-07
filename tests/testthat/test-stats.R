test_that("stats", {
  a <- list(
    "Set 1" = c(1, 3, 5, 7, 9),
    "Set 2" = c(1, 5, 9, 13),
    "Set 3" = c(1, 2, 8, 9),
    "Set 4" = c(6, 7, 10, 12)
  )

  expect_snapshot(normalize_ggvenn_output(ggvenn(a)))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_percentage = FALSE)))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_percentage = TRUE)))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_percentage = TRUE, show_stats = "c")))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_percentage = TRUE, show_stats = "p")))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_percentage = TRUE, show_stats = "cp")))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_stats = "c")))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_stats = "p")))
  expect_snapshot(normalize_ggvenn_output(ggvenn(a, show_stats = "cp")))
})
