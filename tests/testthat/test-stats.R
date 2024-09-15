test_that("stats", {
  a <- list(
    `Set 1` = c(1, 3, 5, 7, 9),
    `Set 2` = c(1, 5, 9, 13),
    `Set 3` = c(1, 2, 8, 9),
    `Set 4` = c(6, 7, 10, 12)
  )
  
  expect_snapshot(ggplot_build(ggvenn(a))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_percentage = FALSE))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_percentage = TRUE))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_percentage = TRUE, show_stats = 'c'))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_percentage = TRUE, show_stats = 'p'))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_percentage = TRUE, show_stats = 'cp'))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_stats = 'c'))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_stats = 'p'))$data)
  expect_snapshot(ggplot_build(ggvenn(a, show_stats = 'cp'))$data)
})
