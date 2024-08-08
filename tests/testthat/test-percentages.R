test_that("stats", {
  
  a <- list(A = 1:5, B = 1:2)
  
  expect_snapshot(
    ggplot_build(
      ggvenn(a, show_percentage = FALSE)
    )$data
  )
  
  expect_snapshot(
    ggplot_build(
      ggvenn(a, show_percentage = TRUE)
    )$data
  )
  
  expect_snapshot(
    ggplot_build(
      ggvenn(a, show_percentage = TRUE, digits = 2)
    )$data
  )
})
