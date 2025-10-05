test_that("stats", {
  
  a <- list(A = 1:5, B = 1:2)
  
  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(a, show_percentage = FALSE)
    )
  )
  
  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(a, show_percentage = TRUE)
    )
  )
  
  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(a, show_percentage = TRUE, digits = 2)
    )
  )
})
