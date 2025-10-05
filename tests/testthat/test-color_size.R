test_that("color and size", {
  a <- list(A = 1:4, B = c(1,3,5))
  
  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(a, stroke_linetype = 2, stroke_size = 0.5,
             set_name_color = "red", set_name_size = 15,
             fill_color = c("pink", "gold"))
    )
  )
})
