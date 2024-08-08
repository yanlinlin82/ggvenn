test_that("show_elements", {
      a <- list(A = c("apple", "pear", "peach"), B = c("apple", "lemon"))
      
  expect_snapshot(
    ggplot_build(
      ggvenn(a, show_elements = TRUE)
    )$data
  )

    expect_snapshot(
    ggplot_build(
      ggvenn(a, show_elements = TRUE, label_sep = "\n")  # show elements in line
  )$data
  )
})
