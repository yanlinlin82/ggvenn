test_that("show_elements", {
      a <- list(A = c("apple", "pear", "peach"), B = c("apple", "lemon"))
      
  expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(a, show_elements = TRUE)
    )
  )

    expect_snapshot(
    normalize_ggvenn_output(
      ggvenn(a, show_elements = TRUE, label_sep = "\n")  # show elements in line
  )
  )
})
