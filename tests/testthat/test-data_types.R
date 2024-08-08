test_that("multiplication works", {
  a <- list(A = 1:5, B = 4:6)
  d <- dplyr::tibble(key = 1:6,
              A = c(rep(TRUE, 5), FALSE),
              B = rep(c(FALSE, TRUE), each = 3))
  
  expect_identical(a, data_frame_to_list(d))  # TRUE
  expect_identical(d, list_to_data_frame(a))  # TRUE
})
