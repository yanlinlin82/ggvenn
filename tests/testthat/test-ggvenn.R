test_that("ggvenn + list", {
  #library(ggvenn)
  
  a <- list("Set 1" = c(1, 3, 5, 7, 9),
            "Set 2" = c(1, 5, 9, 13),
            "Set 3" = c(1, 2, 8, 9),
            "Set 4" = c(6, 7, 10, 12))
  
  expect_snapshot(
    normalize_ggvenn_output(ggvenn(a, c("Set 1", "Set 2")))            # draw two-set venn
  )
  
 expect_snapshot(
    normalize_ggvenn_output(ggvenn(a, c("Set 1", "Set 2", "Set 3")))   # draw three-set venn
 )
 
 expect_snapshot(
 normalize_ggvenn_output(ggvenn(a))   # without set names, the first 4 elements in list will be chose to draw four-set venn
 )
})

test_that("ggvenn + data.frame", {
  
  d <- dplyr::tibble(value   = c(1,     2,     3,     5,     6,     7,     8,     9),
              `Set 1` = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE),
              `Set 2` = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE),
              `Set 3` = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE),
              `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE))
  
  expect_snapshot(normalize_ggvenn_output(ggvenn(d, c("Set 1", "Set 2"))))           # draw two-set venn
  expect_snapshot(normalize_ggvenn_output(ggvenn(d, c("Set 1", "Set 2", "Set 3"))))  # draw three-set venn
  expect_snapshot(normalize_ggvenn_output(ggvenn(d)))   # without set names, the first 4 logical column in data.frame will be chose to draw four-set venn
})
