test_that("geom_venn", {
  
  d <- tibble(value   = c(1,     2,     3,     4,     5,     6,     7,     8),
              `Set 1` = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE),
              `Set 2` = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE),
              `Set 3` = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE),
              `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE))
  
  # draw two-set venn (use A, B in aes)
  expect_snapshot(
    normalize_ggplot_build(
      ggplot(d, aes(A = `Set 1`, B = `Set 2`)) +
      geom_venn() + theme_void() + coord_fixed()
    )
  )
  
  # draw three-set venn (use A, B, C in aes)
  expect_snapshot(
    normalize_ggplot_build(
      ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`)) +
      geom_venn() + theme_void() + coord_fixed()
    )
  )
  
  # draw four-set venn (use A, B, C, D in aes)
  expect_snapshot(
    normalize_ggplot_build(
      ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`)) +
      geom_venn() + theme_void() + coord_fixed()
    )
  )
})
