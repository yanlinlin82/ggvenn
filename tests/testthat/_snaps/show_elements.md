# show_elements

    Code
      normalize_ggvenn_output(ggvenn(a, show_elements = TRUE))
    Output
      [1] "A" "B"
    Condition
      Warning:
      `aes_string()` was deprecated in ggplot2 3.0.0.
      i Please use tidy evaluation idioms with `aes()`.
      i See also `vignette("ggplot2-in-packages")` for more information.
    Output
      [[1]]
      [[1]]$A
      [1]  TRUE  TRUE  TRUE FALSE
      
      [[1]]$B
      [1]  TRUE FALSE FALSE  TRUE
      
      [[1]]$PANEL
      [1] 1 1 1 1
      Levels: 1
      
      [[1]]$group
      [1] 3 2 2 1
      attr(,"n")
      [1] 3
      
      [[1]]$xmin
      [1] -2 -2 -2 -2
      
      [[1]]$xmax
      [1] 2 2 2 2
      
      [[1]]$ymin
      [1] -2 -2 -2 -2
      
      [[1]]$ymax
      [1] 2 2 2 2
      
      

---

    Code
      normalize_ggvenn_output(ggvenn(a, show_elements = TRUE, label_sep = "\n"))
    Output
      [1] "A" "B"
    Condition
      Warning:
      `aes_string()` was deprecated in ggplot2 3.0.0.
      i Please use tidy evaluation idioms with `aes()`.
      i See also `vignette("ggplot2-in-packages")` for more information.
    Output
      [[1]]
      [[1]]$A
      [1]  TRUE  TRUE  TRUE FALSE
      
      [[1]]$B
      [1]  TRUE FALSE FALSE  TRUE
      
      [[1]]$PANEL
      [1] 1 1 1 1
      Levels: 1
      
      [[1]]$group
      [1] 3 2 2 1
      attr(,"n")
      [1] 3
      
      [[1]]$xmin
      [1] -2 -2 -2 -2
      
      [[1]]$xmax
      [1] 2 2 2 2
      
      [[1]]$ymin
      [1] -2 -2 -2 -2
      
      [[1]]$ymax
      [1] 2 2 2 2
      
      

