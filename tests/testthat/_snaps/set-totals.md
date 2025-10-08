# stats

    Code
      normalize_ggvenn_output(ggvenn(a, show_percentage = FALSE, show_elements = FALSE,
        text_size = 6))
    Output
      [[1]]
      [[1]]$A
       [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$B
       [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$C
       [1]  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE
      
      [[1]]$D
       [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
      
      [[1]]$label
       [1]  1  3  5  7  9 13  2  8  6 10 12
      
      [[1]]$PANEL
       [1] 1 1 1 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
       [1] 7 4 6 5 7 3 2 2 1 1 1
      attr(,"n")
      [1] 7
      
      [[1]]$xmin
       [1] -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847
       [8] -2.085847 -2.085847 -2.085847 -2.085847
      
      [[1]]$xmax
       [1] 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382
       [9] 2.085382 2.085382 2.085382
      
      [[1]]$ymin
       [1] -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847
       [8] -1.885847 -1.885847 -1.885847 -1.885847
      
      [[1]]$ymax
       [1] 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4
      
      

---

    Code
      normalize_ggvenn_output(ggvenn(a, show_set_totals = "c", show_elements = FALSE,
        text_size = 6))
    Output
      [[1]]
      [[1]]$A
       [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$B
       [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$C
       [1]  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE
      
      [[1]]$D
       [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
      
      [[1]]$label
       [1]  1  3  5  7  9 13  2  8  6 10 12
      
      [[1]]$PANEL
       [1] 1 1 1 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
       [1] 7 4 6 5 7 3 2 2 1 1 1
      attr(,"n")
      [1] 7
      
      [[1]]$xmin
       [1] -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847
       [8] -2.085847 -2.085847 -2.085847 -2.085847
      
      [[1]]$xmax
       [1] 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382
       [9] 2.085382 2.085382 2.085382
      
      [[1]]$ymin
       [1] -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847
       [8] -1.885847 -1.885847 -1.885847 -1.885847
      
      [[1]]$ymax
       [1] 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4
      
      

---

    Code
      normalize_ggvenn_output(ggvenn(a, show_set_totals = "p", show_elements = FALSE,
        text_size = 6))
    Output
      [[1]]
      [[1]]$A
       [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$B
       [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$C
       [1]  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE
      
      [[1]]$D
       [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
      
      [[1]]$label
       [1]  1  3  5  7  9 13  2  8  6 10 12
      
      [[1]]$PANEL
       [1] 1 1 1 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
       [1] 7 4 6 5 7 3 2 2 1 1 1
      attr(,"n")
      [1] 7
      
      [[1]]$xmin
       [1] -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847
       [8] -2.085847 -2.085847 -2.085847 -2.085847
      
      [[1]]$xmax
       [1] 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382
       [9] 2.085382 2.085382 2.085382
      
      [[1]]$ymin
       [1] -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847
       [8] -1.885847 -1.885847 -1.885847 -1.885847
      
      [[1]]$ymax
       [1] 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4
      
      

---

    Code
      normalize_ggvenn_output(ggvenn(a, show_set_totals = "cp", show_elements = FALSE,
        text_size = 6))
    Output
      [[1]]
      [[1]]$A
       [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$B
       [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
      
      [[1]]$C
       [1]  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE
      
      [[1]]$D
       [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
      
      [[1]]$label
       [1]  1  3  5  7  9 13  2  8  6 10 12
      
      [[1]]$PANEL
       [1] 1 1 1 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
       [1] 7 4 6 5 7 3 2 2 1 1 1
      attr(,"n")
      [1] 7
      
      [[1]]$xmin
       [1] -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847
       [8] -2.085847 -2.085847 -2.085847 -2.085847
      
      [[1]]$xmax
       [1] 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382
       [9] 2.085382 2.085382 2.085382
      
      [[1]]$ymin
       [1] -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847
       [8] -1.885847 -1.885847 -1.885847 -1.885847
      
      [[1]]$ymax
       [1] 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4
      
      

