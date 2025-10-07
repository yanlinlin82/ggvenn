# geom_venn

    Code
      normalize_ggplot_build(ggplot(d, aes(A = `Set 1`, B = `Set 2`)) + geom_venn() +
        theme_void() + coord_fixed())
    Output
      [[1]]
      [[1]]$A
      [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
      
      [[1]]$B
      [1]  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
      
      [[1]]$PANEL
      [1] 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
      [1] 3 1 2 3 1 2 1 3
      attr(,"n")
      [1] 3
      
      [[1]]$xmin
      [1] -2.666163 -2.666163 -2.666163 -2.666163 -2.666163 -2.666163 -2.666163
      [8] -2.666163
      
      [[1]]$xmax
      [1] 2.666667 2.666667 2.666667 2.666667 2.666667 2.666667 2.666667 2.666667
      
      [[1]]$ymin
      [1] -2.2 -2.2 -2.2 -2.2 -2.2 -2.2 -2.2 -2.2
      
      [[1]]$ymax
      [1] 2.2 2.2 2.2 2.2 2.2 2.2 2.2 2.2
      
      

---

    Code
      normalize_ggplot_build(ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`)) +
        geom_venn() + theme_void() + coord_fixed())
    Output
      [[1]]
      [[1]]$A
      [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
      
      [[1]]$B
      [1]  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
      
      [[1]]$C
      [1]  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
      
      [[1]]$PANEL
      [1] 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
      [1] 5 2 3 4 1 3 2 5
      attr(,"n")
      [1] 5
      
      [[1]]$xmin
      [1] -2.666163 -2.666163 -2.666163 -2.666163 -2.666163 -2.666163 -2.666163
      [8] -2.666163
      
      [[1]]$xmax
      [1] 2.666667 2.666667 2.666667 2.666667 2.666667 2.666667 2.666667 2.666667
      
      [[1]]$ymin
      [1] -2.8 -2.8 -2.8 -2.8 -2.8 -2.8 -2.8 -2.8
      
      [[1]]$ymax
      [1] 2.8 2.8 2.8 2.8 2.8 2.8 2.8 2.8
      
      

---

    Code
      normalize_ggplot_build(ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`)) +
        geom_venn() + theme_void() + coord_fixed())
    Output
      [[1]]
      [[1]]$A
      [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
      
      [[1]]$B
      [1]  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
      
      [[1]]$C
      [1]  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
      
      [[1]]$D
      [1] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE
      
      [[1]]$PANEL
      [1] 1 1 1 1 1 1 1 1
      Levels: 1
      
      [[1]]$group
      [1] 6 2 3 5 1 4 2 6
      attr(,"n")
      [1] 6
      
      [[1]]$xmin
      [1] -2.885847 -2.885847 -2.885847 -2.885847 -2.885847 -2.885847 -2.885847
      [8] -2.885847
      
      [[1]]$xmax
      [1] 2.885382 2.885382 2.885382 2.885382 2.885382 2.885382 2.885382 2.885382
      
      [[1]]$ymin
      [1] -2.685847 -2.685847 -2.685847 -2.685847 -2.685847 -2.685847 -2.685847
      [8] -2.685847
      
      [[1]]$ymax
      [1] 2.2 2.2 2.2 2.2 2.2 2.2 2.2 2.2
      
      

