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
      [1] -1.866163 -1.866163 -1.866163 -1.866163 -1.866163 -1.866163 -1.866163
      [8] -1.866163
      
      [[1]]$xmax
      [1] 1.866667 1.866667 1.866667 1.866667 1.866667 1.866667 1.866667 1.866667
      
      [[1]]$ymin
      [1] -1.4 -1.4 -1.4 -1.4 -1.4 -1.4 -1.4 -1.4
      
      [[1]]$ymax
      [1] 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4
      
      

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
      [1] -1.866163 -1.866163 -1.866163 -1.866163 -1.866163 -1.866163 -1.866163
      [8] -1.866163
      
      [[1]]$xmax
      [1] 1.866667 1.866667 1.866667 1.866667 1.866667 1.866667 1.866667 1.866667
      
      [[1]]$ymin
      [1] -2 -2 -2 -2 -2 -2 -2 -2
      
      [[1]]$ymax
      [1] 2 2 2 2 2 2 2 2
      
      

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
      [1] -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847 -2.085847
      [8] -2.085847
      
      [[1]]$xmax
      [1] 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382 2.085382
      
      [[1]]$ymin
      [1] -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847 -1.885847
      [8] -1.885847
      
      [[1]]$ymax
      [1] 1.4 1.4 1.4 1.4 1.4 1.4 1.4 1.4
      
      

