# geom_venn

    Code
      ggplot_build(ggplot(d, aes(A = `Set 1`, B = `Set 2`)) + geom_venn() +
        theme_void() + coord_fixed())$data
    Output
      [[1]]
            A     B PANEL group xmin xmax ymin ymax
      1  TRUE  TRUE     1     3   -2    2   -2    2
      2 FALSE FALSE     1     1   -2    2   -2    2
      3  TRUE FALSE     1     2   -2    2   -2    2
      4  TRUE  TRUE     1     3   -2    2   -2    2
      5 FALSE FALSE     1     1   -2    2   -2    2
      6  TRUE FALSE     1     2   -2    2   -2    2
      7 FALSE FALSE     1     1   -2    2   -2    2
      8  TRUE  TRUE     1     3   -2    2   -2    2
      

---

    Code
      ggplot_build(ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`)) + geom_venn() +
        theme_void() + coord_fixed())$data
    Output
      [[1]]
            A     B     C PANEL group xmin xmax ymin ymax
      1  TRUE  TRUE  TRUE     1     5   -2    2   -2    2
      2 FALSE FALSE  TRUE     1     2   -2    2   -2    2
      3  TRUE FALSE FALSE     1     3   -2    2   -2    2
      4  TRUE  TRUE FALSE     1     4   -2    2   -2    2
      5 FALSE FALSE FALSE     1     1   -2    2   -2    2
      6  TRUE FALSE FALSE     1     3   -2    2   -2    2
      7 FALSE FALSE  TRUE     1     2   -2    2   -2    2
      8  TRUE  TRUE  TRUE     1     5   -2    2   -2    2
      

---

    Code
      ggplot_build(ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`)) +
        geom_venn() + theme_void() + coord_fixed())$data
    Output
      [[1]]
            A     B     C     D PANEL group xmin xmax ymin ymax
      1  TRUE  TRUE  TRUE FALSE     1     6   -2    2   -2    2
      2 FALSE FALSE  TRUE FALSE     1     2   -2    2   -2    2
      3  TRUE FALSE FALSE FALSE     1     3   -2    2   -2    2
      4  TRUE  TRUE FALSE FALSE     1     5   -2    2   -2    2
      5 FALSE FALSE FALSE  TRUE     1     1   -2    2   -2    2
      6  TRUE FALSE FALSE  TRUE     1     4   -2    2   -2    2
      7 FALSE FALSE  TRUE FALSE     1     2   -2    2   -2    2
      8  TRUE  TRUE  TRUE FALSE     1     6   -2    2   -2    2
      

