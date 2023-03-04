# ggvenn

Venn Diagram by ggplot2, with really easy-to-use API. This package is inspired by [Venny](http://bioinfogp.cnb.csic.es/tools/venny/index.html)

## Screenshots

<table width="100%"><tr>
<td><img width="300" height="300" src="man/figures/venn-2.svg" alt="Venn 2"></td>
<td><img width="300" height="300" src="man/figures/venn-3.svg" alt="Venn 3"></td>
<td><img width="300" height="300" src="man/figures/venn-4.svg" alt="Venn 4"></td>
</tr></table>

## Installation

```{r}
install.packages("ggvenn") # install via CRAN
```

or

```{r}
if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn") # install via GitHub (for latest version)
```

## Quick Start

This package supports both `list` and `data.frame` type data as input.

For `list` data (each element is a set):

```{r}
library(ggvenn)

a <- list(`Set 1` = c(1, 3, 5, 7, 9),
          `Set 2` = c(1, 5, 9, 13),
          `Set 3` = c(1, 2, 8, 9),
          `Set 4` = c(6, 7, 10, 12))
ggvenn(a, c("Set 1", "Set 2"))            # draw two-set venn
ggvenn(a, c("Set 1", "Set 2", "Set 3"))   # draw three-set venn
ggvenn(a)   # without set names, the first 4 elements in list will be chose to draw four-set venn
```

For `data.frame` data (each logical column is a set):

```{r}
d <- tibble(value   = c(1,     2,     3,     5,     6,     7,     8,     9),
            `Set 1` = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE),
            `Set 2` = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE),
            `Set 3` = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE),
            `Set 4` = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE))
ggvenn(d, c("Set 1", "Set 2"))           # draw two-set venn
ggvenn(d, c("Set 1", "Set 2", "Set 3"))  # draw three-set venn
ggvenn(d)   # without set names, the first 4 logical column in data.frame will be chose to draw four-set venn
```

For `data.frame` data, there is also another way to plot in ggplot grammar:

```{r}
# draw two-set venn (use A, B in aes)
ggplot(d, aes(A = `Set 1`, B = `Set 2`)) +
  geom_venn() + theme_void() + coord_fixed()

# draw three-set venn (use A, B, C in aes)
ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`)) +
  geom_venn() + theme_void() + coord_fixed()

# draw four-set venn (use A, B, C, D in aes)
ggplot(d, aes(A = `Set 1`, B = `Set 2`, C = `Set 3`, D = `Set 4`)) +
  geom_venn() + theme_void() + coord_fixed()
```

## More Options

There are more options for customizing the venn diagram.

1. Tune the color and size

    For filling:
    
    * `fill_color` - default is c("blue", "yellow", "green", "red")
    * `fill_alpha` - default is 0.5
    
    For stroke:
    
    * `stroke_color` - default is "black"
    * `stroke_alpha` - default is 1
    * `stroke_size` - default is 1
    * `stroke_linetype` - default is "solid"

    For set name:
    
    * `set_name_color` - default is "black"
    * `set_name_size` - default is 6

    For text:
    
    * `text_color` - default is "black"
    * `text_size` - default is 4

    All parameters above could be used in both `ggvenn()` and `geom_venn()`.
    
    For example:
    
    ```{r}
    a <- list(A = 1:4, B = c(1,3,5))
    ggvenn(a, stroke_linetype = 2, stroke_size = 0.5,
      set_name_color = "red", set_name_size = 15,
      fill_color = c("pink", "gold"))
    ```

2. Show elements

    * `show_elements` - default is FALSE
    * `label_sep` - text used to concatenate elements, default is ","
    
    For example:
    
    ```{r}
    a <- list(A = c("apple", "pear", "peach"),
              B = c("apple", "lemon"))
    ggvenn(a, show_elements = TRUE)
    
    ggvenn(a, show_elements = TRUE, label_sep = "\n")  # show elements in line
    ```

3. Hide percentage

    * `show_percentage` - default is TRUE

    For example:
    
    ```{r}
    a <- list(A = 1:5, B = 1:2)
    ggvenn(a, show_percentage = FALSE)
    ```

4. Change digits of percentage

    * `digits` - default is 1

    For example:
    
    ```{r}
    a <- list(A = 1:5, B = 1:2)
    ggvenn(a, digits = 2)
    ```

## Data Format

The `ggvenn` support two types of input data: list and data.frame. Two functions (`data_frame_to_list()` and `list_to_data_frame()`) can convert data between the two types.

```r
a <- list(A = 1:5, B = 4:6)
d <- tibble(key = 1:6,
            A = c(rep(TRUE, 5), FALSE),
            B = rep(c(FALSE, TRUE), each = 3))

identical(a, data_frame_to_list(d))  # TRUE
identical(d, list_to_data_frame(a))  # TRUE
```
