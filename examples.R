library(ggvenn)
library(patchwork)

a <- list(A = 1:5, B = 4:9, C = 3:7, D = 1:20, E = 15:19)
g1 <- ggvenn(a, c("A", "B"))            # draw two-set venn
g2 <- ggvenn(a, c("A", "B", "C"))       # draw three-set venn
g3 <- ggvenn(a, c("A", "B", "C", "D"))  # draw four-set venn
g4 <- ggvenn(a)   # without set names, all elements in list will be chose to draw venn

if (!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
}

g <- (g1 | g2) / (g3 | g4)
ggsave(
  "figures/ggvenn_list.png",
  g,
  width = 12, height = 12, units = "in",
  dpi = 300, bg = "white"
)

d <- data.frame(
  id = 1:32,
  A = 1:32 %% 2 == 1,
  B = (1:32 %/% 2) %% 2 == 1,
  C = (1:32 %/% 4) %% 2 == 1,
  D = (1:32 %/% 8) %% 2 == 1,
  E = (1:32 %/% 16) %% 2 == 1
)

g5 <- ggvenn(d, element_column = "id", show_elements = TRUE)

ggsave(
  "figures/ggvenn_data.frame.png",
  g5,
  width = 12, height = 12, units = "in",
  dpi = 300, bg = "white"
)
