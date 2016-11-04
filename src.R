library(tidyverse)
library(forestr)

d1 <- data_frame(
  x = xcoord(1:25, xspacing = 2, ncol = 5, start = "left-bottom"),
  y = ycoord(1:25, yspacing = 2, ncol = 5, start = "left-bottom"),
  z = available_tree(x, y, 2)
)

d2 <- data_frame(x = 4, y = 4)

ggplot() +
  geom_point(data = d1, aes(x, y, color = z), size = 5) +
  geom_point(data = d2, aes(x, y, color = "Objeto"), size = 5) +
  geom_spoke(data = d2, aes(x, y, angle = 45, radius = 3), linetype = "dashed") +
  annotate(
    "path",
    x = 4+3*cos(seq(0,2*pi, length.out = 500)),
    y = 4+3*sin(seq(0,2*pi, length.out = 500))
  ) +
  annotate("text", x = 5.2, y = 5, label = "2,5 m") +
  labs(color = "Árvore", x = "Coordenada local x", y = "Coordenada local y") +
  scale_colour_manual(
    values = c("FALSE" = "#d95f02", "Objeto" = "#7570b3", "TRUE" = "#1b9e77"),
    labels = c("Não competidora", "Objeto", "Competidora")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")