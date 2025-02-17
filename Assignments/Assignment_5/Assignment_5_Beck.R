library(palmerpenguins)
library(tidyverse)
library(ggimage)
library(gganimate)
library(wesanderson)
library(GGally)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = island)) +
  geom_point(size = 10, alpha = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, color = "yellow", linetype = "dotted", size = 3) + 
  theme(
    panel.background = element_rect(fill = "pink"),
    plot.background = element_rect(fill = "limegreen"),
    panel.grid.major = element_line(color = "red", linetype = "dashed", size = 1),
    panel.grid.minor = element_line(color = "blue", linetype = "dotted"),
    axis.text = element_text(size = 15, face = "bold", color = "purple"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "orange"),
    legend.text = element_text(size = 14, face = "italic", color = "black") 
  ) +
  labs(
    title = "BODY MASS VS FLIPPER LENGTH",
    subtitle = "Obviously best graph ever",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  )
