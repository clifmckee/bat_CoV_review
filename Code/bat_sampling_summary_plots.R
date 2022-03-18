##########################
# Code for plotting CoV biogeography and sampling patterns
# R version 4.1.2 "Bird Hippie"
# Works: 2022-03-17
##########################

# # Installing ggtree package
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggtree")

# Necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtree)
library(reshape2)
library(stringr)
library(cowplot)
library(here)

# Read in tree
tree <- read.tree("../Data/bat_families.nwk")

# Read in data
sampling_sum <- read.csv("../Data/bat_species_sampling.csv")

fam_sum <- read.csv("../Data/bat_family_summary.csv")

# Melt
m_sampling_sum <-
  melt(sampling_sum, id.vars = c("Bat.family", "Plot.order")) %>%
  arrange(Plot.order)
m_fam_sum <-
  melt(fam_sum, id.vars = c("Bat.family", "Plot.order")) %>%
  arrange(Plot.order)

# Assign order to value
m_fam_sum$value <- factor(m_fam_sum$value, levels = c(4, 3, 2, 1))

# Make heatmap image for family biogeography
fam_heat_plot <-
  ggplot(data = m_fam_sum, aes(
    x = variable,
    y = factor(Plot.order),
    fill = factor(value)
  )) +
  geom_tile(color = "black") +
  scale_fill_manual(
    name = "Legend",
    values = c(hcl.colors(n = 3, palette = "Sunset"), "#FFFFFF"),
    labels = c(
      "Family present, sampled, CoVs detected",
      "Family present, sampled, no CoVs detected",
      "Family present, not sampled",
      "Family not present"
    )
  ) +
  scale_x_discrete(
    name = "Continent",
    labels = c("N. America", "S. America", "Europe", "Africa", "Asia", "Oceania")
  ) +
  scale_y_discrete(name = NULL, labels = unique(m_fam_sum$Bat.family)) +
  theme_cowplot(font_size = 12) +
  theme(
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1
    ),
    legend.position = "top",
    legend.direction = "vertical",
    legend.justification = 0.25,
    plot.background = element_rect("white")
  )

# Make heatmap image for species sampling
species_heat_plot <-
  ggplot(data = m_sampling_sum, aes(x = variable, y = factor(Plot.order))) +
  geom_tile(aes(fill = value), color = "black") +
  geom_text(aes(label = value)) +
  scale_x_discrete(name = "Status") +
  scale_y_discrete(name = NULL,
                   labels = NULL) +
  scale_fill_gradientn(name = "Species", colors = c("white", hcl.colors(
    n = 12, palette = "Sunset", rev = TRUE
  ))) +
  theme_cowplot(font_size = 12) +
  theme(
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1
    ),
    legend.position = "top",
    legend.direction = "vertical",
    legend.justification = 0.25,
    plot.background = element_rect("white")
  )

# Combine images
panelsAB <- plot_grid(
  fam_heat_plot,
  species_heat_plot,
  labels = c("A", "B"),
  label_size = 16,
  ncol = 2,
  rel_widths = c(.55, .45),
  align = "h"
)
plot_grid(
  ggtree(tree) + ylim(-1.9, 26.2),
  panelsAB,
  ncol = 2,
  rel_widths = c(.1, .9)
)

# Save to file
ggsave(
  filename = "../Results/bat_sampling_summary.pdf",
  device = "pdf",
  width = 10,
  height = 7,
  units = "in"
)
ggsave(
  filename = "../Results/bat_sampling_summary.png",
  device = "png",
  width = 10,
  height = 7,
  dpi = 300,
  units = "in"
)
ggsave(
  filename = "../Results/bat_sampling_summary.tiff",
  device = "tiff",
  width = 10,
  height = 7,
  dpi = 300,
  units = "in"
)
