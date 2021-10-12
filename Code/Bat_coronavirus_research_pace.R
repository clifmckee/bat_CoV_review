##########################
# Code for assessing the pace of coronavirus publications
# R version 4.1.1 "Kick Things"
# Works: 2021-10-08
##########################

# Necessary packages
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(cowplot)

# Set working directory
setwd("~/Dropbox/JHSPH postdoc/Projects/Bat coronavirus ecology review paper")

# Not in
"%ni%" <- Negate("%in%")

# Read in data on studies
studies <- read_excel("./Data/bat_coronavirus_review.xlsx", sheet = 1, trim_ws = TRUE)
hosts <- read_excel("./Data/bat_coronavirus_review.xlsx", sheet = 3, trim_ws = TRUE)

# Filter to studies that produced sequences or attempted to produce sequences but had zero positives
studies_filter <- studies %>%
  filter(Link %in% hosts$Link | `Proportion positive` %in% c("0", "0.0"))

# Summarize data
studies_summary <- studies_filter %>%
  group_by(Year) %>%
  summarize(Count = n()) %>%
  mutate(Cumsum = cumsum(Count)/10)

# Plot publications over time
ggplot() +
  geom_col(data = studies_summary, aes(x = Year, y = Count), fill = "black") +
  geom_line(data = studies_summary, aes(x = Year, y = Cumsum), color = "#D55E00") +
  scale_x_continuous(breaks = seq(2005, 2021, 1)) +
  scale_y_continuous(
    name = "Publications",
    sec.axis = sec_axis( trans=~.*10, name="Cumulative publications",)
  ) +
  theme_cowplot() +
  theme(axis.title.y = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "#D55E00"),
        axis.text.y.right = element_text(color = "#D55E00"),
        axis.text.x = element_text(angle = 45, hjust = 1))
# Save to file
ggsave("./Results/research_pace.png", bg = "white", width = 6, height = 4, units = "in", dpi = 300)
