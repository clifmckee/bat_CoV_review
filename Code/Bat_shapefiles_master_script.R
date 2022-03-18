##########################
# Code for determining which coronavirus host species distributions overlap
# R version 4.1.2 "Bird Hippie"
# Works: 2022-03-17
##########################

###################################
### Load packages and functions ###
###################################

# Necessary packages
library(sp)
library(rgeos)
library(maps)
library(mapdata)
library(maptools)
library(geosphere)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggthemes)
library(reshape2)
library(RColorBrewer)
library(rgdal)
library(dplyr)
library(tidyr)
library(stringr)
library(mapproj)
library(broom)
library(raster)
library(rmapshaper)
library(here)

# Define functions
"%ni%" <- Negate("%in%")

###################
### Import data ###
###################

# Read in the shape file
# Available from IUCN at http://www.iucnredlist.org/technical-documents/spatial-data/
# readShapeSpatial() is in the "sp" package
# Shape files for the distributions of every terrestrial mammal in the IUCN database
# mammterr <- readOGR(dsn="./Data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
load(file = "../Data/MAMMALS_TERRESTRIAL_ONLY/mammterr.RData")

###################
### Run scripts ###
###################

# Make map for all bat species
source("../Code/Bat_shapefiles_bat_species.R")

# Make map for sampled bat species
source("../Code/Bat_shapefiles_sampled_species.R")

# Make map for all CoV hosts
source("../Code/Bat_shapefiles_all_CoV.R")

# Make map for key CoV subgenera hosts
source("../Code/Bat_shapefiles_key_CoV_subgenera.R")

# Combine plots
plotABC <- plot_grid(plotA, plotB, plotC, nrow = 3)
plotABCD <- plot_grid(plotABC, plotD, ncol = 2)
# Save to file
ggsave(
  filename = "../Results/map_overlap.pdf",
  plot = plotABCD,
  device = "pdf",
  width = 24,
  height = 12,
  units = "in"
)
ggsave(
  filename = "../Results/map_overlap.png",
  plot = plotABCD,
  device = "png",
  dpi = 300,
  width = 24,
  height = 12,
  units = "in"
)
ggsave(
  filename = "../Results/map_overlap.tiff",
  plot = plotABCD,
  device = "tiff",
  dpi = 300,
  width = 24,
  height = 12,
  units = "in"
)

###################
### End of code ###
###################
