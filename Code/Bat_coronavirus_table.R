##########################
# Code for merging bat species with coronaviruses
# R version 4.1.2 "Bird Hippie"
# Works: 2022-03-17
##########################

# Necessary packages
library(dplyr)
library(tidyr)
library(here)

# Read in coronavirus and bat species data
all_CoV_spp <-
  read.csv("../Data/bat_coronavirus_review - Host species.csv",
           stringsAsFactors = FALSE)
key_CoV_spp <-
  read.csv("../Data/bat_coronavirus_review - Host species, key subgenera.csv",
           stringsAsFactors = FALSE)

# Merge tables
merged_CoV_spp <- full_join(
  x = all_CoV_spp,
  y = key_CoV_spp,
  by = c(
    "Bat.species",
    "Bat.family",
    "Continent",
    "Country",
    "Reference"
  )
) %>%
  arrange(Bat.family, Bat.species)

# Rearrange columns
merged_CoV_spp <- merged_CoV_spp[, c(1, 2, 6, 3, 4, 5)]

# Output to file
write.csv(merged_CoV_spp,
          "../Data/bat_coronavirus_review_MERGED.csv",
          row.names = FALSE)
