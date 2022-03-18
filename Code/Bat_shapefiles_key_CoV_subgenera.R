##########################
# Code for determining which coronavirus host species distributions overlap
# R version 4.1.2 "Bird Hippie"
# Works: 2022-03-17
##########################

# Read in coronavirus and bat species data
CoV_spp <-
  read.csv("../Data/bat_coronavirus_review - Host species, key subgenera.csv",
           stringsAsFactors = FALSE)
all_subgenera <-
  c(
    "Duvinacovirus",
    "Hibecovirus",
    "Merbecovirus",
    "Nobecovirus",
    "Rhinacovirus",
    "Sarbecovirus",
    "Setracovirus"
  )

# Sort data
CoV_spp_sorted <- CoV_spp %>%
  arrange(Coronavirus.subgenus, Bat.family, Bat.species)
write.csv(CoV_spp_sorted, "../Data/key_subgenera_CoV_spp_sorted.csv")

# Replace invalid bat species names
CoV_spp$Bat.species[CoV_spp$Bat.species == "Hipposideros commersoni"] <-
  "Macronycteris commersoni"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Hipposideros gigas"] <-
  "Macronycteris gigas"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Hipposideros vittatus"] <-
  "Macronycteris vittatus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Myonycteris angolensis"] <-
  "Lissonycteris angolensis"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Myotis ricketti"] <-
  "Myotis pilosus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pipistrellus deserti"] <-
  "Pipistrellus kuhlii"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pteropus medius"] <-
  "Pteropus giganteus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pipistrellus minus"] <-
  "Pipistrellus tenuis"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Vespertilio superans"] <-
  "Vespertilio sinensis"

# Summarize all data
CoV_spp %>%
  filter(!str_detect(Bat.species, coll("cf.")),
         !str_detect(Bat.species, coll("sp.")),
         !str_detect(Bat.species, coll("spp.")),
         !str_detect(Bat.species, coll("idae")),
         !str_detect(Bat.species, coll("Murina sp")),
         !str_detect(Bat.species, coll("Myotis sp")),
         Bat.species != "Chiroptera (unspecified species)") %>%
  summarize(
    unique.species = n_distinct(Bat.species),
    unique.families = n_distinct(Bat.family),
    unique.refs = n_distinct(Reference)
  )
# Summarize all data by family
CoV_spp %>%
  filter(!str_detect(Bat.species, coll("cf.")),
         !str_detect(Bat.species, coll("sp.")),
         !str_detect(Bat.species, coll("spp.")),
         !str_detect(Bat.species, coll("idae")),
         !str_detect(Bat.species, coll("Murina sp")),
         !str_detect(Bat.species, coll("Myotis sp")),
         Bat.species != "Chiroptera (unspecified species)") %>%
  group_by(Bat.family) %>%
  summarize(unique.species = n_distinct(Bat.species))
# Summarize data, separated by coronavirus subgenera
CoV_spp %>%
  filter(!str_detect(Bat.species, coll("cf.")),
         !str_detect(Bat.species, coll("sp.")),
         !str_detect(Bat.species, coll("spp.")),
         !str_detect(Bat.species, coll("idae")),
         !str_detect(Bat.species, coll("Murina sp")),
         !str_detect(Bat.species, coll("Myotis sp")),
         Bat.species != "Chiroptera (unspecified species)") %>%
  group_by(Coronavirus.subgenus) %>%
  summarize(
    unique.species = n_distinct(Bat.species),
    unique.families = n_distinct(Bat.family),
    unique.refs = n_distinct(Reference)
  )

# Find bat species names that do not match IUCN records
CoV_spp_missing <- CoV_spp %>%
  mutate(IUCN = Bat.species %in% mammterr$binomial) %>%
  filter(IUCN == "FALSE")

subgenus_map <- function(subgenera, size) {
  # Filter out bad species and choose coronavirus subgenera
  # "Duvinacovirus", "Merbecovirus", "Nobecovirus", "Rhinacovirus", "Sarbecovirus", "Setracovirus"
  unique_CoV_spp <- CoV_spp %>%
    filter(Bat.species %ni% CoV_spp_missing$Bat.species,
           Coronavirus.subgenus %in% subgenera) %>%
    distinct(Bat.species) %>%
    arrange(Bat.species)
  
  # Create a list of species names to filter mammterr
  allnames = as.character(unique_CoV_spp$Bat.species)
  
  # Filter mammterr down to just binomial names
  all.binomial = mammterr$binomial
  
  # Check to see if allnames are in the binomial names
  unique.allnames <- unique(allnames)
  
  # Which rows of the data are the species I care about
  keep = list()
  for (i in 1:length(unique.allnames)) {
    keep[[i]] = which(all.binomial == unique.allnames[i])
  }
  x = keep[[1]]
  for (i in 2:length(unique.allnames)) {
    x = c(x, keep[[i]])
  }
  keep.species = data.frame(x = x, species = mammterr[x,]$binomial)
  myspecies.distr <- mammterr[x,]
  
  # Sample points across the species distributions on a regular grid
  spp.points <-
    spsample(myspecies.distr, type = "regular", n = size)
  
  # Summarize the species distributions that overlap the chosen points
  spp.pointsInPolygons <-
    sp::over(x = spp.points,
             y = myspecies.distr,
             returnList = TRUE)
  
  # Count the number of intersections
  spp.counting <-
    lapply(
      spp.pointsInPolygons,
      FUN = function(x)
        nrow(x)
    )
  spp.over.df <-
    data.frame(
      "point" = rownames(t(do.call(
        "cbind", spp.counting
      ))),
      "count" = t(do.call("cbind", spp.counting)),
      "polygon" = paste(spp.pointsInPolygons)
    )
  
  # Summarize counts in a data frame
  spp.points.df <- as.data.frame(spp.points)
  spp.points.df$count <- spp.over.df$count
  
  # Data for world map
  world_map <- map_data("world")
  
  # Combine world map data and species range data into one object
  sf_data <- list(world = world_map, overlap = spp.points.df)
  
  # Plot map of species range overlap
  out <- ggplot() +
    geom_polygon(
      data = sf_data$world,
      aes(x = long, y = lat, group = group),
      color = NA,
      fill = "grey"
    ) +
    geom_tile(data = sf_data$overlap, aes(x = x1, y = x2, fill = count)) +
    ylim(-56, 84) +
    scale_fill_viridis(option = "D",
                       name = "Species",
                       breaks = c(1, ceiling(
                         max(sf_data$overlap$count) * c(0.25, 0.5, 0.75, 1)
                       ))) +
    theme_map(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  return(out)
}

# Plot map of species range overlap for key coronavirus subgenera hosts
key <- subgenus_map(subgenera = all_subgenera, size = 1000000)
# key <- subgenus_map(subgenera = all_subgenera, size = 1000)

# Plot separate maps of species range overlap for key coronavirus subgenera hosts
trans_list <- list()
size_list <-
  c(100000, 100000, 1000000, 1000000, 1000000, 1000000, 100000)
# size_list <-
#   c(1000, 1000, 1000, 1000, 1000, 1000, 1000)
for (i in 1:length(all_subgenera)) {
  trans_list[[i]] <-
    subgenus_map(subgenera = all_subgenera[i], size = size_list[i])
}
plotD <-
  plot_grid(
    key +
      ggtitle("Key coronavirus subgenera hosts") +
      theme(
        plot.title = element_text(size = 24,
                                  face = 2),
        plot.background = element_rect(fill = "white", color = NA)
      ),
    # All
    trans_list[[1]] +
      ggtitle(paste(all_subgenera[1], "hosts",  "(alpha)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Duvina
    trans_list[[2]] +
      ggtitle(paste(all_subgenera[2], "hosts",  "(beta)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Hibe
    trans_list[[3]] +
      ggtitle(paste(all_subgenera[3], "hosts",  "(beta)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Merbe
    trans_list[[4]] +
      ggtitle(paste(all_subgenera[4], "hosts",  "(beta)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Nobe
    trans_list[[5]] +
      ggtitle(paste(all_subgenera[5], "hosts",  "(alpha)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Rhina
    trans_list[[6]] +
      ggtitle(paste(all_subgenera[6], "hosts",  "(beta)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Sarbe
    trans_list[[7]] +
      ggtitle(paste(all_subgenera[7], "hosts",  "(alpha)")) +
      theme(plot.title = element_text(size = 24, face = 2)),
    # Setra
    nrow = 4,
    ncol = 2
  )

###################
### End of code ###
###################
