##########################
# Code for mapping overlap between bat species
# R version 4.1.1 "Kick Things"
# Works: 2021-10-08
##########################

# Read in coronavirus and bat species data
CoV_spp <-
  read.csv("./Data/bat_coronavirus_review - Sampled species.csv",
           stringsAsFactors = FALSE)

# Sort data
CoV_spp_sorted <- CoV_spp %>%
  arrange(Bat.family, Bat.species)
write.csv(CoV_spp_sorted, "./Data/sampled_spp_sorted.csv")

# Replace invalid bat species names
CoV_spp$Bat.species[CoV_spp$Bat.species == "Artibeus phaeotis"] <-
  "Dermanura phaeotis"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Artibeus watsoni"] <-
  "Dermanura watsoni"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Arielulus aureocollaris"] <-
  "Thainycteris aureocollaris"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Chaerephon leucogaster"] <-
  "Mops leucogaster"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Hipposideros commersoni"] <-
  "Macronycteris commersoni"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Hipposideros gigas"] <-
  "Macronycteris gigas"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Hipposideros vittatus"] <-
  "Macronycteris vittatus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Megaderma lyra"] <-
  "Lyroderma lyra"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Megaglossus torquata"] <-
  "Myonycteris torquata"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Mimon crenulatum"] <-
  "Gardnerycteris crenulatum"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Myonycteris angolensis"] <-
  "Lissonycteris angolensis"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Molossus major"] <-
  "Eumops auripendulus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Mormopterus beccarii"] <-
  "Ozimops beccarii"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Mormopterus norfolkensis"] <-
  "Micronomus norfolkensis"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Myotis formosus chofukusei"] <-
  "Myotis rufoniger"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Myotis oxygnathus"] <-
  "Myotis blythii"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Myotis ricketti"] <-
  "Myotis pilosus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Natalus lanatus"] <-
  "Natalus mexicanus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Nyctalus velutinus"] <-
  "Nyctalus plancyi"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pipistrellus deserti"] <-
  "Pipistrellus kuhlii"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pipistrellus minus"] <-
  "Pipistrellus tenuis"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pipistrellus taiwanesis"] <-
  "Myotis fimbriatus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Pteropus medius"] <-
  "Pteropus giganteus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Rhinolophus blythi"] <-
  "Rhinolophus pusillus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Rhinopoma musculatum"] <-
  "Rhinopoma microphyllum"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Scotorepens rueppellii"] <-
  "Scoteanax rueppellii"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Scotoecus albigula"] <-
  "Scotoecus hirundo"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Stenonycteris lanosus"] <-
  "Rousettus lanosus"
CoV_spp$Bat.species[CoV_spp$Bat.species == "Vampyressa bidens"] <-
  "Vampyriscus bidens"
CoV_spp$Bat.species[CoV_spp$Bat.species %in% c("Pipistrellus montanus", "Vespertilio superans")] <-
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

# Find bat species names that do not match IUCN records
CoV_spp_missing <- CoV_spp %>%
  mutate(IUCN = Bat.species %in% mammterr$binomial) %>%
  filter(IUCN == "FALSE")

# Filter out bad species and choose coronavirus subgenera
unique_CoV_spp <- CoV_spp %>%
  filter(Bat.species %ni% CoV_spp_missing$Bat.species) %>%
  distinct(Bat.species) %>%
  arrange(Bat.species)

# Create a list of species names to filter mammterr
allnames <- as.character(unique_CoV_spp$Bat.species)

# Filter mammterr down to just binomial names
all.binomial <- mammterr$binomial

# Check to see if allnames are in the binomial names
unique.allnames <- unique(allnames)

# Which rows of the data are the species I care about
keep <- list()
for (i in 1:length(unique.allnames)) {
  keep[[i]] <- which(all.binomial == unique.allnames[i])
}
x <- keep[[1]]
for (i in 2:length(unique.allnames)) {
  x <- c(x, keep[[i]])
}
keep.species <- data.frame(x = x, species = mammterr[x, ]$binomial)
myspecies.distr <- mammterr[x, ]

##################################
### Species distribution plots ###
##################################

# Sample points across the species distributions on a regular grid
spp.points <-
  spsample(myspecies.distr, n = 1000000, type = "regular")
# spp.points <-
#   spsample(myspecies.distr, n = 1000, type = "regular")

# Summarize the species distributions that overlap the chosen points
spp.pointsInPolygons <-
  sp::over(x = spp.points, y = myspecies.distr, returnList = TRUE)

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

# Plot map of species range overlap for all coronavirus hosts
plotB <- ggplot() +
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
  ggtitle("Sampled species") +
  theme(
    plot.title = element_text(size = 24, face = 2),
    plot.background = element_rect(fill = "white", color = NA)
  )

###################
### End of code ###
###################
