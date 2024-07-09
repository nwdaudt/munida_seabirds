## 
## Spatial autocorrelation
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script develops variogram plots for each species

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Libraries ####
library(dplyr)
library(tidyr)
library(sf)
library(sp)
library(geoR)

## Read data ####
data <- 
  read.csv("./data-processed/all_data_long.csv")[, -1] %>% 
  dplyr::filter(! year == "2012")

## Format some columns
data$taiaroa_east <- 
  factor(data$taiaroa_east,
         levels = c("TaiaroaEast0.5km", "TaiaroaEast5.10km",
                    "TaiaroaEast10.15km", "TaiaroaEast15.20km",
                    "TaiaroaEast20.25km", "TaiaroaEast25.30km",
                    "TaiaroaEast30.35km", "TaiaroaEast35.40km",
                    "TaiaroaEast40.45km", "TaiaroaEast45.50km",
                    "TaiaroaEast50.55km", "TaiaroaEast55.60km"))

# Create a continuous version of 'taiaroa_east', using the 'centroid' distance
data <- 
  data %>% 
  dplyr::mutate(dist_coast = case_when(
    taiaroa_east == "TaiaroaEast0.5km" ~ 2.5,
    taiaroa_east == "TaiaroaEast5.10km" ~ 7.5,
    taiaroa_east == "TaiaroaEast10.15km" ~ 12.5, 
    taiaroa_east == "TaiaroaEast15.20km" ~ 17.5,
    taiaroa_east == "TaiaroaEast20.25km" ~ 22.5, 
    taiaroa_east == "TaiaroaEast25.30km" ~ 27.5,
    taiaroa_east == "TaiaroaEast30.35km" ~ 32.5, 
    taiaroa_east == "TaiaroaEast35.40km" ~ 37.5,
    taiaroa_east == "TaiaroaEast40.45km" ~ 42.5, 
    taiaroa_east == "TaiaroaEast45.50km" ~ 47.5,
    taiaroa_east == "TaiaroaEast50.55km" ~ 52.5, 
    taiaroa_east == "TaiaroaEast55.60km" ~ 57.5,
    .default = TRUE
  ), .after = taiaroa_east)

data$direction <- 
  factor(data$direction,
         levels = c("eastward", "westward"))

data$season <- 
  factor(data$season,
         levels = c("summer", "autumn", "winter", "spring"))

data$count <- as.numeric(data$count)

## Prep data ####

spp_cols_all <- unique(data$species)
sp_cols_only <- spp_cols_all[! grepl("unknown", spp_cols_all)]

### Transform it from long to wide format
wide_data <- 
  data %>% 
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0) %>% 
  ## To match-up with the Temperature/Salinity data, we will only model the way back ("westward")
  dplyr::filter(direction == "westward")

### Identify 'rare' species (i.e. less than 3 occurrences)
sp_rare_cols <- 
  # Get species names and number of occurrences
  data.frame(
    species = sp_cols_only,
    n_occ = apply(wide_data[sp_cols_only], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL) %>%
  # Filter and pull species names
  dplyr::filter(n_occ < 3) %>%
  dplyr::pull(species)

unknown_and_rare_spp <- c(spp_cols_all[!(spp_cols_all %in% sp_cols_only)], sp_rare_cols)

### Back again to long-format
long_data <- 
  wide_data %>%
  # Remove unknown and rare species columns
  dplyr::select(- all_of(unknown_and_rare_spp)) %>% 
  tidyr::pivot_longer(cols = c(black_backed_gull:yellow_eye_penguin),
                      names_to = "species",
                      values_to = "count")

# Get species names in a vector
spp <- unique(long_data$species)

### Transform 'long_data' into a spatial object

sf_long_data <-
  long_data %>%
  dplyr::mutate(long = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  sf::st_transform("epsg:2193")

# Transform it into an 'sp' object (SpatialPointDataFrame) and re-project it
sp_long_data <- as(sf_long_data, "Spatial")
sp::proj4string(sp_long_data) <- sp::CRS("+init=epsg:2193")
sp_long_data <- sp::spTransform(sp_long_data, CRSobj = sp::CRS("+init=epsg:2193"))

## Variogram {geoR} ####

# spp 

list_spp_variograms <- list()

for (sp in spp) {
  print(sp)

  sp_i <- sp
  
  data <-
    sp_long_data[sp_long_data$species == sp_i, ]
  
  var <- geoR::variog(coords = data@coords, 
                      data = data$count)
  
  list_spp_variograms[[sp_i]] <- var
  
  pdf(file = paste0("./results/variograms/", sp_i, ".pdf"),
      width = 4.5, height = 4.5)
  plot(var, main = paste(sp_i))
  dev.off()
  
  rm("sp", "sp_i", "data", "var")
}

saveRDS(list_spp_variograms, 
        file = "./results/variograms/list_spp_variograms.rds")
# readRDS()

# rm("list_spp_variograms")