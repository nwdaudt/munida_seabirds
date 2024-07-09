


## Libraries ####
library(dplyr)
library(sp)
library(sf)
library(lwgeom)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(RColorBrewer)
# library(mapview) # only used for visually checking the spatial process

## Read spatial data ####

# Read NZ polygon
nz_polygon <- sf::read_sf("./data-raw/spatial/nz-coastlines-and-islands-polygons-topo-150k.gpkg")

# Read NZ isobaths
nz_isobaths <- sf::read_sf("./data-raw/spatial/nz_isobath_otago.gpkg")

## Create the Munida transect spatial feature ####

## First, need to create a couple of functions to help out

# https://gis.stackexchange.com/questions/312289/r-create-multiple-linestrings-from-multiple-coordinates
# Spacedman's function to create line segments from a matrix/df 
st_segment <- function(r){sf::st_linestring(t(matrix(unlist(r), 2, 2)))}

# Split transects (linestrings) into segments
split_transects <- function(df, n, dist, crs) {
  
  require(sf)
  require(lwgeom)
  require(dplyr)
  
  df_splited <- data.frame()
  
  y <- 1 / (n)
  pts <- seq(from = y, to = 1, by = y)
  
  for (i in 1:nrow(df)) {
    
    x <- df[i,]
    
    x <- sf::st_transform(x, crs = crs)
    
    blade_pts <- sf::st_buffer(
      sf::st_line_sample(x, sample = pts),
      dist = dist)
    
    line_split <- lwgeom::st_split(x, blade_pts)
    
    df_tmp <- sf::st_collection_extract(line_split, "LINESTRING")
    
    df_tmp <- 
      df_tmp %>% 
      dplyr::mutate(len = as.numeric(sf::st_length(.))) %>%
      dplyr::filter(len > 6) %>% 
      dplyr::mutate(id = 1:nrow(.))
    
    df_splited <- rbind(df_splited, df_tmp)
  }
  return(df_splited)
}

## Then, specify the geographic coordinates of start/end of the transect and create a spatial object of it
sf_Munida <- data.frame(
  lon = c(170.7342),
  lat = c(-45.76978),
  lon2 = c(171.5363),
  lat2 = c(-45.82722)
)

sf_Munida$geom <- 
  sf::st_sfc(sapply(
    1:nrow(sf_Munida), 
    function(i){st_segment(sf_Munida[i,])}, simplify = FALSE), 
    crs = 4326)

sf_Munida <- sf::st_sf(sf_Munida)
# mapview::mapview(sf_Munida)

## Finally, split the main transect into the 12 5-km transects
n <- 12
crs <- 2193
dist <- 3

sf_Munida_split <- split_transects(df = sf_Munida,
                                   n = n, dist = dist, crs = crs)

sf_Munida_split <- 
  dplyr::select(sf_Munida_split, c(id, len)) %>% 
  dplyr::mutate(taiaroa_head = dplyr::case_when(
    id == 1 ~ "0-5 km",
    id == 2 ~ "5-10 km",
    id == 3 ~ "10-15 km",
    id == 4 ~ "15-20 km",
    id == 5 ~ "20-25 km",
    id == 6 ~ "25-30 km",
    id == 7 ~ "30-35 km",
    id == 8 ~ "35-40 km",
    id == 9 ~ "40-45 km",
    id == 10 ~ "45-50 km",
    id == 11 ~ "50-55 km",
    id == 12 ~ "55-60 km"
  )) %>% 
  dplyr::mutate(taiaroa_head = factor(taiaroa_head,
                                      levels = c(
                                        "0-5 km", "5-10 km", "10-15 km", "15-20 km",
                                        "20-25 km", "25-30 km", "30-35 km", "35-40 km",
                                        "40-45 km", "45-50 km", "50-55 km", "55-60 km"
                                      )))

# mapview::mapview(sf_Munida_split, zcol = "taiaroa_head")

## Save it 
# sf::write_sf(sf_Munida_split,
#              "./data-raw/spatial/munida_5km-transects.gpkg")

## For the sake of the study area map, add a buffer to ease visualization
sf_Munida_split_buffer <- sf::st_buffer(sf_Munida_split, dist = 500, endCapStyle = "FLAT")

# mapview::mapview(sf_Munida_split_buffer)

## Maps ####

## NZ base map
nz_base_map <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "black") + #lightgrey
  coord_sf(xlim = c(166, 179), ylim = c(-48, -34))


## (A) NZ map -- void
nz_base_map_rect <- 
  nz_base_map +
  geom_rect(data = nz_polygon,
            aes(xmin = 170, xmax = 171.5, ymin = -46.2, ymax = -45.5),
            colour = "red", fill = "NA", linewidth = 1) +
  theme_void()

ggsave(nz_base_map_rect,
       file = "./results/map_A_aotearoa.png",
       height = 5.5, width = 4, units = "cm", dpi = 300)

## (B) Munida base map
munida_base_map <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  geom_sf(data = nz_isobaths[nz_isobaths$DEPTH %in% c(100, 500),], 
          aes(linetype  = as.factor(DEPTH)), colour = "black") +
  scale_linetype_manual(values = c("dotdash", "solid"), name = "Isobaths") +
  coord_sf(xlim = c(170.45, 171.5), ylim = c(-45.72, -45.95)) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_blank())

# ## Munida base map (end up not using the 'inset')
# munida_base_map_final <-
#   cowplot::ggdraw() +
#   cowplot::draw_plot(munida_base_map) + 
#   cowplot::draw_plot(nz_base_map_rect, 
#                      x = -0.04, y = 0.8, # y = 0.58
#                      width = 0.43, height = 0.43)

ggsave(munida_base_map,
       file = "./results/map_B_munida.png",
       height = 9, width = 17, units = "cm", dpi = 300)

## (C) Add transect and other isobaths to the map
palette_12cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "BrBG"))(12)

munida_base_map_transect <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  # ## Add isobaths
  geom_sf(data = nz_isobaths[nz_isobaths$DEPTH %in% c(100, 200, 500, 1000),],
          aes(linetype  = as.factor(DEPTH)), colour = "black") +
  scale_linetype_manual(values = c("dotdash", "dotted", "solid", "twodash"), name = "Isobaths") +
  ## Add the transect line
  geom_sf(data = sf_Munida_split_buffer,
          aes(fill = as.factor(taiaroa_head)), color = "white") +
  scale_fill_manual(values = palette_12cols, name = "") + # Distance from\nTaiaroa Head
  coord_sf(xlim = c(170.45, 171.5), ylim = c(-45.72, -45.95)) + 
  ylab("") + xlab("") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical", 
        legend.margin = margin()) +
  ggspatial::annotation_scale(location = "bl") +
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering(),
                                    location = "tr", 
                                    pad_x = unit(0.75, "cm"), pad_y = unit(0.55, "cm"),
                                    height = unit(0.8, "cm"), width = unit(0.8, "cm"))

ggsave(munida_base_map_transect,
       file = "./results/map_C_munida_transect.png",
       height = 11, width = 17, units = "cm", dpi = 300)
