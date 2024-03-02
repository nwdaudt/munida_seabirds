## 
## Bernoulli models, for each species
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script runs Bernoulli models for each species

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
# library(ggExtra)
# library(patchwork)
# library(RColorBrewer)
# library(colorspace)
# library(cowplot)

## Read data ####
data <- read.csv("./data-processed/all_data_long.csv")[, -1]

## Format some columns
data$taiaroa_east <- 
  factor(data$taiaroa_east,
         levels = c("TaiaroaEast0.5km", "TaiaroaEast5.10km",
                    "TaiaroaEast10.15km", "TaiaroaEast15.20km",
                    "TaiaroaEast20.25km", "TaiaroaEast25.30km",
                    "TaiaroaEast30.35km", "TaiaroaEast35.40km",
                    "TaiaroaEast40.45km", "TaiaroaEast45.50km",
                    "TaiaroaEast50.55km", "TaiaroaEast55.60km"))

data$direction <- 
  factor(data$direction,
         levels = c("eastward", "westward"))

data$season <- 
  factor(data$season,
         levels = c("summer", "autumn", "winter", "spring"))

data$count <- as.numeric(data$count)

## Prep for modelling ####

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
long_data_pa <- 
  wide_data %>%
  # Remove unknown and rare species columns
  dplyr::select(- all_of(unknown_and_rare_spp)) %>% 
  tidyr::pivot_longer(cols = c(black_backed_gull:yellow_eye_penguin),
                      names_to = "species",
                      values_to = "p_a") %>% 
  # and transform counts in presence-absence (p_a) [0,1]
  dplyr::mutate(p_a = replace(p_a, p_a > 0, 1)) %>% 
  # filter away year 2012
  dplyr::filter(year > 2012)

## Bernoulli GLM (y ~ dist_coast, per year) ####

spp <- unique(long_data_pa$species)
years <- unique(long_data_pa$year)

dfs_yfit_year_sp <- data.frame()

for (sp in spp) {
  
  list_yfit_year <- list()
  
  # Get 'sp' vector
  sp_i <- as.character(sp)
  
  # Filter
  tmp <- 
    long_data_pa %>% 
    dplyr::filter(species == sp_i) %>% 
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
  
  for(year in years){
    
    year_i <- year
    
    tmp_yr <- tmp %>% filter(year == year_i)
    
    glm_tmp <- glm(p_a ~ dist_coast, data = tmp_yr, family = binomial(link = "logit"))
    
    list_yfit_year[[as.character(year_i)]] <- 
      data.frame(taiaroa_east = tmp_yr$taiaroa_east,
                 yfit = predict(glm_tmp,type = "response")) %>% 
      dplyr::group_by(taiaroa_east) %>% 
      dplyr::summarise(yfit_avg = mean(yfit))
  }
  
  df_yfit_year_sp <- 
    dplyr::bind_rows(list_yfit_year, .id = "id") %>% 
    dplyr::mutate(species = sp_i)
  
  dfs_yfit_year_sp <- rbind(dfs_yfit_year_sp, df_yfit_year_sp)
  
  # Clean environment
  rm("list_yfit_year", 
     "sp", "sp_i", "tmp", 
     "year", "year_i", "tmp_yr", "glm_tmp",
     "df_yfit_year_sp")
}

rm("spp", "years")

# unique(dfs_yfit_year_sp$id)
# unique(dfs_yfit_year_sp$species)

plot_prob_occ_species_year <- 
  ggplot(data = dfs_yfit_year_sp,
       aes(x = taiaroa_east, y = yfit_avg, group = id, colour = id)) +
  geom_line() + 
  scale_color_brewer(palette = "Reds") +
  scale_x_discrete(labels = c("0-5 km", "", "", "", "", "25-30 km", "", "", "", "", "", "55-60 km")) +
  ylab("Yearly average predicted probability of occurrence") + xlab("") +
  facet_wrap(~ species, scales = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(0.8, 0.05)) +
  guides(colour = guide_legend(nrow = 2))

ggsave(plot_prob_occ_species_year,
       filename = "./results/glm-Bernoulli_spp-years.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)

## Bernoulli GLM (y ~ dist_coast, per season) ####

spp <- unique(long_data_pa$species)
seasons <- unique(long_data_pa$season)

dfs_yfit_season_sp <- data.frame()

for (sp in spp) {
  
  list_yfit_season <- list()
  
  # Get 'sp' vector
  sp_i <- as.character(sp)
  
  # Filter
  tmp <- 
    long_data_pa %>% 
    dplyr::filter(species == sp_i) %>% 
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
  
  for(season in seasons){
    
    season_i <- season
    
    tmp_season <- tmp %>% filter(season == season_i)
    
    glm_tmp <- glm(p_a ~ dist_coast, data = tmp_season, family = binomial(link = "logit"))
    
    list_yfit_season[[as.character(season_i)]] <- 
      data.frame(taiaroa_east = tmp_season$taiaroa_east,
                 yfit = predict(glm_tmp,type = "response")) %>% 
      dplyr::group_by(taiaroa_east) %>% 
      dplyr::summarise(yfit_avg = mean(yfit))
  }
  
  df_yfit_season_sp <- 
    dplyr::bind_rows(list_yfit_season, .id = "id") %>% 
    dplyr::mutate(species = sp_i)
  
  dfs_yfit_season_sp <- rbind(dfs_yfit_season_sp, df_yfit_season_sp)
  
  # Clean environment
  rm("list_yfit_season", 
     "sp", "sp_i", "tmp", 
     "season", "season_i", "tmp_season", "glm_tmp",
     "df_yfit_season_sp")
}

rm("spp", "seasons")

# unique(dfs_yfit_season_sp$id)
# unique(dfs_yfit_season_sp$species)

plot_prob_occ_species_seasons <- 
  ggplot(data = dfs_yfit_season_sp,
         aes(x = taiaroa_east, y = yfit_avg, group = id, colour = id)) +
  geom_line() + 
  scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),
                    name = "Season") +
  scale_x_discrete(labels = c("0-5 km", "", "", "", "", "25-30 km", "", "", "", "", "", "55-60 km")) +
  ylab("Seasonal average predicted probability of occurrence") + xlab("") +
  facet_wrap(~ species, scales = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 13),
        legend.position = c(0.8, 0.05)) +
  guides(colour = guide_legend(nrow = 1))

ggsave(plot_prob_occ_species_seasons,
       filename = "./results/glm-Bernoulli_spp-seasons.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)
