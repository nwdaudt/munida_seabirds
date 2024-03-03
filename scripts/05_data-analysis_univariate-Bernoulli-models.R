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
  # and transform counts to presence-absence ("p_a") [0,1]
  dplyr::mutate(p_a = replace(p_a, p_a > 0, 1)) %>% 
  # Remove year 2012
  dplyr::filter(year > 2012)%>% 
  # Create 'dist_coast', with 'centroid' distance from Taiaroa Head
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

spp <- unique(long_data_pa$species)

## Bernoulli GLM (y ~ dist_coast, per year) ####

# List to save models
sp_Bernoulli_year_models <- list()

# Dataframe to save fitted values and plot
dfs_yfit_year_sp <- data.frame()

for (sp in spp) {
  
  # Get 'sp' as a character vector
  sp_i <- as.character(sp)
  
  # Filter 'sp_i'
  tmp <- 
    long_data_pa %>% 
    dplyr::filter(species == sp_i)
  
  # Fit the model
  glm_tmp <- glm(p_a ~ dist_coast + year, data = tmp, family = binomial(link = "logit"))
  
  sp_Bernoulli_year_models[[sp_i]] <- glm_tmp
  
  df_yfit_year_sp <- 
    cbind(tmp, 
          data.frame(yfit = predict(glm_tmp,type = "response")))
  
  dfs_yfit_year_sp <- rbind(dfs_yfit_year_sp, 
                            df_yfit_year_sp)
  
  # Clean environment
  rm("sp", "sp_i", "tmp", "glm_tmp", "df_yfit_year_sp")
}

## Save model results
saveRDS(sp_Bernoulli_year_models,
        "./results/glm-Bernoulli_spp-dist-coast-years.rds")

## Plot results
plot_prob_occ_species_year <- 
  ggplot(data = dfs_yfit_year_sp,
       aes(x = taiaroa_east, y = yfit, group = as.factor(year), colour = as.factor(year))) +
  geom_line() + 
  scale_color_brewer(palette = "Reds") +
  scale_x_discrete(labels = c("0-5 km", "", "", "", "", "25-30 km", "", "", "", "", "", "55-60 km")) +
  ylab("Predicted probability of occurrence") + xlab("") +
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

rm("sp_Bernoulli_year_models", "dfs_yfit_year_sp",
   "plot_prob_occ_species_year")

## Bernoulli GLM (y ~ dist_coast, per season) ####

# List to save models
sp_Bernoulli_seasons_models <- list()

# Dataframe to save fitted values and plot
dfs_yfit_seasons_sp <- data.frame()

for (sp in spp) {
  
  # Get 'sp' as a character vector
  sp_i <- as.character(sp)
  
  # Filter 'sp_i'
  tmp <- 
    long_data_pa %>% 
    dplyr::filter(species == sp_i)
  
  # Fit the model
  glm_tmp <- glm(p_a ~ dist_coast + season, data = tmp, family = binomial(link = "logit"))
  
  sp_Bernoulli_seasons_models[[sp_i]] <- glm_tmp
  
  df_yfit_seasons_sp <- 
    cbind(tmp, 
          data.frame(yfit = predict(glm_tmp,type = "response")))
  
  dfs_yfit_seasons_sp <- rbind(dfs_yfit_seasons_sp, 
                               df_yfit_seasons_sp)
  
  # Clean environment
  rm("sp", "sp_i", "tmp", "glm_tmp", "df_yfit_seasons_sp")
}

## Save model results
saveRDS(sp_Bernoulli_seasons_models,
        "./results/glm-Bernoulli_spp-dist-coast-seasons.rds")

## Plot results
plot_prob_occ_species_seasons <- 
  ggplot(data = dfs_yfit_seasons_sp,
         aes(x = taiaroa_east, y = yfit, group = season, colour = season)) +
  geom_line() + 
  scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")) +
  scale_x_discrete(labels = c("0-5 km", "", "", "", "", "25-30 km", "", "", "", "", "", "55-60 km")) +
  ylab("Predicted probability of occurrence") + xlab("") +
  facet_wrap(~ species, scales = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(0.8, 0.05)) +
  guides(colour = guide_legend(nrow = 1))

ggsave(plot_prob_occ_species_seasons,
       filename = "./results/glm-Bernoulli_spp-seasons.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)

