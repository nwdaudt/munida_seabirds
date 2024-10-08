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
data <- 
  read.csv("./data-processed/all_data_long.csv") %>% 
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

data$season <- 
  factor(data$season,
         levels = c("summer", "autumn", "winter", "spring"),
         labels = c("Summer", "Autumn", "Winter", "Spring"))

data$count <- as.numeric(data$count)

## Prep for modelling ####

spp_cols_all <- unique(data$species)
sp_cols_only <- spp_cols_all[! grepl("unknown", spp_cols_all)]

### Transform it from long to wide format
wide_data <- 
  data %>% 
  ## Remove 'species_nice_name' for now
  dplyr::select(- species_nice_name) %>% 
  ## Pivot wider
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0) %>% 
  ## To match-up with the Temperature/Salinity data, we will only model the way back ("inbound")
  dplyr::filter(direction == "inbound")
  

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
  ### Create 'dist_coast', with 'centroid' distance from Taiaroa Head
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

### Add again 'species_nice_name' (manually; see script '01')
# but also, add the distribution status (see Table S2 in the supplementary material of the manuscript)

long_data_pa <-
  long_data_pa %>% 
  dplyr::mutate(species_nice_name = dplyr::case_when(
    species == "black_backed_gull" ~ "Black-backed gull [(S)]",
    species == "red_billed_gull" ~ "Red-billed gull [(S)]",
    species == "white_capped_mollymawk" ~ "White-capped albatross [M - SA, SO]",
    species == "white_fronted_tern" ~ "White-fronted tern [(S)]",
    species == "sooty_shearwater" ~ "Sooty shearwater [M - NP, EP]",
    species == "cape_petrel" ~ "Cape petrel [D - SWP, SO]",
    species == "southern_royal_albatross" ~ "Southern royal albatross [M - SA, SO]",
    species == "bullers_mollymawk" ~ "Buller's albatross [M - EP]",
    species == "white_chinned_petrel" ~ "White-chinned petrel [D - SO]",
    species == "bullers_shearwater" ~ "Buller's shearwater [M - NP, EP]",
    species == "hutton_fluttering_shearwater" ~ "Hutton's/Fluttering shearwater [M - TS/A]",
    species == "northern_royal_albatross" ~ "Northern royal albatross [M - SA, SO]",
    species == "salvins_mollymawk" ~ "Salvin's albatross [M - EP, SA, SO]",
    species == "black_browed_mollymawk" ~ "Black-browed albatross [D - SO]",
    species == "fairy_prion" ~ "Fairy prion [D - SWP, SO]",
    species == "black_bellied_storm_petrel" ~ "Black-bellied storm petrel [M - SWP]",
    species == "campbell_albatross" ~ "Campbell albatross [M - TS/A, SWP]",
    species == "mottled_petrel" ~ "Mottled petrel [M - NP]",
    species == "otago_shag" ~ "Otago shag [(S)]",
    species == "light_mantled_sooty_albatross" ~ "Light-mantled albatross [D - SO]",
    species == "black_fronted_tern" ~ "Black-fronted tern [(S)]",
    species == "grey_petrel" ~ "Grey petrel [D - SO]",
    species == "broad_billed_prion" ~ "Broad-billed prion [D - SWP, SO]",
    species == "white_headed_petrel" ~ "White-headed petrel [D - SO]",
    species == "spotted_shag" ~ "Spotted shag [(S)]",
    species == "wilsons_storm_petrel" ~ "Wilson's storm petrel [(M - NP)]",
    species == "grey_backed_storm_petrel" ~ "Grey-backed storm petrel [D - SO]",
    species == "southern_giant_petrel" ~ "Southern giant petrel [(D - SO)]",
    species == "northern_giant_petrel" ~ "Northern giant petrel [D - SO]",
    species == "grey_faced_petrel" ~ "Grey-faced petrel [D - SP]",
    species == "soft_plumaged_petrel" ~ "Soft-plumaged petrel [D - SO]",
    species == "white_faced_storm_petrel" ~ "White-faced storm petrel [M - EP]",
    species == "wandering_albatross" ~ "Wandering albatross [D - SO]",
    species == "westland_petrel" ~ "Westland petrel [M - EP]",
    species == "diving_petrel" ~ "Diving petrel [D - SWP]",
    species == "black_billed_gull" ~ "Black-billed gull [(S)]",
    species == "cooks_petrel" ~ "Cook's petrel [M - NP, EP]",
    species == "antarctic_fulmar" ~ "Antarctic fulmar [(D - SO)]",
    species == "yellow_eye_penguin" ~ "Yellow-eyed penguin [S]",
  ), .after = species)

## As good practice for modelling, centre 'year' and 'dist_coast'
long_data_pa <-
  long_data_pa %>% 
  dplyr::mutate(dist_coast = scale(long_data_pa$dist_coast, scale = FALSE)[,1],
                year_centre = scale(long_data_pa$year, scale = FALSE)[,1])

## Get a vector with species names to loop through
spp <- unique(long_data_pa$species_nice_name)

## Bernoulli GLM (y ~ dist_coast + year) ####

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
    dplyr::filter(species_nice_name == sp_i)
  
  # Fit the model
  glm_tmp <- glm(p_a ~ dist_coast + year_centre, data = tmp, family = binomial(link = "logit"))
  
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
# saveRDS(sp_Bernoulli_year_models,
#         "./results/glm-Bernoulli_spp-dist-coast-years.rds")

## Plot results (conditional to distance from coast)
plot_prob_occ_species_distcoastyear <- 
  ggplot(data = dfs_yfit_year_sp,
       aes(x = taiaroa_east, y = yfit, group = as.factor(year), colour = as.factor(year))) +
  geom_line() + 
  scale_color_brewer(palette = "Reds") +
  scale_x_discrete(labels = c("0-5 km", "", "", "", "", 
                              "25-30 km", "", "", "", "", "", "55-60 km")) +
  ylab("Predicted yearly probability of occurrence, conditional on distance from coast") + 
  xlab("") +
  facet_wrap(~ species_nice_name, scales = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 8, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(0.8, 0.05)) +
  guides(colour = guide_legend(nrow = 2))

# ggsave(plot_prob_occ_species_distcoastyear,
#        filename = "./results/glm-Bernoulli_spp-years.pdf",
#        height = 25, width = 40, units = "cm", dpi = 300)


### Get coeff+CI for the year parameter
df_year_slope <- data.frame(sp_name = rep(NA, length(sp_Bernoulli_year_models)),
                            coeff = rep(NA, length(sp_Bernoulli_year_models)),
                            confint2 = rep(NA, length(sp_Bernoulli_year_models)),
                            confint95 = rep(NA, length(sp_Bernoulli_year_models)),
                            p_value = rep(NA, length(sp_Bernoulli_year_models)))

for(i in 1:length(sp_Bernoulli_year_models)){
  
  tmp_summary <- summary(sp_Bernoulli_year_models[[i]])
  
  df_year_slope[i, ] <- 
    data.frame(sp_name = names(sp_Bernoulli_year_models[i]),
               coeff = sp_Bernoulli_year_models[[i]][["coefficients"]][["year_centre"]],
               confint2 = confint(sp_Bernoulli_year_models[[i]])[3,1], 
               confint95 = confint(sp_Bernoulli_year_models[[i]])[3,2],
               p_value = tmp_summary[["coefficients"]][3,4]
    )
  
  rm("tmp_summary", "i")
}

## For Cook's petrel, the CIs are a bit all over the show and it couldn't estimate an upper CI
# confint(sp_Bernoulli_year_models[["Cook's petrel [M - NP, EP]"]])
#                     2.5 %       97.5 %
# (Intercept)            NA 3.302257e+05
# dist_coast    -0.06917448 3.371251e-02
# year        -163.23577458           NA

df_year_slope <- 
  df_year_slope %>% 
  dplyr::mutate(evidence = 
                  factor(dplyr::case_when(
                    ## Following Muff et al. (2022) Trends Ecol. Evol.
                    p_value > 0.5 ~ "No",
                    p_value <= 0.5 & p_value > 0.1 ~ "Little",
                    p_value <= 0.1 & p_value > 0.05 ~ "Weak",
                    p_value <= 0.05 & p_value > 0.01 ~ "Moderate",
                    p_value <= 0.01 & p_value > 0.001 ~ "Strong",
                    p_value <= 0.001 ~ "Very strong"), 
                    levels = c("No", "Little", "Weak", "Moderate", "Strong", "Very strong"))
                ) %>% 
  dplyr::mutate(sp_name_order = forcats::fct_reorder(sp_name, as.numeric(evidence)))

## 15 (38%) out of 39 of the species have weak-to-strong evidence 
## of changing their probability of occurrence related to year
# table(df_year_slope$evidence)

plot_coeff_CI_year <- 
  ggplot(data = df_year_slope[! df_year_slope$sp_name == "Cook's petrel [M - NP, EP]", ]) +
  geom_point(aes(x = coeff, y = sp_name_order, color = evidence), size = 2.5) +
  geom_errorbar(aes(y = sp_name, xmin = confint2, xmax = confint95, color = evidence), #color = "black",
                width = 0.5) +
  scale_color_manual(values = c(hcl.colors(10, "Mako", rev = T)[c(2,3,6,7,10)])) + # hcl.colors(7, "Oslo")[c(3,5:7)]
  geom_vline(xintercept = 0,
             linetype = "longdash", colour = "grey50") +
  ylab("") +  xlab("Coefficient + C.I.") +
  theme_bw() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 11, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = c(0.8, 0.16))

# ggsave(plot_coeff_CI_year,
#        filename = "./results/glm-Bernoulli_spp-years_year-coeff-CIs.pdf",
#        height = 19, width = 15, units = "cm", dpi = 300)

rm("sp_Bernoulli_year_models", 
   "dfs_yfit_year_sp",
   "plot_prob_occ_species_distcoastyear",
   "df_year_slope",
   "plot_coeff_CI_year")

## Bernoulli GLM (y ~ dist_coast + season) ####

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
    dplyr::filter(species_nice_name == sp_i)
  
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

# ## Save model results
# saveRDS(sp_Bernoulli_seasons_models,
#         "./results/glm-Bernoulli_spp-dist-coast-seasons.rds")

## Plot results
plot_prob_occ_species_seasons <- 
  ggplot(data = dfs_yfit_seasons_sp,
         aes(x = taiaroa_east, y = yfit, group = season, colour = season)) +
  geom_line() + 
  scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")) +
  scale_x_discrete(labels = c("0-5 km", "", "", "", "", 
                              "25-30 km", "", "", "", "", "", "55-60 km")) +
  ylab("Predicted seasonal probability of occurrence, conditional on distance from coast") + 
  xlab("") +
  facet_wrap(~ species_nice_name, scales = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 8, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(0.8, 0.05)) +
  guides(colour = guide_legend(nrow = 1))

# ggsave(plot_prob_occ_species_seasons,
#        filename = "./results/glm-Bernoulli_spp-seasons.pdf",
#        height = 25, width = 40, units = "cm", dpi = 300)

rm("sp_Bernoulli_seasons_models",
   "dfs_yfit_seasons_sp",
   "plot_prob_occ_species_seasons")
