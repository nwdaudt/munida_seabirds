## 
## Model-based ordinations
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script runs model-based ordinations using the {gllvm} framework

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)
library(gllvm)
library(ggplot2)
library(ggExtra)
library(patchwork)
library(RColorBrewer)
# library(colorspace)
# library(cowplot)

## Read data ####
data <- 
  read.csv("./data-processed/all_data_long.csv") %>% 
  dplyr::filter(! year == "2012")

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

## Format some columns
data$taiaroa_east <- 
  factor(data$taiaroa_east,
         levels = c("TaiaroaEast0.5km", "TaiaroaEast5.10km",
                    "TaiaroaEast10.15km", "TaiaroaEast15.20km",
                    "TaiaroaEast20.25km", "TaiaroaEast25.30km",
                    "TaiaroaEast30.35km", "TaiaroaEast35.40km",
                    "TaiaroaEast40.45km", "TaiaroaEast45.50km",
                    "TaiaroaEast50.55km", "TaiaroaEast55.60km"),
         labels = c("0-5 km", "5-10 km", "10-15 km", "15-20 km",
                    "20-25 km", "25-30 km", "30-35 km", "35-40 km",
                    "40-45 km", "45-50 km", "50-55 km", "55-60 km"))

data$direction <- 
  factor(data$direction,
         levels = c("outbound", "inbound"))

data$season <- 
  factor(data$season,
         levels = c("summer", "autumn", "winter", "spring"),
         labels = c("Summer", "Autumn", "Winter", "Spring"))

data$water_mass <- 
  factor(data$water_mass,
         levels = c("STW", "NW", "SASW"),
         labels = c("STW", "NW", "SASW"))

data$count <- as.numeric(data$count)

## Prep for modelling ####

spp_cols_all <- unique(data$species)
sp_cols_only <- spp_cols_all[! grepl("unknown", spp_cols_all)]

### First, transform it from long to wide format
wide_data <- 
  data %>% 
  # Need to delete this column otherwise it messes up with the 'pivot_wide' results
  dplyr::select(- species_nice_name) %>% 
  # No info on 'water_mass' preclude running the GLLVM models, so get rid of them now
  dplyr::filter(! is.na(water_mass)) %>% 
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0) %>% 
  ## To match-up with the Temperature/Salinity data, we will only model the way back ("inbound")
  dplyr::filter(direction == "inbound")

### Second, identify 'rare' species (i.e. less than 3 occurrences)
sp_rare_cols <- 
  # Get species names and number of occurrences
  data.frame(
    species = sp_cols_only,
    n_occ = apply(wide_data[sp_cols_only], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL) %>%
  # Filter and pull species names
  dplyr::filter(n_occ < 3) %>%
  dplyr::pull(species)

### Get seabird data
spp_matrix <- 
  wide_data %>%
  # Select species columns
  dplyr::select(all_of(sp_cols_only)) %>%
  # But, remove rare species columns -- they will be more noisy than explanatory
  dplyr::select(- all_of(sp_rare_cols))

### ----------- Presence/Absence ----------- ###
# spp_matrix_pa <- spp_matrix
# spp_matrix_pa[spp_matrix_pa > 0] <- 1

## Unconstrained ordination, purely biological (Null model) ####

### Run NULL models with 1 and 2 LV, respectively

gllvm_null_model_lv1 <-
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 1, 
               family = "negative.binomial",
               seed = 321)

gllvm_null_model_lv2 <-
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 2, 
               family = "negative.binomial",
               seed = 321)

### Based on the BIC, choose the best model

BIC(gllvm_null_model_lv1, gllvm_null_model_lv2)
#                      df      BIC
# gllvm_null_model_lv1 117 14819.02
# gllvm_null_model_lv2 155 14643.49 ## -->> best model

## Run AIC's just to have another layer of check
# AIC(gllvm_null_model_lv1, gllvm_null_model_lv2)
# AICc(gllvm_null_model_lv1, gllvm_null_model_lv2)
## -->> All Information Criteria suggest 'lv2' model fits better

### Residual plots

# plot(gllvm_null_model_lv1, which = 1:4, mfrow = c(2,2))

# pdf(file = "./results/gllvm_null-model_lv2_residuals.pdf")
# plot(gllvm_null_model_lv2, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model
# saveRDS(gllvm_null_model_lv2,
#         file = "./results/gllvm_null-model_lv2.rds")

### You can load the files back instead of running the models again
# gllvm_null_model_lv2 <- readRDS("./results/gllvm_null-model_lv2.rds")

### Get LV values and arrange it in a dataframe to plot

df_plot_null_model_lv2 <-
  cbind(wide_data,
        as.data.frame(gllvm::getLV.gllvm(gllvm_null_model_lv2))) %>% 
  dplyr::mutate(water_mass = factor(water_mass, levels = c("NW", "STW", "SASW")))

## Plot colour-coded by 'water_mass' ----------------------------------------- #

plot_null_model_watermass <- 
  ggplot(
    data = df_plot_null_model_lv2,
    aes(x = LV1, y = LV2,
        color = water_mass)) +
  geom_point(alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2") +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave(plot_null_model_watermass,
       filename = "./results/gllvm_null-model_lv2_biplot_watermass.pdf",
       height = 9, width = 10, units = "cm", dpi = 300)

## Plot colour-coded by 'season' --------------------------------------------- #

plot_null_model_season <- 
  ggplot(
    data = df_plot_null_model_lv2,
    aes(x = LV1, y = LV2,
        color = season)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave(plot_null_model_season,
       filename = "./results/gllvm_null-model_lv2_biplot_season.pdf",
       height = 9, width = 10, units = "cm", dpi = 300)

# plot_unconstrained_biol_model_season <- 
#   ggplot(data = df_plot_lv1_unconstr_biol_model, 
#          aes(x = V1, 
#              y = rep(0, times = nrow(df_plot_lv1_unconstr_biol_model)),
#              color = season)) +
#   geom_point(alpha = 0.6) + 
#   scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
#                                 "Winter" = "#E15759", "Spring" = "#76B7B2")) +
#   xlab("Latent Variable 1") + ylab("") +
#   theme_bw() + 
#   theme(axis.title.x = element_text(size = 12),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_blank(),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 10),
#         legend.position = "bottom")
# 
# plot_unconstrained_biol_model_season <- 
#   ggExtra::ggMarginal(plot_unconstrained_biol_model_season,
#                       type = "density",
#                       groupColour = TRUE,
#                       groupFill = TRUE)
# 
# ggsave(plot_unconstrained_biol_model_season,
#        filename = "./results/gllvm_unconstrained_biol_lv1_biplot_season.pdf",
#        height = 9, width = 12, units = "cm", dpi = 300)

## Plot colour-coded by 'taiaroa_head' --------------------------------------- #

# Specify a 12-colour palette
palette_12cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "BrBG"))(12)

plot_null_model_taiaroa <- 
  ggplot(
    data = df_plot_null_model_lv2,
    aes(x = LV1, y = LV2,
        color = taiaroa_east)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = palette_12cols) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

ggsave(plot_null_model_taiaroa,
       filename = "./results/gllvm_null-model_lv2_biplot_taiaroa.pdf",
       height = 11, width = 11, units = "cm", dpi = 300)


## --- patchwork them together --- ##
# (not great because the legend of '_taiaroa' plot is huge, hehe... so I will do it in Inkscape)
#
# gllvm_null_biplot_patchworked <-
#   (patchwork::wrap_elements(plot_null_model_watermass) /
#      (patchwork::wrap_elements(plot_null_model_taiaroa)) /
#      patchwork::wrap_elements(plot_null_model_season)) +
#   patchwork::plot_annotation(tag_levels = 'A')
# 
# ggsave(gllvm_null_biplot_patchworked,
#        filename = "./results/gllvm_unconstrained_biol_lv1_biplot_tilling.pdf",
#        height = 24, width = 12, units = "cm", dpi = 300)

## Clean environment
# rm("plot_null_model_watermass")
# rm("plot_null_model_taiaroa", "palette_12cols")
# rm("plot_null_model_season")

## Unconstrained ordination, purely biological (Null model), *with voyage random effect* ####

# gllvm_null_model_lv1_re <-
#   gllvm::gllvm(y = spp_matrix, 
#                X = data.frame(voyage = wide_data$id),
#                num.lv = 1, 
#                family = "negative.binomial",
#                row.eff = ~(1|voyage),
#                seed = 321)

gllvm_null_model_lv2_re <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(voyage = wide_data$id),
               num.lv = 2, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

BIC(gllvm_null_model_lv1_re, gllvm_null_model_lv2_re)
#                          df      BIC
# gllvm_null_model_lv1_re 118 14622.57
# gllvm_null_model_lv2_re 156 14702.92

AIC(gllvm_null_model_lv1_re, gllvm_null_model_lv2_re)
#                          df      AIC
# gllvm_null_model_lv1_re 118 14172.86
# gllvm_null_model_lv2_re 156 14108.39

AICc(gllvm_null_model_lv1_re, gllvm_null_model_lv2_re)
# gllvm_null_model_lv1_re 14175.03 
# gllvm_null_model_lv2_re 14112.19

## AIC and AICc both suggest the lv2 is better although BIC suggests otherwise;
## In addition, as a comparative with the 'null model' without RE, I'll select the 'lv2_re' model

### Residual plots

# pdf(file = "./results/gllvm_null-model-RE_lv2_residuals.pdf")
# plot(gllvm_null_model_lv2_re, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model
# saveRDS(gllvm_null_model_lv2_re,
#         file = "./results/gllvm_null-model-RE_lv2.rds")

### You can load the files back instead of running the models again
# gllvm_null_model_lv2_re <- readRDS("./results/gllvm_null-model-RE_lv2.rds")

### Get LV values and arrange it in a dataframe to plot

df_plot_null_model_lv2_re <-
  cbind(wide_data,
        as.data.frame(gllvm::getLV.gllvm(gllvm_null_model_lv2_re))) %>% 
  dplyr::mutate(water_mass = factor(water_mass, levels = c("NW", "STW", "SASW")))

## Plot colour-coded by 'water_mass' ----------------------------------------- #

plot_null_model_watermass_re <- 
  ggplot(
    data = df_plot_null_model_lv2_re,
    aes(x = LV1, y = LV2,
        color = water_mass)) +
  geom_point(alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2") +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave(plot_null_model_watermass_re,
       filename = "./results/gllvm_null-model-RE_lv2_biplot_watermass.pdf",
       height = 9, width = 10, units = "cm", dpi = 300)

## Plot colour-coded by 'season' --------------------------------------------- #

plot_null_model_season_re <- 
  ggplot(
    data = df_plot_null_model_lv2_re,
    aes(x = LV1, y = LV2,
        color = season)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave(plot_null_model_season_re,
       filename = "./results/gllvm_null-model-RE_lv2_biplot_season.pdf",
       height = 9, width = 10, units = "cm", dpi = 300)

## Plot colour-coded by 'taiaroa_head' --------------------------------------- #

# Specify a 12-colour palette
palette_12cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "BrBG"))(12)

plot_null_model_taiaroa_re <- 
  ggplot(
    data = df_plot_null_model_lv2_re,
    aes(x = LV1, y = LV2,
        color = taiaroa_east)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = palette_12cols) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

ggsave(plot_null_model_taiaroa_re,
       filename = "./results/gllvm_null-model-RE_lv2_biplot_taiaroa.pdf",
       height = 11, width = 11, units = "cm", dpi = 300)

## Unconstrained ordination, including predictors (Full model)) ####

### Run models with 0, 1 and 2 LV, respectively

# ### --> using 'taiaroa_east' (categorical)
# 
# gllvm_full_lv0_taiaroa <-
#   gllvm::gllvm(y = spp_matrix,
#                X = data.frame(season = wide_data$season,
#                               taiaroa_east = wide_data$taiaroa_east,
#                               water_mass = wide_data$water_mass,
#                               voyage = wide_data$id),
#                formula = ~ season + taiaroa_east + water_mass,
#                num.lv = 0,
#                family = "negative.binomial",
#                row.eff = ~(1|voyage),
#                seed = 321)
# # > "Standard errors for parameters could not be calculated, due to singular fit."
# 
# gllvm_full_lv1_taiaroa <-
#   gllvm::gllvm(y = spp_matrix,
#                X = data.frame(season = wide_data$season,
#                               taiaroa_east = wide_data$taiaroa_east,
#                               water_mass = wide_data$water_mass,
#                               voyage = wide_data$id),
#                formula = ~ season + taiaroa_east + water_mass,
#                num.lv = 1,
#                family = "negative.binomial",
#                row.eff = ~(1|voyage),
#                seed = 321)
# # > "Determinant of the variance-covariance matix is zero. 
# #    Please double check your model for e.g. overfitting or lack of convergence."
# 
# gllvm_full_lv2_taiaroa <-
#   gllvm::gllvm(y = spp_matrix,
#                X = data.frame(season = wide_data$season,
#                               taiaroa_east = wide_data$taiaroa_east,
#                               water_mass = wide_data$water_mass,
#                               voyage = wide_data$id),
#                formula = ~ season + taiaroa_east + water_mass,
#                num.lv = 2,
#                family = "negative.binomial",
#                row.eff = ~(1|voyage),
#                seed = 321)
# # > "Determinant of the variance-covariance matix is zero. 
# #    Please double check your model for e.g. overfitting or lack of convergence."

### --> using 'dist_coast' (continuous)

gllvm_full_lv0_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ season + dist_coast + water_mass,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

gllvm_full_lv1_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ season + dist_coast + water_mass,
               num.lv = 1, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)
# "Determinant of the variance-covariance matix is zero. 
#  Please double check your model for e.g. overfitting or lack of convergence."

gllvm_full_lv2_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ season + dist_coast + water_mass,
               num.lv = 2, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

### Based on the BIC, choose the best model

BIC(gllvm_full_lv0_taiaroa, gllvm_full_lv0_distcoast,
    gllvm_full_lv1_taiaroa, gllvm_full_lv1_distcoast,
    gllvm_full_lv2_taiaroa, gllvm_full_lv2_distcoast) 

#                           df      BIC
# gllvm_full_lv0_taiaroa   703 -7.984271e+192 ## Clearly something went wrong here, so I won't consider this model
# gllvm_full_lv0_distcoast 313   1.395639e+04 ## -->> best model
# gllvm_full_lv1_taiaroa   742   1.593856e+04
# gllvm_full_lv1_distcoast 352   1.413170e+04
# gllvm_full_lv2_taiaroa   780   1.615938e+04
# gllvm_full_lv2_distcoast 390   1.433808e+04

## Note: all models using 'taiaroa' (as categorical) doubled the degrees of freedom; --- #
##       also, not considering the 'lv0_taiaroa' model, they all had higher BIC -------- #

### Residual plots (just ran for the 'best model')
# pdf(file = "./results/gllvm_full-model-distcoast_lv0_residuals.pdf")
# plot(gllvm_full_lv0_distcoast, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects
# saveRDS(gllvm_full_lv0_distcoast,
#         file = "./results/gllvm_full-model-distcoast_lv0_model.rds")

### You can load the files back instead of running the models again
# gllvm_full_lv0_distcoast <- readRDS("./results/gllvm_full-model-distcoast_lv0_model.rds")

### Clear environment, as these models will not be used further
rm("gllvm_full_lv0_taiaroa", 
   "gllvm_full_lv1_taiaroa", 
   "gllvm_full_lv2_taiaroa" 
   #, "gllvm_full_lv1_distcoast", "gllvm_full_lv2_distcoast"
   )

## Covariate selection in model 'gllvm_full_lv0_distcoast' #### 

## 'Full' model (with the three predictors): gllvm_full_lv0_distcoast

# season + water_mass
gllvm_full_lv0_distcoast_season.watermass <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ season + water_mass,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# dist_coast + water_mass
gllvm_full_lv0_distcoast_distcoast.watermass <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ dist_coast + water_mass,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# dist_coast + season
gllvm_full_lv0_distcoast_distcoast.season <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ dist_coast + season,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# Only season
gllvm_full_lv0_distcoast_season <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ season,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# Only dist_coast
gllvm_full_lv0_distcoast_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ dist_coast,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)
# > "Standard errors for parameters could not be calculated, due to singular fit."

# Only water_mass
gllvm_full_lv0_distcoast_watermass <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass,
                              voyage = wide_data$id),
               formula = ~ water_mass,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)
# > "Standard errors for parameters could not be calculated, due to singular fit."

### Based on the BIC, choose the best model

BIC(gllvm_full_lv0_distcoast,
    gllvm_full_lv0_distcoast_season.watermass,
    gllvm_full_lv0_distcoast_distcoast.watermass,
    gllvm_full_lv0_distcoast_distcoast.season,
    gllvm_full_lv0_distcoast_season,
    gllvm_full_lv0_distcoast_distcoast,
    gllvm_full_lv0_distcoast_watermass)

#                                               df            BIC
# gllvm_full_lv0_distcoast                     313   1.395639e+04
# gllvm_full_lv0_distcoast_season.watermass    274   1.408454e+04
# gllvm_full_lv0_distcoast_distcoast.watermass 196   1.437498e+04
# gllvm_full_lv0_distcoast_distcoast.season    235   1.365818e+04 ## -->> best model
# gllvm_full_lv0_distcoast_season              196   1.424825e+04
# gllvm_full_lv0_distcoast_distcoast           118 -1.539534e+143 ## Clearly something went wrong here, so I won't consider this model 
# gllvm_full_lv0_distcoast_watermass           157 -2.405874e+290 ## Clearly something went wrong here, so I won't consider this model 

### Residual plots (only for the 'best model')
# pdf(file = "./results/gllvm_best-model_lv0_distcoast-season_residuals.pdf")
# plot(gllvm_full_lv0_distcoast_distcoast.season, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects
# saveRDS(gllvm_full_lv0_distcoast_distcoast.season,
#         file = "./results/gllvm_best-model_lv0_distcoast-season_model.rds")

### Clear environment, as these models will not be used further
rm("gllvm_full_lv0_distcoast",
   "gllvm_full_lv0_distcoast_season.watermass",
   "gllvm_full_lv0_distcoast_distcoast.watermass",
   "gllvm_full_lv0_distcoast_season",
   "gllvm_full_lv0_distcoast_distcoast",
   "gllvm_full_lv0_distcoast_watermass")

## Coefficient plots for the 'best model' (~ dist_coast + season) ####

gllvm_full_lv0_distcoast_distcoast.season <- 
  readRDS("./results/gllvm_best-model_lv0_distcoast-season_model.rds")

# Adjust species name for plot -- a bit of a manual job...
gllvm_spp <- rownames(gllvm_full_lv0_distcoast_distcoast.season$params$Xcoef)
gllvm_spp <- snakecase::to_sentence_case(gllvm_spp)
gllvm_spp[1] <- "Black-backed gull"
gllvm_spp[2] <- "Red-billed gull"
gllvm_spp[3] <- "White-capped albatross"
gllvm_spp[4] <- "White-fronted tern"
gllvm_spp[8] <- "Buller's albatross"
gllvm_spp[9] <- "White-chinned petrel"
gllvm_spp[10] <- "Buller's shearwater"
gllvm_spp[11] <- "Hutton's/Fluttering shearwater"
gllvm_spp[13] <- "Salvin's albatross"
gllvm_spp[14] <- "Black-browed albatross"
gllvm_spp[16] <- "Black-bellied storm petrel"
gllvm_spp[20] <- "Light-mantled albatross"
gllvm_spp[21] <- "Black-fronted tern"
gllvm_spp[23] <- "Broad-billed prion"
gllvm_spp[24] <- "White-headed petrel"
gllvm_spp[26] <- "Wilson's storm petrel"
gllvm_spp[27] <- "Grey-backed storm petrel"
gllvm_spp[30] <- "Grey-faced petrel"
gllvm_spp[31] <- "Soft-plumaged petrel"
gllvm_spp[32] <- "White-faced storm petrel"
gllvm_spp[36] <- "Black-billed gull"
gllvm_spp[37] <- "Cook's petrel"
gllvm_spp[39] <- "Yellow-eyed penguin"

# I was not cleaver enough to find out how to automatically get the ordered vector,
# so I specified it by hand (after running the plot once to see the order)
gllvm_spp_ordered <- c(
  gllvm_spp[22], gllvm_spp[26], gllvm_spp[24], gllvm_spp[31], gllvm_spp[33], 
  gllvm_spp[20], gllvm_spp[27], gllvm_spp[17], gllvm_spp[16], gllvm_spp[30], 
  gllvm_spp[9], gllvm_spp[14], gllvm_spp[15], gllvm_spp[18], gllvm_spp[23], 
  gllvm_spp[32], gllvm_spp[28], gllvm_spp[7], gllvm_spp[34], gllvm_spp[10], 
  gllvm_spp[3], gllvm_spp[13], gllvm_spp[29], gllvm_spp[12], gllvm_spp[35], 
  gllvm_spp[37], gllvm_spp[8], gllvm_spp[6], gllvm_spp[37], gllvm_spp[21], ###### ----- FIX gllvm_spp[37]
  gllvm_spp[4], gllvm_spp[11], gllvm_spp[5], gllvm_spp[1], gllvm_spp[19], 
  gllvm_spp[39], gllvm_spp[2], gllvm_spp[36], gllvm_spp[25]
)

At.y <- seq(1, length(gllvm_spp))

# coefplot: 'seasons' (summer == intercept)
pdf(file = "./results/gllvm_best-model_lv0_distcoast-season_coefplot-season.pdf",
    width = 10, height = 8)
par(mfrow = c(1, 3), 
    oma = c(1, 12, 1, 1),
    cex = 1)

gllvm::coefplot(gllvm_full_lv0_distcoast_distcoast.season,
                which.Xcoef = c(2),
                order = FALSE,
                cex.ylab = 0.0001,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1))
axis(side = 2, at = At.y, labels = gllvm_spp, las = 1)
title(xlab = "Autumn", cex.lab = 1.1)

gllvm::coefplot(gllvm_full_lv0_distcoast_distcoast.season,
                which.Xcoef = c(3),
                order = FALSE,
                y.label = FALSE,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1))
title(xlab = "Winter", cex.lab = 1.1)

gllvm::coefplot(gllvm_full_lv0_distcoast_distcoast.season,
                which.Xcoef = c(4),
                order = FALSE,
                y.label = FALSE,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1))
title(xlab = "Spring", cex.lab = 1.1)

dev.off()


# coefplot: 'dist_coast'

pdf(file = "./results/gllvm_best-model_lv0_distcoast-season_coefplot-distcoast.pdf",
    width = 5.5, height = 8)
par(mfrow = c(1, 1), 
    mar = c(0.1, 0.1, 0.1, 0.1), 
    oma = c(1, 6.5, 1, 1))

gllvm::coefplot(gllvm_full_lv0_distcoast_distcoast.season,
                which.Xcoef = c(1),
                order = TRUE,
                cex.ylab = 0.0001,
                cex.lab = 0.0001)
axis(side = 2, at = At.y, labels = rev(gllvm_spp_ordered), las = 1)
title(xlab = "Distance from coast", cex.lab = 1.1)

dev.off()

# par(mfcol = c(1,1))
rm("gllvm_spp", "gllvm_spp_ordered", "At.y")

## Compare predictions between GLLVMs accounting for predictors, with(out) LVs, and raw data ####

## The idea of this section is compare the effects of including Latent Variables (LV) in the models.
## Although BIC and AIC values suggests the model without LVs is the best, we want to check this further.

## So, to compare the influence of LVs, we selected the full 'distcoast' models that used LV == 0, 1, and 2.
## We then get the predicted/expected values for each model, for each species, and plot them all together to verify
## any possible (dis)agreement between models. 

# gllvm_full_lv0_distcoast
# gllvm_full_lv1_distcoast
# gllvm_full_lv2_distcoast

## Get predicted/expected values for unconstrained model without LVs [LV == 0] (lv0)

fitmod_lv0 <- data.frame(
  exp(
    predict(gllvm_full_lv0_distcoast, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass))
  )
)

fitmod_lv0 <- fitmod_lv0[order(wide_data$season), ]

fitlong_lv0 <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv0), fitmod_lv0), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

fitlong_lv0 <-
  cbind(fitlong_lv0, Source = rep("LV = 0", times = nrow(fitlong_lv0)))

## Get predicted/expected values for unconstrained model with [LV == 1] (lv1)

fitmod_lv1 <- data.frame(
  exp(
    predict(gllvm_full_lv0_distcoast, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass))
  )
)

fitmod_lv1 <- fitmod_lv1[order(wide_data$season), ]

fitlong_lv1 <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv1), fitmod_lv1), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

fitlong_lv1 <-
  cbind(fitlong_lv1, Source = rep("LV = 1", times = nrow(fitlong_lv1)))

## Get predicted/expected values for unconstrained model with [LV == 2] (lv2)

fitmod_lv2 <- data.frame(
  exp(
    predict(gllvm_full_lv2_distcoast, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass))
  )
)

fitmod_lv2 <- fitmod_lv2[order(wide_data$season), ]

fitlong_lv2 <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv2), fitmod_lv2), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

fitlong_lv2 <-
  cbind(fitlong_lv2, Source = rep("LV = 2", times = nrow(fitlong_lv2)))

## Reshape raw data to the same format
yord <- spp_matrix[order(wide_data$season), ]

ylong <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv0), yord), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

ylong <-
  cbind(ylong, Source = rep("Raw data", times = nrow(ylong)))

rm("yord", "fitmod_lv0", "fitmod_lv1", "fitmod_lv2")

## Bind dataframes
df_lv0_lv1_lv2_raw <- rbind(fitlong_lv0, fitlong_lv1, fitlong_lv2, ylong)

rm("fitlong_lv0","fitlong_lv1", "fitlong_lv2", "ylong")

## Compare results through a plot

plot_comparing_lv0_lv1_lv2_raw <-
  ggplot(data = df_lv0_lv1_lv2_raw, 
         aes(x = site, y = Number, colour = Source, shape = Source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#000000", "#F8766D", "skyblue1", "#00BA38")) +
  facet_wrap(~ Species, scales = "free_y") + 
  ylab("Number") + xlab("Sample") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.1),
        legend.title = element_blank(),
        strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6))

ggsave(plot_comparing_lv0_lv1_lv2_raw,
       filename = "./results/comparing_pred-lv0-lv1-lv2-distcoast-models_with_raw-data.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)

## The plot shows that all, LV==0, LV==1 and LV==2 have *very* similar results when predicting values. 
## Therefore, we will stick with the best model according to BIC values (i.e. LV==0).

## (In a particular case, for black_billed_gull, note that LV == 2 seems to have had a better fit.)

rm("plot_comparing_lv0_lv1_lv2_raw",
   "df_lv0_lv1_lv2_raw",
   "gllvm_full_lv0_distcoast",
   "gllvm_full_lv1_distcoast",
   "gllvm_full_lv2_distcoast")

