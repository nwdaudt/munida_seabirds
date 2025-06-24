## 
## Model-based ordinations
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script runs model-based ordinations using the {gllvm} framework...................

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
  # No info on 'water_mass' preclude running the GLLVM models, so get rid of them now: this processed removed 2 voyages
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


## GLLVM, purely biological (null model) ####

### Run NULL models with 1 and 2 LV, respectively

gllvm_null_model_lv1 <-
  gllvm::gllvm(y = spp_matrix, 
               row.eff = "fixed",
               num.lv = 1, 
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

gllvm_null_model_lv2 <-
  gllvm::gllvm(y = spp_matrix, 
               row.eff = "fixed",
               num.lv = 2, 
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

### Based on BIC, choose the best model

BIC(gllvm_null_model_lv1, gllvm_null_model_lv2)
#                      df      BIC
# gllvm_null_model_lv1 412 17490.18
# gllvm_null_model_lv2 450 17349.96 ## -->> best model

# summary(gllvm_null_model_lv1)$`log-likelihood`
# summary(gllvm_null_model_lv2)$`log-likelihood`

## --->> BIC and LL suggests the model 'lv2' fits better the data

### Residual plots

# pdf(file = "./results/gllvm_null-model_lv2_residuals.pdf")
# plot(gllvm_null_model_lv2, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model
# saveRDS(gllvm_null_model_lv2,
#         file = "./results/gllvm_null-model_lv2_model.rds")

### You can load the files back instead of running the models again
# gllvm_null_model_lv2 <- readRDS("./results/gllvm_null-model_lv2_model.rds")

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

### Not great {patchwork}ing them because the legend of '_taiaroa' plot is huge. 
### So, I've put the plots together for publication in Inkscape

## Clean environment
rm(gllvm_null_model_lv1, 
   plot_null_model_watermass,
   plot_null_model_taiaroa, palette_12cols, 
   plot_null_model_season,
   df_plot_null_model_lv2)

## GLLVM, purely biological but *with voyage random effect* ("null" model) ####

gllvm_null_model_lv1_re <-
  gllvm::gllvm(y = spp_matrix, 
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1, 
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

gllvm_null_model_lv2_re <-
  gllvm::gllvm(y = spp_matrix, 
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 2, 
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

### Based on BIC, choose the best model 

BIC(gllvm_null_model_lv1_re, gllvm_null_model_lv2_re)
#                          df      BIC
# gllvm_null_model_lv1_re  80 14811.51
# gllvm_null_model_lv2_re 118 14628.15 ## -->> best model

# summary(gllvm_null_model_lv1_re)$`log-likelihood`
# summary(gllvm_null_model_lv2_re)$`log-likelihood`

## --->> BIC suggests the model 'lv2_re' fits better the data

### Residual plots

# pdf(file = "./results/gllvm_null-model-RE_lv2_residuals.pdf")
# plot(gllvm_null_model_lv2_re, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model
# saveRDS(gllvm_null_model_lv2_re,
#         file = "./results/gllvm_null-model-RE_lv2_model.rds")

### You can load the files back instead of running the models again
# gllvm_null_model_lv2_re <- readRDS("./results/gllvm_null-model-RE_lv2_model.rds")

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


rm(gllvm_null_model_lv1_re, 
   plot_null_model_watermass_re, 
   plot_null_model_season_re, 
   plot_null_model_taiaroa_re, palette_12cols,
   df_plot_null_model_lv2_re)

## GLLVM, including predictors (full model) ####

### Run models with 0, 1 and 2 LV, respectively

# ### --> using 'taiaroa_east' (categorical)
# ### [I commented all the 'taiaroa_east' section; way too many parameters]
# 
# gllvm_full_lv0_taiaroa <-
#   gllvm::gllvm(y = spp_matrix,
#                X = data.frame(season = wide_data$season,
#                               taiaroa_east = wide_data$taiaroa_east,
#                               water_mass = wide_data$water_mass),
#                formula = ~ season + taiaroa_east + water_mass,
#                studyDesign = data.frame(voyage = factor(wide_data$id)),
#                row.eff = ~ (1|voyage),
#                num.lv = 0,
#                family = "negative.binomial",
#                disp.formula = rep(1, 39),
#                method = "LA",
#                seed = 321)
# 
# gllvm_full_lv1_taiaroa <-
#   gllvm::gllvm(y = spp_matrix,
#                X = data.frame(season = wide_data$season,
#                               taiaroa_east = wide_data$taiaroa_east,
#                               water_mass = wide_data$water_mass),
#                formula = ~ season + taiaroa_east + water_mass,
#                studyDesign = data.frame(voyage = factor(wide_data$id)),
#                row.eff = ~ (1|voyage),
#                num.lv = 1,
#                family = "negative.binomial",
#                disp.formula = rep(1, 39),
#                method = "LA",
#                seed = 321)
# 
# gllvm_full_lv2_taiaroa <-
#   gllvm::gllvm(y = spp_matrix,
#                X = data.frame(season = wide_data$season,
#                               taiaroa_east = wide_data$taiaroa_east,
#                               water_mass = wide_data$water_mass),
#                formula = ~ season + taiaroa_east + water_mass,
#                studyDesign = data.frame(voyage = factor(wide_data$id)),
#                row.eff = ~ (1|voyage),
#                num.lv = 2,
#                family = "negative.binomial",
#                disp.formula = rep(1, 39),
#                method = "LA",
#                seed = 321)

### --> using 'dist_coast' (continuous)

gllvm_full_lv0_distcoast <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass),
               formula = ~ season + dist_coast + water_mass,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 0,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

gllvm_full_lv1_distcoast <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass),
               formula = ~ season + dist_coast + water_mass,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

gllvm_full_lv2_distcoast <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass),
               formula = ~ season + dist_coast + water_mass,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 2,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

### Based on Information Criteria, choose the best model

BIC(
  #gllvm_full_lv0_taiaroa, gllvm_full_lv1_taiaroa, gllvm_full_lv2_taiaroa,
  gllvm_full_lv0_distcoast, gllvm_full_lv1_distcoast, gllvm_full_lv2_distcoast)

## Note: All models using 'taiaroa' (as categorical) ------------------------ #
##       doubled the degrees of freedom, so they will not be considered ----- #

#                           df      BIC
# gllvm_full_lv0_taiaroa   665 18596.15 # not considered
# gllvm_full_lv1_taiaroa   704 18419.94 # not considered
# gllvm_full_lv2_taiaroa   742 18512.33 # not considered
# gllvm_full_lv0_distcoast 275 15510.87
# gllvm_full_lv1_distcoast 314 15209.21 ## -->> best model
# gllvm_full_lv2_distcoast 352 15287.40

# summary(gllvm_full_lv1_distcoast)$`log-likelihood` 
# summary(gllvm_full_lv2_distcoast)$`log-likelihood` 

## --->> BIC suggests the model '*_lv1_distcoast' fits best the data

### Residual plots 
# pdf(file = "./results/gllvm_full-model-distcoast_lv1_residuals.pdf")
# plot(gllvm_full_lv1_distcoast, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects
# saveRDS(gllvm_full_lv1_distcoast,
#         file = "./results/gllvm_full-model-distcoast_lv1_model.rds")

### You can load the files back instead of running the models again
# gllvm_full_lv1_distcoast <- readRDS("./results/gllvm_full-model-distcoast_lv1_model.rds")

### Clear environment, as these models will not be used further
rm("gllvm_full_lv0_taiaroa", 
   "gllvm_full_lv1_taiaroa", 
   "gllvm_full_lv2_taiaroa"#, 
   # "gllvm_full_lv0_distcoast", 
   # "gllvm_full_lv2_distcoast" ## See 'Compare predictions...' section below 
)

## Compare predictions between GLLVMs accounting for predictors (full models), with and without LVs and raw data ####

## The idea of this section is compare the effects of including Latent Variables (LV) in the models.
## Although BIC values suggests the model with 1 LV fits the best, we wanted to check this further.

## So, to compare the influence of LVs, we selected the full 'distcoast' models that used LV == 0, 1, and 2.
## We then get the predicted/expected values for each model, for each species, and plot them together to verify
## any possible (dis)agreement between models. 

## Models being evaluated:
# gllvm_full_lv0_distcoast
# gllvm_full_lv1_distcoast
# gllvm_full_lv2_distcoast

## Get predicted/expected values for full model without LVs [LV == 0] (lv0)

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

## Get predicted/expected values for full model with [LV == 1] (lv1)

fitmod_lv1 <- data.frame(
  exp(
    predict(gllvm_full_lv1_distcoast, 
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

## Get predicted/expected values for full model with [LV == 2] (lv2)

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
       filename = "./results/comparing_pred-lv0-lv1-lv2-full-distcoast-models_with_raw-data.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)

## Even though LL suggests the model 'lv2' fits the data better, the plot shows that all
## models have *very* similar results when predicting values - although note LV==2 seem to 
## capture better the variability. Nonetheless, given the range of values (many spp with 
## zero to 'few' [<5]), these variability would be expected under a NB distribution and therefore
## are basically insignificant. Therefore, we will stick with the best model according to 
## BIC values (i.e. LV==1), as it is the most parsimonious model.

rm(plot_comparing_lv0_lv1_lv2_raw,
   df_lv0_lv1_lv2_raw,
   gllvm_full_lv0_distcoast,
   gllvm_full_lv2_distcoast)

## Covariate selection in model 'gllvm_full_lv0_distcoast' ####

## 'Full' model (with the three predictors): gllvm_full_lv0_distcoast

# season + water_mass
gllvm_full_lv1_distcoast_season.watermass <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(season = wide_data$season,
                              water_mass = wide_data$water_mass),
               formula = ~ season + water_mass,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

# dist_coast + water_mass
gllvm_full_lv1_distcoast_distcoast.watermass <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(dist_coast = wide_data$dist_coast,
                              water_mass = wide_data$water_mass),
               formula = ~ dist_coast + water_mass,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

# dist_coast + season
gllvm_full_lv1_distcoast_distcoast.season <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast),
               formula = ~ season + dist_coast,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

# Only season
gllvm_full_lv1_distcoast_season <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(season = wide_data$season),
               formula = ~ season,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

# Only dist_coast
gllvm_full_lv1_distcoast_distcoast <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(dist_coast = wide_data$dist_coast),
               formula = ~ dist_coast,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

# Only water_mass
gllvm_full_lv1_distcoast_watermass <-
  gllvm::gllvm(y = spp_matrix,
               X = data.frame(water_mass = wide_data$water_mass),
               formula = ~ water_mass,
               studyDesign = data.frame(voyage = factor(wide_data$id)),
               row.eff = ~ (1|voyage),
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 39),
               method = "LA",
               seed = 321)

### Based on BIC, choose the best model

BIC(gllvm_full_lv1_distcoast,
    gllvm_full_lv1_distcoast_season.watermass,
    gllvm_full_lv1_distcoast_distcoast.watermass,
    gllvm_full_lv1_distcoast_distcoast.season,
    gllvm_full_lv1_distcoast_season,
    gllvm_full_lv1_distcoast_distcoast,
    gllvm_full_lv1_distcoast_watermass)

#                                               df      BIC
# gllvm_full_lv1_distcoast                     314 15209.21
# gllvm_full_lv1_distcoast_season.watermass    275 15175.35
# gllvm_full_lv1_distcoast_distcoast.watermass 197 15205.31
# gllvm_full_lv1_distcoast_distcoast.season    236 14609.29 ## -->> best model
# gllvm_full_lv1_distcoast_season              197 14793.81
# gllvm_full_lv1_distcoast_distcoast           119 14607.23 ## -->> best model
# gllvm_full_lv1_distcoast_watermass           158 15094.94

# summary(gllvm_full_lv1_distcoast)$`log-likelihood`
# summary(gllvm_full_lv1_distcoast_distcoast.season)$`log-likelihood`
# summary(gllvm_full_lv1_distcoast_distcoast)$`log-likelihood`

## --->> BIC suggests the models '*_lv1_distcoast_distcoast.season' and '*_lv1_distcoast_distcoast'
##       fit equally well the data

### Clear environment, as these models will not be used further
rm(#gllvm_full_lv1_distcoast,
  gllvm_full_lv1_distcoast_season.watermass,
  gllvm_full_lv1_distcoast_distcoast.watermass,
  gllvm_full_lv1_distcoast_season,
  #gllvm_full_lv1_distcoast_distcoast,
  gllvm_full_lv1_distcoast_watermass
)

## Compare predictions between GLLVMs 'distcoast', 'distcoast.season' and raw data ####

## BIC values suggests that both models fit equally better the data.

## I followed the same idea from the previous 'Compare predictions' section.
## We get the predicted/expected values for each model, for each species, and plot them all together to verify
## any possible (dis)agreement between models. 

## Models being evaluated
# gllvm_full_lv1_distcoast_distcoast
# gllvm_full_lv1_distcoast_distcoast.season

## Get predicted values for 'gllvm_full_lv1_distcoast_distcoast'

fitmod_lv1_dc <- data.frame(
  exp(
    predict(gllvm_full_lv1_distcoast_distcoast, 
            newX = data.frame(dist_coast = wide_data$dist_coast))
  )
)

fitlong_lv1_dc <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv1_dc), fitmod_lv1_dc), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

fitlong_lv1_dc <-
  cbind(fitlong_lv1_dc, Source = rep("~distcoast", times = nrow(fitlong_lv1_dc)))

## Get predicted values for 'gllvm_full_lv1_distcoast_distcoast.season'

fitmod_lv1_dc_season <- data.frame(
  exp(
    predict(gllvm_full_lv1_distcoast_distcoast.season, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast))
  )
)

fitlong_lv1_dc_season <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv1_dc_season), fitmod_lv1_dc_season), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

fitlong_lv1_dc_season <-
  cbind(fitlong_lv1_dc_season, Source = rep("~distcoast+season", times = nrow(fitlong_lv1_dc_season)))

## Reshape raw data to the same format

ylong <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv1_dc), spp_matrix), 
                key = "Species", value = "Number", 
                black_backed_gull:yellow_eye_penguin)

ylong <-
  cbind(ylong, Source = rep("Raw data", times = nrow(ylong)))

rm("fitmod_lv1_dc", "fitmod_lv1_dc_season")

## Bind dataframes
df_plot_compare_models <- rbind(fitlong_lv1_dc, fitlong_lv1_dc_season, ylong)

rm("fitlong_lv1_dc", "fitlong_lv1_dc_season", "ylong")

## Compare results through a plot

plot_comparing_selected_models <-
  ggplot(data = df_plot_compare_models, 
         aes(x = site, y = Number, colour = Source, shape = Source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c(#"#000000", 
    "#F8766D", "skyblue1", "#00BA38")) +
  facet_wrap(~ Species, scales = "free_y") + 
  ylab("Number") + xlab("Sample") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.1),
        legend.title = element_blank(),
        strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6))

ggsave(plot_comparing_selected_models,
       filename = "./results/comparing_pred-lv1-models-with-raw-data.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)

## The plot shows that both models *very* similar results when predicting values.

rm(plot_comparing_selected_models)

### ----------- Another layer of checks ------------------------------------ ###

### Overall raw data vs predicted
pdf(file = "./results/gllvm_best-models-lv1_check_scatterplot-raw-vs-predicted.pdf",
    width = 7, height = 4)
par(mfrow = c(1,2))

plot(df_plot_compare_models[df_plot_compare_models$Source == "Raw data", ]$Number,
     df_plot_compare_models[df_plot_compare_models$Source == "~distcoast", ]$Number,
     xlim = c(0,1200), ylim = c(0,1000),
     xlab = "Raw data", ylab = "Model prediction: ~distcoast")
lines(x = c(0,1200), y = c(0,1200), col = "red")

plot(df_plot_compare_models[df_plot_compare_models$Source == "Raw data", ]$Number,
     df_plot_compare_models[df_plot_compare_models$Source == "~distcoast+season", ]$Number,
     ## There are four predicted values of '38796' for black-billed gulls, these were also removed
     xlim = c(0,1200), ylim = c(0,1600), 
     xlab = "Raw data", ylab = "Model prediction: ~distcoast+season")
lines(x = c(0,1600), y = c(0,1600), col = "red")
dev.off()

### `coefplot` for both models
pdf(file = "./results/gllvm_best-models-lv1_check_coefplots.pdf",
    width = 7, height = 7)
par(mfrow = c(1,2))

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast.season,
                which.Xcoef = c(4),
                order = FALSE,
                main = "~dist+season")

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast,
                which.Xcoef = c(1),
                order = FALSE,
                main = "~dist")
dev.off()

### `coefplot` for both models, adjusting x-lim to look closer at coefficients
pdf(file = "./results/gllvm_best-models-lv1_check_coefplots-adjusted.pdf",
    width = 7, height = 7)
par(mfrow = c(1,2))

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast.season,
                which.Xcoef = c(4),
                order = FALSE,
                main = "~dist+season",
                xlim.list = list(c(-1.5, 0.5)))

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast,
                which.Xcoef = c(1),
                order = FALSE,
                main = "~dist",
                xlim.list = list(c(-1.5, 0.5)))
dev.off()

rm(df_plot_compare_models)

### ------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------ ###

### I've chosen model '*_lv1_distcoast_distcoast.season', given their predictions 
### were very similar and the estimated 'dist_coast' are also very similar. 
### This model, also helps us to quantitatively show the effects of seasonality 
### in addition to 'dist_coast' only.

### Residual plots (only for the 'best model')
# pdf(file = "./results/gllvm_best-model_lv1_distcoast-season_residuals.pdf")
# plot(gllvm_full_lv1_distcoast_distcoast.season, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects
# saveRDS(gllvm_full_lv1_distcoast_distcoast.season,
#         file = "./results/gllvm_best-model_lv1_distcoast-season_model.rds")

# rm(gllvm_full_lv1_distcoast_distcoast)

## Coefficient plots for the chosen 'best model' (~ dist_coast + season; LV == 1) ####

# gllvm_full_lv1_distcoast_distcoast.season <- 
#   readRDS("./results/gllvm_best-model_lv1_distcoast-season_model.rds")

# Adjust species name for plot -- a bit of a manual job...
gllvm_spp <- rownames(gllvm_full_lv1_distcoast_distcoast.season$params$Xcoef)
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
gllvm_spp[35] <- "Common diving petrel"
gllvm_spp[36] <- "Black-billed gull"
gllvm_spp[37] <- "Cook's petrel"
gllvm_spp[39] <- "Yellow-eyed penguin"

# I was not cleaver enough to find out how to automatically get the ordered vector,
# so I specified it by hand (after running the plot once to check the order)
gllvm_spp_ordered <- c(
  gllvm_spp[22], gllvm_spp[26], gllvm_spp[24], gllvm_spp[31], gllvm_spp[33], 
  gllvm_spp[20], gllvm_spp[27], gllvm_spp[17], gllvm_spp[16], gllvm_spp[30], 
  gllvm_spp[9], gllvm_spp[14], gllvm_spp[15], gllvm_spp[18], gllvm_spp[23], 
  gllvm_spp[32], gllvm_spp[28], gllvm_spp[7], gllvm_spp[34], gllvm_spp[10], 
  gllvm_spp[3], gllvm_spp[13], gllvm_spp[29], gllvm_spp[12], gllvm_spp[35], 
  gllvm_spp[37], gllvm_spp[8], gllvm_spp[6], gllvm_spp[38], gllvm_spp[21], 
  gllvm_spp[4], gllvm_spp[11], gllvm_spp[5], gllvm_spp[1], gllvm_spp[19], 
  gllvm_spp[39], gllvm_spp[2], gllvm_spp[36], gllvm_spp[25]
)

At.y <- seq(1, length(gllvm_spp))

# coefplot: 'seasons' (note: summer == intercept)
pdf(file = "./results/gllvm_best-model_lv1_distcoast-season_coefplot-season.pdf",
    width = 10, height = 8)
par(mfrow = c(1, 3), 
    oma = c(1, 12, 1, 1),
    cex = 1)

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast.season,
                which.Xcoef = c(1),
                order = FALSE,
                cex.ylab = 0.0001,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1),
                xlim.list = list(c(-30, 55)))
axis(side = 2, at = At.y, labels = gllvm_spp, las = 1)
title(xlab = "Autumn", cex.lab = 1.1)

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast.season,
                which.Xcoef = c(2),
                order = FALSE,
                y.label = FALSE,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1),
                xlim.list = list(c(-40, 55)))
title(xlab = "Winter", cex.lab = 1.1)

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast.season,
                which.Xcoef = c(3),
                order = FALSE,
                y.label = FALSE,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1),
                xlim.list = list(c(-25, 25)))
title(xlab = "Spring", cex.lab = 1.1)

dev.off()


# coefplot: 'dist_coast'
pdf(file = "./results/gllvm_best-model_lv1_distcoast-season_coefplot-distcoast.pdf",
    width = 5.5, height = 8)
par(mfrow = c(1, 1), 
    mar = c(0.1, 0.1, 0.1, 0.1), 
    oma = c(1, 6.5, 1, 1))

gllvm::coefplot(gllvm_full_lv1_distcoast_distcoast.season,
                which.Xcoef = c(4),
                order = TRUE,
                cex.ylab = 0.0001,
                cex.lab = 0.0001)
axis(side = 2, at = At.y, labels = rev(gllvm_spp_ordered), las = 1)
title(xlab = "Distance from coast", cex.lab = 1.1)

dev.off()

# par(mfcol = c(1,1))
rm(gllvm_spp, gllvm_spp_ordered, At.y)
