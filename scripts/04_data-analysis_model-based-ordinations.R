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

## Prep for modelling ####

spp_cols_all <- unique(data$species)
sp_cols_only <- spp_cols_all[! grepl("unknown", spp_cols_all)]

### First, transform it from long to wide format
wide_data <- 
  data %>% 
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0) %>% 
  ## To match-up with the Temperature/Salinity data, we will only model the way back ("westward")
  dplyr::filter(direction == "westward")

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

## Unconstrained ordination (purely biological) ####

### Run NULL models with 1 and 2 LV, respectively

unconstrained_biol_model_lv1 <-
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 1, 
               family = "negative.binomial",
               seed = 321)

unconstrained_biol_model_lv2 <-
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 2, 
               family = "negative.binomial",
               seed = 321)

### Based on the BIC/AIC, choose the best model

BIC(unconstrained_biol_model_lv1, unconstrained_biol_model_lv2)

#                               df      BIC
# unconstrained_biol_model_lv1 117 15157.04 ## best model
# unconstrained_biol_model_lv2 155 15672.03

AIC(unconstrained_biol_model_lv1, unconstrained_biol_model_lv2)

#                               df      AIC
# unconstrained_biol_model_lv1 117 14703.02 ## best model
# unconstrained_biol_model_lv2 155 15070.55

### Residual plots

# pdf(file = "./results/gllvm_unconstrained_biol_lv1_residuals.pdf")
# plot(unconstrained_biol_model_lv1, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_biol_lv2_residuals.pdf")
# plot(unconstrained_biol_model_lv2, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects

# saveRDS(unconstrained_biol_model_lv1,
#         file = "./results/gllvm_unconstrained_biol_lv1_model.rds")

# saveRDS(unconstrained_biol_model_lv2,
#         file = "./results/gllvm_unconstrained_biol_lv2_model.rds")

### You can load the files back instead of running the models again

# unconstrained_biol_model_lv1 <- readRDS("./results/gllvm_unconstrained_biol_lv1_model.rds")
# # unconstrained_biol_model_lv2 <- readRDS("./results/gllvm_unconstrained_biol_lv2_model.rds")

### Get LV values and arrange it in a dataframe to plot

df_plot_lv1_unconstr_biol_model <-
  cbind(wide_data,
        as.data.frame(gllvm::getLV.gllvm(unconstrained_biol_model_lv1)))

## Plot colour-coded by 'water_mass' ----------------------------------------- #

plot_unconstrained_biol_model_watermass <- 
  ggplot(
    data = df_plot_lv1_unconstr_biol_model[!is.na(df_plot_lv1_unconstr_biol_model$water_mass),],
    aes(x = V1, 
        y = rep(0, nrow(df_plot_lv1_unconstr_biol_model[!is.na(df_plot_lv1_unconstr_biol_model$water_mass),])), 
        color = water_mass)) +
  geom_point(alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2") +
  xlab("Latent Variable 1") + ylab("") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

plot_unconstrained_biol_model_watermass <- 
  ggExtra::ggMarginal(plot_unconstrained_biol_model_watermass,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)

ggsave(plot_unconstrained_biol_model_watermass,
       filename = "./results/gllvm_unconstrained_biol_lv1_biplot_watermass.pdf",
       height = 9, width = 12, units = "cm", dpi = 300)

rm("plot_unconstrained_biol_model_watermass")

## Plot colour-coded by 'season' --------------------------------------------- #

plot_unconstrained_biol_model_season <- 
  ggplot(data = df_plot_lv1_unconstr_biol_model, 
         aes(x = V1, 
             y = rep(0, times = nrow(df_plot_lv1_unconstr_biol_model)),
             color = season)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = c("summer" = "#4E79A7", "autumn" = "#F28E2B", 
                                "winter" = "#E15759", "spring" = "#76B7B2")) +
  xlab("Latent Variable 1") + ylab("") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

plot_unconstrained_biol_model_season <- 
  ggExtra::ggMarginal(plot_unconstrained_biol_model_season,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)

ggsave(plot_unconstrained_biol_model_season,
       filename = "./results/gllvm_unconstrained_biol_lv1_biplot_season.pdf",
       height = 9, width = 12, units = "cm", dpi = 300)

rm("plot_unconstrained_biol_model_season")

## Plot colour-coded by 'taiaroa_head' --------------------------------------- #

# Specify a 12-colour palette
palette_12cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "BrBG"))(12)

plot_unconstrained_biol_model_taiaroa <- 
  ggplot(data = df_plot_lv1_unconstr_biol_model, 
         aes(x = V1, 
             y = rep(0, times = nrow(df_plot_lv1_unconstr_biol_model)), 
             color = taiaroa_east)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = palette_12cols) +
  xlab("Latent Variable 1") + ylab("") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "left")

plot_unconstrained_biol_model_taiaroa <- 
  ggExtra::ggMarginal(plot_unconstrained_biol_model_taiaroa,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)

ggsave(plot_unconstrained_biol_model_taiaroa,
       filename = "./results/gllvm_unconstrained_biol_lv1_biplot_taiaroa.pdf",
       height = 10, width = 17, units = "cm", dpi = 300)

rm("plot_unconstrained_biol_model_taiaroa", "palette_12cols")

## Unconstrained ordination (including predictors) ####

### Run models with 0, 1 and 2 LV, respectively

## --> using 'taiaroa_east' (categorical)

unconstrained_pred_model_lv0_taiaroaeast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season + taiaroa_east,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

unconstrained_pred_model_lv1_taiaroaeast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season + taiaroa_east,
               num.lv = 1, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

unconstrained_pred_model_lv2_taiaroaeast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season + taiaroa_east,
               num.lv = 2, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

## --> using 'dist_coast' (continuous)

unconstrained_pred_model_lv0_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              voyage = wide_data$id),
               formula = ~ season + dist_coast,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

unconstrained_pred_model_lv1_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              voyage = wide_data$id),
               formula = ~ season + dist_coast,
               num.lv = 1, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

unconstrained_pred_model_lv2_distcoast <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              voyage = wide_data$id),
               formula = ~ season + dist_coast,
               num.lv = 2, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

### Based on the BIC/AIC, choose the best model

BIC(unconstrained_pred_model_lv0_taiaroaeast, unconstrained_pred_model_lv0_distcoast,
    unconstrained_pred_model_lv1_taiaroaeast, unconstrained_pred_model_lv1_distcoast,
    unconstrained_pred_model_lv2_taiaroaeast, unconstrained_pred_model_lv2_distcoast)

#                                           df      BIC
# unconstrained_pred_model_lv0_taiaroaeast 625 16287.31
# unconstrained_pred_model_lv0_distcoast   235 14570.37 ## best model
# unconstrained_pred_model_lv1_taiaroaeast 664 16516.65
# unconstrained_pred_model_lv1_distcoast   274 14799.72
# unconstrained_pred_model_lv2_taiaroaeast 702 16740.11
# unconstrained_pred_model_lv2_distcoast   312 14934.43

AIC(unconstrained_pred_model_lv0_taiaroaeast, unconstrained_pred_model_lv0_distcoast,
    unconstrained_pred_model_lv1_taiaroaeast, unconstrained_pred_model_lv1_distcoast,
    unconstrained_pred_model_lv2_taiaroaeast, unconstrained_pred_model_lv2_distcoast)

#                                           df      AIC
# unconstrained_pred_model_lv0_taiaroaeast 625 13861.98
# unconstrained_pred_model_lv0_distcoast   235 13658.45 ## best model
# unconstrained_pred_model_lv1_taiaroaeast 664 13939.98
# unconstrained_pred_model_lv1_distcoast   274 13736.45
# unconstrained_pred_model_lv2_taiaroaeast 702 14015.98
# unconstrained_pred_model_lv2_distcoast   312 13723.71

### Residual plots 

# pdf(file = "./results/gllvm_unconstrained_pred_lv0_taiaroaeast_residuals.pdf")
# plot(unconstrained_pred_model_lv0_taiaroaeast, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv1_taiaroaeast_residuals.pdf")
# plot(unconstrained_pred_model_lv1_taiaroaeast, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv2_taiaroaeast_residuals.pdf")
# plot(unconstrained_pred_model_lv2_taiaroaeast, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv0_distcoast_residuals.pdf")
# plot(unconstrained_pred_model_lv0_distcoast, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv1_distcoast_residuals.pdf")
# plot(unconstrained_pred_model_lv1_distcoast, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv2_distcoast_residuals.pdf")
# plot(unconstrained_pred_model_lv2_distcoast, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects

# saveRDS(unconstrained_pred_model_lv0_taiaroaeast,
#         file = "./results/gllvm_unconstrained_pred_lv0_taiaroaeast_model.rds")

# saveRDS(unconstrained_pred_model_lv1_taiaroaeast,
#         file = "./results/gllvm_unconstrained_pred_lv1_taiaroaeast_model.rds")

# saveRDS(unconstrained_pred_model_lv2_taiaroaeast,
#         file = "./results/gllvm_unconstrained_pred_lv2_taiaroaeast_model.rds")

# saveRDS(unconstrained_pred_model_lv0_distcoast,
#         file = "./results/gllvm_unconstrained_pred_lv0_distcoast_model.rds")

# saveRDS(unconstrained_pred_model_lv1_distcoast,
#         file = "./results/gllvm_unconstrained_pred_lv1_distcoast_model.rds")

# saveRDS(unconstrained_pred_model_lv2_distcoast,
#         file = "./results/gllvm_unconstrained_pred_lv2_distcoast_model.rds")

### You can load the files back instead of running the models again

# unconstrained_pred_model_lv0_taiaroaeast <- readRDS("./results/gllvm_unconstrained_pred_lv0_taiaroaeast_model.rds")
# unconstrained_pred_model_lv1_taiaroaeast <- readRDS("./results/gllvm_unconstrained_pred_lv1_taiaroaeast_model.rds")
# unconstrained_pred_model_lv2_taiaroaeast <- readRDS("./results/gllvm_unconstrained_pred_lv2_taiaroaeast_model.rds")
# unconstrained_pred_model_lv0_distcoast <- readRDS("./results/gllvm_unconstrained_pred_lv0_distcoast_model.rds")
# unconstrained_pred_model_lv1_distcoast <- readRDS("./results/gllvm_unconstrained_pred_lv1_distcoast_model.rds")
# unconstrained_pred_model_lv2_distcoast <- readRDS("./results/gllvm_unconstrained_pred_lv2_distcoast_model.rds")

### Clear environment, as these models will not be used further
rm("unconstrained_pred_model_lv0_taiaroaeast",
   "unconstrained_pred_model_lv1_taiaroaeast", "unconstrained_pred_model_lv1_distcoast",
   "unconstrained_pred_model_lv2_taiaroaeast", "unconstrained_pred_model_lv2_distcoast")

## Covariate selection in model 'pred_lv0_distcoast' #### 

# Only season
unconstrained_pred_model_lv0_season_only <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              voyage = wide_data$id),
               formula = ~ season,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# Only dist_coast
unconstrained_pred_model_lv0_distcoast_only <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast,
                              voyage = wide_data$id),
               formula = ~ dist_coast,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

### Based on the BIC/AIC, choose the best model

BIC(unconstrained_pred_model_lv0_distcoast, 
    unconstrained_pred_model_lv0_season_only, 
    unconstrained_pred_model_lv0_distcoast_only)

#                                              df      BIC
# unconstrained_pred_model_lv0_distcoast      235 14570.37 ## best model
# unconstrained_pred_model_lv0_season_only    196 15240.57
# unconstrained_pred_model_lv0_distcoast_only 118 15117.25

AIC(unconstrained_pred_model_lv0_distcoast, 
    unconstrained_pred_model_lv0_season_only, 
    unconstrained_pred_model_lv0_distcoast_only)

#                                              df      AIC
# unconstrained_pred_model_lv0_distcoast      235 13658.45 ## best model
# unconstrained_pred_model_lv0_season_only    196 14479.99
# unconstrained_pred_model_lv0_distcoast_only 118 14659.34

### Residual plots

# pdf(file = "./results/gllvm_unconstrained_pred_lv0_season-only_residuals.pdf")
# plot(unconstrained_pred_model_lv0_season_only, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv0_distcoast-only_residuals.pdf")
# plot(unconstrained_pred_model_lv0_distcoast_only, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects

# saveRDS(unconstrained_pred_model_lv0_season_only,
#         file = "./results/gllvm_unconstrained_pred_lv0_season-only_model.rds")

# saveRDS(unconstrained_pred_model_lv0_distcoast_only,
#         file = "./results/gllvm_unconstrained_pred_lv0_distcoast-only_model.rds")

### Clear environment, as these models will not be used further
rm("unconstrained_pred_model_lv0_season_only", 
   "unconstrained_pred_model_lv0_distcoast_only")

## Compare predictions between GLLVMs accounting for predictors, with(out) LVs, and raw data ####

## The idea of this section is compare the effects of including Latent Variables (LV) in the models.
## Although BIC and AIC values suggests the model without LVs is the best, we want to check this further.
## The previous section showed that the full model had lowest BIC and AIC values compared to models with 
## single predictors (i.e. 'distcoast-only' or 'season-only'). 
## So, to compare the influence of LVs, we selected the full 'distcoast' models that used LV == 0, 1, and 2.
## We then get the expected values for each model, for each species, and plot them all together to verify
## any possible (dis)agreement between models. 

# unconstrained_pred_model_lv0_distcoast <- readRDS("./results/gllvm_unconstrained_pred_lv0_distcoast_model.rds")
# unconstrained_pred_model_lv1_distcoast <- readRDS("./results/gllvm_unconstrained_pred_lv1_distcoast_model.rds")
# unconstrained_pred_model_lv2_distcoast <- readRDS("./results/gllvm_unconstrained_pred_lv2_distcoast_model.rds")

## Get predicted/expected values for unconstrained model without LVs [LV == 0] (lv0)

fitmod_lv0 <- data.frame(
  exp(
    predict(unconstrained_pred_model_lv0_distcoast, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast))
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
    predict(unconstrained_pred_model_lv1_distcoast, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast))
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
    predict(unconstrained_pred_model_lv2_distcoast, 
            newX = data.frame(season = wide_data$season,
                              dist_coast = wide_data$dist_coast))
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

## The plot shows that all, LV==0, LV==1 and LV==2 have very similar results when predicting values. 
## Therefore, we will stick with the best model according to BIC/AIC values (i.e. LV==0).

## (Note, in a particular case, for black_billed_gull, LV == 2 seems to have had a better fit.)

rm("plot_comparing_lv0_lv1_lv2_raw",
   "df_lv0_lv1_lv2_raw",
   "unconstrained_pred_model_lv1_distcoast",
   "unconstrained_pred_model_lv2_distcoast")

## Coefficient plots for the best model (~ dist_coast + season) ####

# Adjust species name for plot -- a bit of a manual job...
gllvm_spp <- rownames(unconstrained_pred_model_lv0_distcoast$params$Xcoef)
gllvm_spp <- snakecase::to_sentence_case(gllvm_spp)
gllvm_spp[1] <- "Black-backed gull"
gllvm_spp[2] <- "Red-billed gull"
gllvm_spp[3] <- "White-capped mollymawk"
gllvm_spp[4] <- "White-fronted tern"
gllvm_spp[8] <- "Buller's mollymawk"
gllvm_spp[9] <- "White-chinned petrel"
gllvm_spp[10] <- "Buller's shearwater"
gllvm_spp[11] <- "Hutton/Fluttering shearwater"
gllvm_spp[13] <- "Salvin's mollymawk"
gllvm_spp[14] <- "Black-browed mollymawk"
gllvm_spp[16] <- "Black-bellied storm petrel"
gllvm_spp[20] <- "Light-mantled sooty albatross"
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
# so I specified it by hand after running the plot once
gllvm_spp_ordered <- c(
  gllvm_spp[25], gllvm_spp[36], gllvm_spp[2], gllvm_spp[39], gllvm_spp[19], 
  gllvm_spp[1], gllvm_spp[11], gllvm_spp[5], gllvm_spp[4], gllvm_spp[21], 
  gllvm_spp[38], gllvm_spp[8], gllvm_spp[6], gllvm_spp[37], gllvm_spp[35], 
  gllvm_spp[12], gllvm_spp[3], gllvm_spp[13], gllvm_spp[29], gllvm_spp[10], 
  gllvm_spp[34], gllvm_spp[7], gllvm_spp[28], gllvm_spp[15], gllvm_spp[32], 
  gllvm_spp[23], gllvm_spp[18], gllvm_spp[14], gllvm_spp[9], gllvm_spp[16], 
  gllvm_spp[30], gllvm_spp[17], gllvm_spp[20], gllvm_spp[27], gllvm_spp[33], 
  gllvm_spp[31], gllvm_spp[24], gllvm_spp[26], gllvm_spp[22]
)


# 'seasons' (summer == intercept)
pdf(file = "./results/gllvm_unconstrained_pred_lv0_distcoast_coefplot-season.pdf",
    width = 10, height = 8)
par(mfrow = c(1, 3), 
    oma = c(1, 12, 1, 1),
    cex = 1)

gllvm::coefplot(unconstrained_pred_model_lv0_distcoast,
                which.Xcoef = c(1),
                order = FALSE,
                cex.ylab = 0.0001,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1))
axis(side = 2, at = At.y, labels = gllvm_spp, las = 1)
title(xlab = "Autumn", cex.lab = 1.1)

gllvm::coefplot(unconstrained_pred_model_lv0_distcoast,
                which.Xcoef = c(2),
                order = FALSE,
                y.label = FALSE,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1))
title(xlab = "Winter", cex.lab = 1.1)

gllvm::coefplot(unconstrained_pred_model_lv0_distcoast,
                which.Xcoef = c(3),
                order = FALSE,
                y.label = FALSE,
                cex.lab = 0.0001, 
                mar = c(4,1,2,1))
title(xlab = "Spring", cex.lab = 1.1)

dev.off()


# 'dist_coast'

pdf(file = "./results/gllvm_unconstrained_pred_lv0_distcoast_coefplot-distcoast.pdf",
    width = 5.5, height = 8)
par(mfrow = c(1, 1), 
    mar = c(0.1, 0.1, 0.1, 0.1), 
    oma = c(1, 6.5, 1, 1))

gllvm::coefplot(unconstrained_pred_model_lv0_distcoast,
                which.Xcoef = c(4),
                order = TRUE,
                cex.ylab = 0.0001,
                cex.lab = 0.0001)
axis(side = 2, at = At.y, labels = gllvm_spp_ordered, las = 1)
title(xlab = "Distance from coast", cex.lab = 1.1)

dev.off()

# par(mfcol = c(1,1))
rm("gllvm_spp", "gllvm_spp_ordered", "At.y")
