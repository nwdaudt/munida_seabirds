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

### Run models with 1 and 2 LV, respectively
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

### Based on the BIC/AIC, choose the best model -- LV == 1
# BIC(unconstrained_biol_model_lv1, unconstrained_biol_model_lv2)
#                               df      BIC
# unconstrained_biol_model_lv1 117 15157.04
# unconstrained_biol_model_lv2 155 15672.03

# AIC(unconstrained_biol_model_lv1, unconstrained_biol_model_lv2)
#                               df      AIC
# unconstrained_biol_model_lv1 117 14703.02
# unconstrained_biol_model_lv2 155 15070.55

### Residuals -- look good for both models, but LV2 slightly better
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

# df_plot_lv2_unconstr_biol_model <-
#   cbind(wide_data, 
#         as.data.frame(gllvm::getLV.gllvm(unconstrained_biol_model_lv2)))

## Plot colour-coded by 'water_mass' ----------------------------------------- #

plot_unconstrained_biol_model_watermass <- 
  ggplot(
    # data = df_plot_lv2_unconstr_biol_model[!is.na(df_plot_lv2_unconstr_biol_model$water_mass),],
    # aes(x = LV1, y = LV2, color = water_mass)) +
  ## Plot of LV1 model, but I'll likely not use this...
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

rm("plot_unconstrained_biol_model_taiaroa")

## Unconstrained ordination (including predictors) ####

### Run models with 0, 1 and 2 LV, respectively
unconstrained_pred_model_lv0 <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season + taiaroa_east,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# summary(unconstrained_pred_model_lv0)

unconstrained_pred_model_lv1 <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season + taiaroa_east,
               num.lv = 1, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

unconstrained_pred_model_lv2 <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season + taiaroa_east,
               num.lv = 2, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

### Based on the BIC/AIC, choose the best model -- LV == 0
# BIC(unconstrained_pred_model_lv0, unconstrained_pred_model_lv1, unconstrained_pred_model_lv2)
#                               df      BIC
# unconstrained_pred_model_lv0 625 16287.31
# unconstrained_pred_model_lv1 664 16516.65
# unconstrained_pred_model_lv2 702 16740.11

# AIC(unconstrained_pred_model_lv0, unconstrained_pred_model_lv1, unconstrained_pred_model_lv2)
#                               df      AIC
# unconstrained_pred_model_lv0 625 13861.98
# unconstrained_pred_model_lv1 664 13939.98
# unconstrained_pred_model_lv2 702 14015.98

### Residuals -- look good for LV1 and LV2 models, but LV1 slightly better; LV0 terrible
# pdf(file = "./results/gllvm_unconstrained_pred_lv0_residuals.pdf")
# plot(unconstrained_pred_model_lv0, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv1_residuals.pdf")
# plot(unconstrained_pred_model_lv1, which = 1:4, mfrow = c(2,2))
# dev.off()

# pdf(file = "./results/gllvm_unconstrained_pred_lv2_residuals.pdf")
# plot(unconstrained_pred_model_lv2, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model objects
# saveRDS(unconstrained_pred_model_lv0,
#         file = "./results/gllvm_unconstrained_pred_lv0_model.rds")

# saveRDS(unconstrained_pred_model_lv1,
#         file = "./results/gllvm_unconstrained_pred_lv1_model.rds")

# saveRDS(unconstrained_pred_model_lv2,
#         file = "./results/gllvm_unconstrained_pred_lv2_model.rds")

### You can load the files back instead of running the models again
# unconstrained_pred_model_lv0 <- readRDS("./results/gllvm_unconstrained_pred_lv0_model.rds")
# # unconstrained_pred_model_lv1 <- readRDS("./results/gllvm_unconstrained_pred_lv1_model.rds")
# # unconstrained_pred_model_lv2 <- readRDS("./results/gllvm_unconstrained_pred_lv2_model.rds")

## Comparing predictions between GLLVMs accounting for predictors, with(out) LVs, and raw data ####

## Get predicted/expected values for unconstrained model without LVs [LV == 0] (lv0)

fitmod_lv0 <- data.frame(
  exp(
    predict(unconstrained_pred_model_lv0, newX = data.frame(season = wide_data$season,
                                                            taiaroa_east = wide_data$taiaroa_east))
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
    predict(unconstrained_pred_model_lv1, newX = data.frame(season = wide_data$season,
                                                            taiaroa_east = wide_data$taiaroa_east))
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
    predict(unconstrained_pred_model_lv2, newX = data.frame(season = wide_data$season,
                                                            taiaroa_east = wide_data$taiaroa_east))
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
       filename = "./results/comparing_pred_lv0_lv1_lv2_raw.pdf",
       height = 25, width = 40, units = "cm", dpi = 300)

rm("plot_comparing_lv0_lv1_lv2_raw")

## >> The plot shows that all, LV ==0, LV == 1 and LV == 2 have very similar 
## >> results when predicting values.

## Covariate selection in model 'pred_lv0' [??] #### 

# Only season
unconstrained_pred_model_lv0_season <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ season,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

# Only taiaroa_east
unconstrained_pred_model_lv0_taiaroa <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = wide_data$season,
                              taiaroa_east = wide_data$taiaroa_east,
                              voyage = wide_data$id),
               formula = ~ taiaroa_east,
               num.lv = 0, 
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 321)

### BIC/AIC
BIC(unconstrained_pred_model_lv0, 
    unconstrained_pred_model_lv0_season, 
    unconstrained_pred_model_lv0_taiaroa)
#                                       df      BIC
# unconstrained_pred_model_lv0         625 16287.31
# unconstrained_pred_model_lv0_season  196 15240.57
# unconstrained_pred_model_lv0_taiaroa 508 16808.85

AIC(unconstrained_pred_model_lv0, 
    unconstrained_pred_model_lv0_season, 
    unconstrained_pred_model_lv0_taiaroa)
#                                       df      AIC
# unconstrained_pred_model_lv0         625 13861.98
# unconstrained_pred_model_lv0_season  196 14479.99
# unconstrained_pred_model_lv0_taiaroa 508 14837.54

# 'seasons' (summer == intercept)
gllvm::coefplot(unconstrained_pred_model_lv0_season,
                which.Xcoef = c(1:3),
                order = FALSE)

# # Selected 'taiaroa_head' (TaiaroaEast0.5km == intercept) 
# gllvm::coefplot(unconstrained_pred_model_lv0_season,
#                 which.Xcoef = c(4,7,10,13),
#                 order = FALSE)

