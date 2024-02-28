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
library(patchwork)
library(RColorBrewer)
# library(colorspace)
# library(cowplot)

## Read data ####
data <- read.csv("./data-processed/all_data_long.csv")[, -c(1:2)]

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

### Run the model
unconstrained_biol_model <-
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 2, 
               family = "negative.binomial",
               seed = 321)

## Residuals -- look good!
pdf(file = "./results/gllvm_unconstrained_biol_residuals.pdf")
plot(unconstrained_biol_model, which = 1:4, mfrow = c(2,2))
dev.off()

# summary(unconstrained_model)
# ordiplot.gllvm(unconstrained_biol_model, biplot = TRUE, ind.spp = 10)

## Get LV values and arrange it in a dataframe to plot
df_plot_unconstrained_biol_model <-
  cbind((wide_data %>% dplyr::select(season, taiaroa_east)),
        as.data.frame(gllvm::getLV.gllvm(unconstrained_biol_model)))

## Plot colour-coded by 'season' --------------------------------------------- #
plot_unconstrained_biol_model_season <- 
  ggplot(data = df_plot_unconstrained_biol_model, 
         aes(x = LV1, y = LV2, color = season)) +
  geom_point() + 
  scale_color_manual(values = c("summer" = "#4E79A7", "autumn" = "#F28E2B", 
                                "winter" = "#E15759", "spring" = "#76B7B2")) +
  # coord_cartesian(ylim = c(-2.5,4), xlim = c(-2.5,4)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(plot_unconstrained_biol_model_season,
       filename = "./results/gllvm_unconstrained_biol_biplot_season.pdf",
       height = 9, width = 12, units = "cm", dpi = 300)

## Plot colour-coded by 'taiaroa_head' --------------------------------------- #

# Specify a 12-colour palette
palette_12cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "BrBG"))(12)

plot_unconstrained_biol_model_taiaroa <- 
  ggplot(data = df_plot_unconstrained_biol_model, 
         aes(x = LV1, y = LV2, color = taiaroa_east)) +
  geom_point() + 
  scale_color_manual(values = palette_12cols) +
  # coord_cartesian(ylim = c(-2.5,4), xlim = c(-2.5,4)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(plot_unconstrained_biol_model_taiaroa,
       filename = "./results/gllvm_unconstrained_biol_biplot_taiaroa.pdf",
       height = 10, width = 16, units = "cm", dpi = 300)

# Save the model object
saveRDS(unconstrained_biol_model,
        file = "./results/gllvm_unconstrained_biol_model.rds")

## Unconstrained ordination (including predictors) ####
