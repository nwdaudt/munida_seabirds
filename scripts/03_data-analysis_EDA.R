## 
## EDA
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script runs Exploratory Data Analysis (EDA), mostly summaries and 
## descriptive stats.

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(ggplot2)
library(patchwork)
# library(cowplot)
# library(RColorBrewer)
# library(colorspace)

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

## EDA ####

#------------------------------------------------------------------------------#
## Colour palettes (colour-blind friendly) ------------------------------------#
#------------------------------------------------------------------------------#

## two_colour_palette_discrete
# palette.colors(palette = "Set2")[2:3]

## four_colour_palette_discrete 
# palette.colors(palette = "Tableau 10")[1:4]

#------------------------------------------------------------------------------#
## Summarise number of samples, by direction, in each 5 km transect -----------#
#------------------------------------------------------------------------------#
n_sample_taiaroa_east <-
  data %>% 
  dplyr::group_by(taiaroa_east, direction) %>% 
  dplyr::summarise(n = n_distinct(date))

## Plot 
gg_n_sample_taiaroa_east <-
  ggplot(data = n_sample_taiaroa_east) +
  geom_bar(aes(x = taiaroa_east, y = n, fill = direction),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FC8D62", "#8DA0CB"), name = NULL) +
  xlab("") + ylab("Number of samples") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.86),
        legend.background = element_rect(colour = "black"))

ggsave(gg_n_sample_taiaroa_east,
       filename = "./results/EDA_n_sample_taiaroa_east.pdf",
       height = 12, width = 18, units = "cm")

rm("n_sample_taiaroa_east", "gg_n_sample_taiaroa_east")

#------------------------------------------------------------------------------#
## Summarise number of species, by direction, in each 5 km transect -----------#
#------------------------------------------------------------------------------#
n_spp_taiaroa_east <-
  data %>% 
  dplyr::group_by(id, taiaroa_east, direction) %>% 
  dplyr::summarise(n = n_distinct(species))

## Plot 
gg_n_spp_taiaroa_east <-
  ggplot(data = n_spp_taiaroa_east, aes(x = taiaroa_east, y = n, fill = direction)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#FC8D62", "#8DA0CB"), name = NULL) +
  xlab("") + ylab("Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.15, 0.88),
        legend.direction =  "horizontal",
        legend.background = element_rect(colour = "black"))

ggsave(gg_n_spp_taiaroa_east,
       filename = "./results/EDA_n_spp_taiaroa_east.pdf",
       height = 12, width = 18, units = "cm")

rm("n_spp_taiaroa_east", "gg_n_spp_taiaroa_east")

#------------------------------------------------------------------------------#
## Summarise number of species and total number of birds, by season (overall) -#
#------------------------------------------------------------------------------#

## Species richness
n_spp_taiaroa_east_season <-
  data %>% 
  dplyr::group_by(id, taiaroa_east, season) %>% 
  dplyr::summarise(n = n_distinct(species))

overall_mean_n_spp_taiaroa_east <- floor(mean(n_spp_taiaroa_east_season$n))

## Total number of birds
n_birds_taiaroa_east_season <-
  data %>% 
  dplyr::group_by(id, taiaroa_east, season) %>% 
  dplyr::summarise(n = sum(count)) %>% 
  dplyr::mutate(log10_n = log10(n))

overall_log10mean_n_birds_taiaroa_east <- round(mean(n_birds_taiaroa_east_season$log10_n), 
                                                digits = 2)

## Plot 1: species richness
gg_n_spp_season <-
  ggplot(data = n_spp_taiaroa_east_season, 
         aes(x = season, y = n, fill = season)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_n_spp_taiaroa_east,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

## Plot 2: number of birds
gg_n_birds_season <-
  ggplot(data = n_birds_taiaroa_east_season, 
         aes(x = season, y = log10_n, fill = season)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  geom_hline(yintercept = overall_log10mean_n_birds_taiaroa_east,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Total number of birds (log10)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

gg_spprichness_nbirds_season <- gg_n_spp_season / gg_n_birds_season

ggsave(gg_spprichness_nbirds_season,
       filename = "./results/EDA_spp-richness_n-birds_season.pdf",
       height = 10, width = 10, units = "cm")

rm("gg_n_spp_season", "gg_n_birds_season", "gg_spprichness_nbirds_season")

#------------------------------------------------------------------------------#
## Summarise number of species and total number of birds, ---------------------#
## on each 5 km transect ------------------------------------------------------#
#------------------------------------------------------------------------------#

## Note: using the same summarised objects from above

## Plot 1: species richness
gg_n_spp_taiaroa_east_season <-
  ggplot(data = n_spp_taiaroa_east_season, 
         aes(x = taiaroa_east, y = n, fill = season)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_n_spp_taiaroa_east,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top",
        legend.background = element_rect(colour = "black"))

## Plot 2: total number of birds
gg_n_birds_taiaroa_east_season <-
  ggplot(data = n_birds_taiaroa_east_season, 
         aes(x = taiaroa_east, y = log10_n, fill = season)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  geom_hline(yintercept = overall_log10mean_n_birds_taiaroa_east,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Total number of birds (log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        legend.background = element_rect(colour = "black"))

gg_spprichness_nbirds_taiaroa_east <- 
  gg_n_spp_taiaroa_east_season / gg_n_birds_taiaroa_east_season

ggsave(gg_spprichness_nbirds_taiaroa_east,
       filename = "./results/EDA_spp-richness_n-birds_taiaroa-east.pdf",
       height = 15, width = 16, units = "cm")

rm("gg_n_spp_taiaroa_east_season", "gg_n_birds_taiaroa_east_season", "gg_spprichness_nbirds_taiaroa_east",
   "n_spp_taiaroa_east_season", "overall_mean_n_spp_taiaroa_east",
   "n_birds_taiaroa_east_season", "overall_log10mean_n_birds_taiaroa_east")

#------------------------------------------------------------------------------#
## Summarise % of each water mass, in each 5 km transect ----------------------#
#------------------------------------------------------------------------------#
wm_data <- read.csv("./data-processed/ts_data_summarised_watermasses.csv")

pct_watermass_taiaroa_east_season <-
  wm_data %>% 
  dplyr::group_by(taiaroa_east, season, water_mass) %>%
  dplyr::summarise(n = n())

pct_watermass_taiaroa_east_season$taiaroa_east <- 
  factor(pct_watermass_taiaroa_east_season$taiaroa_east,
         levels = c("TaiaroaEast0.5km", "TaiaroaEast5.10km",
                    "TaiaroaEast10.15km", "TaiaroaEast15.20km",
                    "TaiaroaEast20.25km", "TaiaroaEast25.30km",
                    "TaiaroaEast30.35km", "TaiaroaEast35.40km",
                    "TaiaroaEast40.45km", "TaiaroaEast45.50km",
                    "TaiaroaEast50.55km", "TaiaroaEast55.60km"))

pct_watermass_taiaroa_east_season$season <- 
  factor(pct_watermass_taiaroa_east_season$season,
         levels = c("summer", "autumn", "winter", "spring"))

gg_pct_watermass_taiaroa_east_season <-
  ggplot(data = na.omit(pct_watermass_taiaroa_east_season),
         aes(x = taiaroa_east, y = n, fill = water_mass)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Accent", name = NULL) + 
  facet_wrap(~ season, nrow = 4) + 
  xlab("") + ylab("% water mass") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 10, colour = "black"),
        legend.position = "top")

ggsave(gg_pct_watermass_taiaroa_east_season,
       filename = "./results/EDA_pct_watermass_taiaroa_east_season.pdf",
       height = 15, width = 15, units = "cm")

rm("pct_watermass_taiaroa_east_season", "gg_pct_watermass_taiaroa_east_season")

#------------------------------------------------------------------------------#
## Summarise % of windstress class, by season ---------------------------------#
#------------------------------------------------------------------------------#

pct_windstress_season <-
  data %>% 
  dplyr::distinct(id, season, windstress_class) %>% 
  dplyr::group_by(season, windstress_class) %>%
  dplyr::summarise(n = n()) %>% 
  na.omit()

pct_windstress_season$season <- 
  factor(pct_windstress_season$season,
         levels = c("summer", "autumn", "winter", "spring"))

pct_windstress_season$windstress_class <- 
  factor(pct_windstress_season$windstress_class,
         levels = c("strong_upfront", "weak_upfront", "weak_downfront", "strong_downfront"))

## Check colour palette
# RColorBrewer::display.brewer.pal(12, "Paired")
# RColorBrewer::brewer.pal(12, "Paired")

gg_pct_windstress_season <-
  ggplot(data = pct_windstress_season,
         aes(x = season, y = n, fill = windstress_class)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = c("#FF7F00", "#FDBF6F", "#CAB2D6", "#6A3D9A"), name = NULL) +
  xlab("") + ylab("% windstress class") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 10, colour = "black"),
        legend.position = "right")

ggsave(gg_pct_windstress_season,
       filename = "./results/EDA_pct_windstress_season.pdf",
       height = 8, width = 11, units = "cm")
