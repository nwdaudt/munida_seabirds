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
library(tidyr)
library(forcats)
library(ggplot2)
library(patchwork)

# library(cowplot)
# library(RColorBrewer)
# library(colorspace)

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

data$count <- as.numeric(data$count)

##  ---- Transform the data set from long to (simplified) wide format  ---- ##
data_wide <- 
  data %>% 
  tidyr::pivot_wider(names_from = species_nice_name,
                     values_from = count,
                     values_fill = 0)

# Get spp and sp-only column names
spp_cols <- colnames(data_wide[,c(18:74)]) ## All seabirds
sp_only_cols <- spp_cols[! grepl(pattern = "Unknown", x = spp_cols)] ## Only species-level

# Calculate total number of birds per sample (total_birds)
data_wide <- 
  data_wide %>% 
  dplyr::mutate(total_birds = rowSums(across(all_of(spp_cols)))) %>% 
  dplyr::select(id, season, year, all_of(sp_only_cols), total_birds) %>% 
  dplyr::group_by(id, season, year) %>% 
  dplyr::summarise(across(everything(), list(sum)))

colnames(data_wide) <- c("id", "season", "year", sp_only_cols, "total_birds")

## EDA ####

#------------------------------------------------------------------------------#
## Colour palettes (colour-blind friendly) ------------------------------------#
#------------------------------------------------------------------------------#

## two_colour_palette_discrete
# palette.colors(palette = "Set2")[2:3]

## four_colour_palette_discrete 
# palette.colors(palette = "Tableau 10")[1:4]

## Effort summary ----------------------------------------------------------####
#------------------------------------------------------------------------------#

effort_summary <-
  data_wide %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(number_of_voyages = n_distinct(id),
                   number_of_years_sampled = n_distinct(year),
                   number_of_species = sum(colSums(across(all_of(sp_only_cols))) > 0),
                   number_of_individuals = sum(total_birds))

# write.csv(effort_summary,
#           "./results/seasonal-effort-summary.csv",
#           row.names = FALSE)

rm("effort_summary")

## Summarise number of samples, by direction /5 km segment ----------------####
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

## Summarise number of species, by direction /5 km segment ----------------####
#------------------------------------------------------------------------------#
n_spp_taiaroa_east <-
  data %>% 
  dplyr::filter(species_nice_name %in% sp_only_cols) %>% 
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

## Summarise number of species and total number of birds, by season (overall) ####
#------------------------------------------------------------------------------#

## Species richness
n_spp_season <-
  data %>% 
  dplyr::filter(species_nice_name %in% sp_only_cols) %>% 
  dplyr::group_by(id, season) %>% 
  dplyr::summarise(n = n_distinct(species_nice_name))

overall_mean_n_spp_season <- floor(mean(n_spp_season$n))

## Total number of birds
n_birds_season <-
  data %>% 
  dplyr::filter(species_nice_name %in% sp_only_cols) %>% 
  dplyr::group_by(id, season) %>% 
  dplyr::summarise(n = sum(count)) %>% 
  dplyr::mutate(log10_n = log10(n))

overall_log10mean_n_birds_season <- round(mean(n_birds_season$log10_n), 
                                                digits = 2)

## Plot 1: species richness
gg_n_spp_season <-
  ggplot(data = n_spp_season, 
         aes(x = season, y = n, fill = season)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_n_spp_season,
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
  ggplot(data = n_birds_season, 
         aes(x = season, y = log10_n, fill = season)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  geom_hline(yintercept = overall_log10mean_n_birds_season,
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

rm("gg_n_spp_season", "gg_n_birds_season", "gg_spprichness_nbirds_season",
   "n_spp_season", "overall_mean_n_spp_season", "n_birds_season", "overall_log10mean_n_birds_season")

## Summarise number of species and total number of birds, /5 km segment ----####
#------------------------------------------------------------------------------#

## Species richness
n_spp_taiaroa_east_season <-
  data %>% 
  dplyr::filter(species_nice_name %in% sp_only_cols) %>% 
  dplyr::group_by(id, taiaroa_east, season) %>% 
  dplyr::summarise(n = n_distinct(species_nice_name))

overall_mean_n_spp_taiaroa_east <- floor(mean(n_spp_taiaroa_east_season$n))

## Total number of birds
n_birds_taiaroa_east_season <-
  data %>% 
  dplyr::filter(species_nice_name %in% sp_only_cols) %>% 
  dplyr::group_by(id, taiaroa_east, season) %>% 
  dplyr::summarise(n = sum(count)) %>% 
  dplyr::mutate(log10_n = log10(n))

overall_log10mean_n_birds_taiaroa_east <- round(mean(n_birds_taiaroa_east_season$log10_n), 
                                                digits = 2)

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

## Frequency of occurrence and numeric frequency, by season ----------------####
#------------------------------------------------------------------------------#

# Specify functions to calculate frequency of occurrence (freq_occ) and numeric frequency (freq_num)
funs <- list(freq_occ = ~ sum(.x >= 1)/n() *100,
             freq_num = ~ sum(.x)/sum(dplyr::pick(total_birds)) *100)

data_species_fo_nf <-
  data_wide %>% 
  dplyr::group_by(season) %>%
  dplyr::summarise(across(all_of(sp_only_cols), .fns = funs)) %>%
  tidyr::pivot_longer(cols = !season, 
                      names_to = "species_freq",
                      values_to = "value") %>%
  dplyr::mutate(value = round(value, digits = 2)) %>% 
  # split name into variables
  tidyr::separate(species_freq, 
                  into = c("species", "freq"),
                  sep = -8) %>% 
  # remove an extra underline
  dplyr::mutate(species = stringr::str_sub(species, end = -2))

#### FO/NF plots

## Frequency of occurrence
plot_freq_occ <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_occ") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"), 
                    name = NULL) +
  facet_grid(~ season) +
  ylab("Frequency of occurrence (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title.x = element_text(size = 8),
        strip.text = element_text(size = 8))

## Relative abundance (numeric frequency -- total)
plot_freq_num <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_num") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")) +
  facet_grid(~ season) +
  ylab("Relative abundance (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title.x = element_text(size = 8),
        strip.text = element_text(size = 8))

## Patchwork these plots and save it
freqs_occ_num <-
  plot_freq_occ / plot_freq_num

ggsave(freqs_occ_num,
       filename = "./results/EDA_sp_frqs-occ-num-season.pdf",
       width = 16, height = 25, units = "cm", dpi = 300)

rm("plot_freq_occ", "plot_freq_num", "freqs_occ_num", "data_species_fo_nf")

## Frequency of occurrence and numeric frequency, by year ----------------####
#------------------------------------------------------------------------------#

data_species_fo_nf_year <-
  data_wide %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(across(all_of(sp_only_cols), .fns = funs)) %>%
  tidyr::pivot_longer(cols = !year, 
                      names_to = "species_freq",
                      values_to = "value") %>%
  dplyr::mutate(value = round(value, digits = 2)) %>% 
  # split name into variables
  tidyr::separate(species_freq, 
                  into = c("species", "freq"),
                  sep = -8) %>% 
  # remove an extra underline
  dplyr::mutate(species = stringr::str_sub(species, end = -2))

## rm 'funs' as it is not needed anymore
rm("funs")

#### FO/NF plots

## Frequency of occurrence
plot_freq_occ_year <-
  data_species_fo_nf_year %>% 
  dplyr::filter(freq == "freq_occ") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = as.factor(year))) + 
  geom_col(color = "gray58") +
  scale_fill_brewer(palette = "Reds", name = NULL) +
  facet_grid(~ year) +
  ylab("Frequency of occurrence (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title.x = element_text(size = 8),
        strip.text = element_text(size = 8))

## Relative abundance (numeric frequency -- total)
plot_freq_num_year <-
  data_species_fo_nf_year %>% 
  dplyr::filter(freq == "freq_num") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species_nice_name), value), 
                y = value, 
                fill = as.factor(year))) + 
  geom_col(color = "gray58") +
  scale_fill_brewer(palette = "Reds", name = NULL) +
  facet_grid(~ year) +
  ylab("Relative abundance (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title.x = element_text(size = 8),
        strip.text = element_text(size = 8))

## Patchwork these plots and save it
freqs_occ_num_year <-
  plot_freq_occ_year / plot_freq_num_year

ggsave(freqs_occ_num_year,
       filename = "./results/EDA_sp_frqs-occ-num-year.pdf",
       width = 30, height = 25, units = "cm", dpi = 300)

rm("plot_freq_occ_year", "plot_freq_num_year", "freqs_occ_num_year", "data_species_fo_nf_year")

## Summarise % of each water mass /5 km segment ---------------------------####
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
                    "TaiaroaEast50.55km", "TaiaroaEast55.60km"),
         labels = c("0-5 km", "5-10 km", "10-15 km", "15-20 km",
                    "20-25 km", "25-30 km", "30-35 km", "35-40 km",
                    "40-45 km", "45-50 km", "50-55 km", "55-60 km"))

pct_watermass_taiaroa_east_season$season <- 
  factor(pct_watermass_taiaroa_east_season$season,
         levels = c("summer", "autumn", "winter", "spring"),
         labels = c("Summer", "Autumn", "Winter", "Spring"))

pct_watermass_taiaroa_east_season$water_mass <- 
  factor(pct_watermass_taiaroa_east_season$water_mass,
         levels = c("NW", "STW", "SASW"),
         labels = c("NW", "STW", "SASW"))

gg_pct_watermass_taiaroa_east_season <-
  ggplot(data = na.omit(pct_watermass_taiaroa_east_season),
         aes(x = taiaroa_east, y = n, fill = water_mass)) + 
  geom_bar(position = "fill", stat = "identity", alpha = 0.8) + 
  scale_fill_brewer(palette = "Dark2", name = NULL) + 
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

rm("pct_watermass_taiaroa_east_season", "gg_pct_watermass_taiaroa_east_season", "wm_data")

## Summarise % of windstress class, by season ------------------------------####
#------------------------------------------------------------------------------#

pct_windstress_season <-
  data %>% 
  dplyr::distinct(id, season, windstress_class) %>% 
  dplyr::group_by(season, windstress_class) %>%
  dplyr::summarise(n = n()) %>% 
  na.omit()

pct_windstress_season$windstress_class <- 
  factor(pct_windstress_season$windstress_class,
         levels = c("strong_upfront", "weak_upfront", "weak_downfront", "strong_downfront"),
         labels = c("Strong upfront", "Weak upfront", "Weak downfront", "Strong downfront"))

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

rm("pct_windstress_season", "gg_pct_windstress_season")
