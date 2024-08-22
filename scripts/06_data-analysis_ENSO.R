## 
## ENSO 
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script runs multivariate GLM models to test if ENSO affected seabird numbers
## It also summarises some numbers regarding the ENSO phases

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
# library(mvabund)

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

data$direction <- 
  factor(data$direction,
         levels = c("outbound", "inbound"))

data$season <- 
  factor(data$season,
         levels = c("summer", "autumn", "winter", "spring"))

data$count <- as.numeric(data$count)

##  ---- Transform the data set from long to (simplified) wide format  ---- ##
data_wide <- 
  data %>% 
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0)

# Get spp and sp-only column names
spp_cols <- colnames(data_wide[,c(18:74)]) ## All seabirds
sp_only_cols <- spp_cols[! grepl(pattern = "unknown", x = spp_cols)] ## Only species-level

# Calculate total number of birds per sample (total_birds)
data_wide <- 
  data_wide %>% 
  dplyr::mutate(total_birds = rowSums(across(all_of(spp_cols)))) %>% 
  dplyr::select(id, season, year, all_of(sp_only_cols), total_birds) %>% 
  dplyr::group_by(id, season, year) %>% 
  dplyr::summarise(across(everything(), list(sum)))

colnames(data_wide) <- c("id", "season", "year", sp_only_cols, "total_birds")

## ENSO data ----------------------------------------------------------------- #

## NOAA NCEI data 
# Source: https://www.ncei.noaa.gov/access/monitoring/enso/soi 
# Accessed on the 17 Jul 2024
noaa_soi <- read.table("./data-raw/NOAA-NCEI_SOI.txt")
noaa_soi <- noaa_soi[2:(nrow(noaa_soi)-1),] ## "-1" as we won't need 2024
# Fix column names
col_names_soi <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(noaa_soi) <- col_names_soi
# Fix Year data-type
noaa_soi$year <- as.numeric(noaa_soi$year)

noaa_soi_long <- 
  noaa_soi %>% 
  tidyr::pivot_longer(cols = col_names_soi[-1],
                      names_to = "month",
                      values_to = "soi") %>% 
  # Get months as 'numeric' to merge with seabird data
  dplyr::mutate(month_number = match(month, month.abb), .after = month) %>% 
  # Fix SOI data-type to numeric 
  dplyr::mutate(soi = as.numeric(soi)) %>% 
  # Specify the SOI phase based on its value
  dplyr::mutate(soi_phase = dplyr::case_when(
    soi >= 1 ~ "La Niña",
    soi <= -1 ~ "El Niño",
    .default = "Neutral"
  ))

# Merge seabird (inbound) and SOI data
data_soi <-
  dplyr::left_join(data[data$direction == "inbound", ],
                   noaa_soi_long,
                   by = join_by(year == year, month == month_number))

## NIWA (until 2022)
# soi_phases_since_1880s <- 
#   read.csv("./data-raw/SOI-phases_NIWA/southern-oscillation-index-1876-to-2022.csv")
# 
# soi_phases <- 
#   read.csv("./data-raw/SOI-phases_NIWA/southern-oscillation-index-1876-to-2022.csv") %>% 
#   dplyr::filter(year >= 2015) %>% 
#   dplyr::mutate(month_number = match(month, month.abb)) %>% 
#   dplyr::select(year, month_number, soi_phase)
# 
# # Merge seabird (inbound) and ENSO data
# data_soi <- 
#   dplyr::left_join(data[data$direction == "inbound", ],
#                    soi_phases,
#                    by = join_by(year == year, month == month_number))

## ENSO phases % along the sampled years -----------------------------------####
#------------------------------------------------------------------------------#
soi_phases_summary_year <-
  noaa_soi_long %>% 
  dplyr::group_by(year, soi_phase) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::filter(year > 2014)

## Stacked barplot
plot_stacked_hist_ENSO_year <-
  ggplot(data = soi_phases_summary_year, 
         aes(x = as.character(year), # just to label all years in the fig :)
             y = count,
             fill = soi_phase)) +
  geom_bar(position = "fill", stat = "identity") +
  # scale_x_discrete() +
  scale_fill_brewer(palette = "Set1", name = NULL) +
  ylab("% ENSO phase/year") + xlab("") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 9, face = "bold",
                                   angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        legend.text = element_text(size = 9))

# ggsave(plot_stacked_hist_ENSO_year,
#        filename = "./results/ENSO_stacked-barplot_per-year.pdf",
#        height = 8, width = 10, units = "cm", dpi = 300)

## Number of voyages in each ENSO phase ------------------------------------####
#------------------------------------------------------------------------------#
SOI_summary_voyage <-
  data_soi %>% 
  dplyr::select(id, soi_phase) %>% 
  dplyr::distinct(id, soi_phase) %>% 
  dplyr::group_by(soi_phase) %>% 
  dplyr::summarise(count = n())
  

## Barplot
plot_barplot_voyages_per_ENSO_phase <-
  ggplot(data = SOI_summary_voyage,
         aes(x = soi_phase,
             y = count,
             fill = soi_phase)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1", name = NULL) +
  xlab("") + ylab("Number of voyages") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none")

# ggsave(plot_barplot_voyages_per_ENSO_phase,
#        filename = "./results/ENSO_barplot_number-of-voyages-per-phase.pdf",
#        height = 8, width = 8, units = "cm", dpi = 300)

## {patchwork} the above two plots -----------------------------------------####
#------------------------------------------------------------------------------#

ENSO_summary <- 
  plot_stacked_hist_ENSO_year / plot_barplot_voyages_per_ENSO_phase +
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(ENSO_summary,
       filename = "./results/ENSO_summary_bar-and-stack-plots.pdf",
       height = 14, width = 12, units = "cm", dpi = 300)


rm("soi_phases_summary_year", "plot_stacked_hist_ENSO_year",
   "SOI_summary_voyage", "plot_barplot_voyages_per_ENSO_phase",
   "ENSO_summary")

## Species counts related to SOI phases ------------------------------------####
#------------------------------------------------------------------------------#

data_soi_plot <- 
  data_soi %>% 
  dplyr::group_by(id, species, soi_phase) %>% 
  dplyr::summarise(count_id = sum(count)) %>% 
  dplyr::ungroup() %>% 
  # Keep only species identified to the species level
  dplyr::filter(species %in% sp_only_cols) %>% 
  # Select columns for plot
  dplyr::select(species, count_id, soi_phase)

less_than_3_obs <-
  # Get species names and number of occurrences
  data.frame(
    species = sp_only_cols,
    n_occ = apply(data_wide[sp_only_cols], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL) %>%
  # Filter and pull species names
  dplyr::filter(n_occ < 3) %>%
  dplyr::pull(species)

# Exclude species with less than 3 observations
data_soi_plot <-
  data_soi_plot %>% 
  dplyr::filter(! species %in% less_than_3_obs)

## Violin plot
violin_counts_soi <- 
  ggplot(data = data_soi_plot,
         aes(y = count_id,
             x = as.factor(soi_phase),
             fill = soi_phase)) +
  geom_point(data = data_soi_plot,
             aes(color = soi_phase, fill = soi_phase),
             position = "jitter", show.legend = FALSE) +
  geom_violin(alpha = 0.6) + 
  scale_color_brewer(palette = "Set1", name = NULL) +
  scale_fill_brewer(palette = "Set1", name = NULL) +
  facet_wrap(~ species, scales = "free_y") +
  ylab("log10(total count per voyage)") + xlab("") +
  theme_bw() + 
  theme(strip.text = element_text(size = 6.5),
        # axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.position = c(0.75, 0.05)) +
  guides(fill = guide_legend(nrow = 1),
         color = "none")

ggsave(violin_counts_soi,
       filename = "./results/ENSO_violin-plot_total-counts-by-voyage.pdf",
       height = 16, width = 32, units = "cm", dpi = 300)

rm("data_soi_plot", "less_than_3_obs", "violin_counts_soi")

## {mvabund} ---------------------------------------------------------------####
#------------------------------------------------------------------------------#
#
# ### There results *were not* used in the manuscript, but I'll leave the code here anyway 
# 
# data_soi_wide <- 
#   # Get ENSO phases into the wide-format data
#   dplyr::left_join(data_wide,
#                    (data_soi %>% 
#                       dplyr::select(id, soi_phase) %>% 
#                       dplyr::distinct(id, soi_phase)),
#                    by = "id") %>% 
#   # 2023 do not have data on ENSO phases
#   dplyr::filter(! year == "2023") %>% 
#   # To use 'Neutral' as the Intercept in the model, specify levels to the factor
#   dplyr::mutate(soi_phase = factor(soi_phase,
#                                    levels = c("Neutral", 
#                                               "El Niño",
#                                               "La Niña")))
# 
# ### Get the multivariate data into the right format
# Y <- 
#   mvabund::as.mvabund(data_soi_wide[, colnames(data_soi_wide) %in% sp_only_cols])
# 
# # pdf(file = "./results/mvGLM_mean-var-plot.pdf")
# # mvabund::meanvar.plot(Y)
# # dev.off()
# 
# ### Specify the model 
# mvGLM_ENSO <- 
#   mvabund::manyglm(Y ~ soi_phase,
#                    data = data_soi_wide,
#                    family = "negative.binomial")
# 
# ### Residuals
# # pdf(file = "./results/mvGLM_Y-ENSO_residuals.pdf")
# # plot(mvGLM_ENSO, which = 1:3)
# # dev.off()
# 
# ### Results ---
# 
# summary(mvGLM_ENSO)
# 
# # Test statistics:
# #                  wald value Pr(>wald)    
# # (Intercept)           61.08     0.001 ***
# # soi_phaseEl Niño       5.20     0.459    
# # soi_phaseLa Niña       6.95     0.180    
# # --- 
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# # 
# # Test statistic:  8.621, p-value: 0.269 
# # Arguments:
# #   Test statistics calculated assuming response assumed to be uncorrelated 
# #   P-value calculated using 999 resampling iterations via pit.trap resampling (to account for correlation in testing).
# 
# anova(mvGLM_ENSO)
# # > This gives an analysis of deviance table where we use likelihood ratio tests and 
# # > resampled p values to look for a significant effect of ENSO on the community data
# 
# # Analysis of Deviance Table
# # 
# # Model: Y ~ soi_phase
# # 
# # Multivariate test:
# #             Res.Df Df.diff   Dev Pr(>Dev)
# # (Intercept)     33                       
# # soi_phase       31       2 123.4    0.162
# # Arguments:
# #   Test statistics calculated assuming uncorrelated response (for faster computation) 
# # P-value calculated using 999 iterations via PIT-trap resampling.
# 
# mvGLM_anova_puni <- anova(mvGLM_ENSO, p.uni = "adjusted")
# # Treatment (ENSO) effect on mean abundance 
# mvGLM_anova_puni[["table"]]
# # Species-specific tests
# View(
#   cbind(
#     Dev = t(mvGLM_anova_puni[["uni.test"]])[,2], 
#     p_value = t(mvGLM_anova_puni[["uni.p"]])[,2]
#     ))
# # anova(mvGLM_ENSO, p.uni = "adjusted") # straight like this it returns Dev and Pr(>Dev)
