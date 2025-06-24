## 
## Binomial models
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script runs GLMMs using the {gllvm} framework (i.e. zero latent variables)

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)
library(gllvm)
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
  ## Remove columns that will not be used in the model 
  dplyr::select(- c(latitude, longitude, 
                    date, month, species_nice_name, 
                    avg_windstress, avg_sst_grad_km, avg_sss_grad_km,
                    sst, sss, windstress_class, water_mass)) %>% 
  ## Pivot wider
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0) %>% 
  ## Only using data from the 'inbound' direction
  dplyr::filter(direction == "inbound") %>% 
  ## Remove 'direction' column
  dplyr::select(- direction)

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
  # Transform counts to presence-absence ("p_a") [0,1]
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
# but also, add the distribution status (see Table S2 in the Supplementary Material of the manuscript)

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
    species == "diving_petrel" ~ "Common diving petrel [D - SWP]",
    species == "black_billed_gull" ~ "Black-billed gull [(S)]",
    species == "cooks_petrel" ~ "Cook's petrel [M - NP, EP]",
    species == "antarctic_fulmar" ~ "Antarctic fulmar [(D - SO)]",
    species == "yellow_eye_penguin" ~ "Yellow-eyed penguin [S]",
  ), .after = species)

sp_cols_nice_names <- unique(long_data_pa$species_nice_name)

## As good practice for modelling, centre 'year' and 'dist_coast'
long_data_pa <-
  long_data_pa %>% 
  dplyr::mutate(dist_coast_centred = scale(long_data_pa$dist_coast, scale = FALSE)[,1],
                year_centred = scale(long_data_pa$year, scale = FALSE)[,1],
                voyage = as.factor(id))

## And finally, back again to wide-format to allow fitting 'gllvm's
wide_data_pa <-
  long_data_pa %>% 
  dplyr::select(- c(id, taiaroa_east, dist_coast, year, species)) %>% 
  ## Pivot wider
  tidyr::pivot_wider(names_from = species_nice_name,
                     values_from = p_a,
                     values_fill = 0)

### -------------------------------------------------- ###
### -------------- objs for modelling ---------------- ###
### -------------------------------------------------- ###

Y_matrix_pa <- wide_data_pa[, 5:43]
X_matrix <- wide_data_pa[, 1:4]

### -------------------------------------------------- ###

## Clean environment
rm(sp_cols_only, sp_rare_cols, spp_cols_all, unknown_and_rare_spp,
   data, long_data_pa, wide_data)

## Binomial models ####

gllvm_lv0 <- 
  gllvm::gllvm(y = Y_matrix_pa,
               X = data.frame(dist_coast_centred = X_matrix$dist_coast_centred,
                              year_centred = X_matrix$year_centred),
               formula = ~ dist_coast_centred + year_centred,
               studyDesign = data.frame(voyage = factor(X_matrix$voyage)),
               row.eff = ~ (1|voyage),
               num.lv = 0,
               method = "LA",
               family = binomial(),
               link = "logit",
               seed = 321)

### Residuals
# pdf(file = "./results/GLMM_gllvm-lv0-Binomial_residuals.pdf")
# plot(gllvm_lv0, which = 1:4, mfrow = c(2,2))
# dev.off()

### Save the model
# saveRDS(gllvm_lv0,
#         file = "./results/GLMM_gllvm-lv0-Binomial_model.rds")

### Plot the coefficient to check
# gllvm::coefplot(gllvm_lv0, which.Xcoef = c(2), order = FALSE)
# confint(gllvm_lv0, level = 0.95, parm = "Xcoef")[40:78,] ## Only coef 2 (== year_centred)

### Nice plot of the coefficients for publication

## Get estimated parameter
tmp <- 
  as.data.frame(gllvm_lv0[["params"]][["Xcoef"]]) %>% 
  tibble::rownames_to_column(var = "species") %>% 
  dplyr::filter(! species == "Cook's petrel [M - NP, EP]") %>%
  dplyr::mutate(species_ordered = forcats::fct_reorder(species, year_centred))

## Get *SD* values for the 'year_centred' parameter [better to use *CI*, see below]
tmp2_sd <-
  as.data.frame(gllvm_lv0[["sd"]][["Xcoef"]]) %>%
  tibble::rownames_to_column(var = "species") %>%
  dplyr::filter(! species == "Cook's petrel [M - NP, EP]") %>%
  dplyr::select(- dist_coast_centred) %>%
  dplyr::mutate(year_centred_SD = year_centred) %>%
  dplyr::mutate(species_ordered = forcats::fct_reorder(species, year_centred))

## Get *CI* for the 'year_centred' parameter
tmp2_ci <-
  as.data.frame(confint(gllvm_lv0, level = 0.95, parm = "Xcoef")[40:78,]) %>% 
  tibble::rownames_to_column(var = "species") %>% 
  dplyr::filter(! species == "Xcoef76") %>% 
  dplyr::rename(year_centred_CIlow = cilow,
                year_centred_CIup = ciup) %>% 
  dplyr::mutate(species_ordered = tmp$species_ordered)

## Get p-values and classify the level of evidence according to Muff et al. (2022)
tmp3 <- 
  as.data.frame(summary(gllvm_lv0)[["Coef.tableX"]]) %>% 
  tibble::rownames_to_column(var = "species") %>% 
  dplyr::filter(! stringr::str_detect(species, pattern = "dist_coast_centred")) %>% 
  dplyr::mutate(species = gsub(x = species, pattern = ".*\\:", replacement = "")) %>%
  dplyr::filter(! species == "Cook's petrel [M - NP, EP]") %>%
  dplyr::select(species, p_value = "Pr(>|z|)") %>% 
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
  )

## Put them all together and plot
tmp <- 
  cbind(tmp, 
        year_centred_SD = tmp2_sd$year_centred_SD, 
        year_centred_CIlow = tmp2_ci$year_centred_CIlow, 
        year_centred_CIup = tmp2_ci$year_centred_CIup,
        p_value = tmp3$p_value, 
        evidence = tmp3$evidence) %>% 
  dplyr::mutate(xmin = year_centred - year_centred_SD,
                xmax = year_centred + year_centred_SD) %>%
  dplyr::mutate(species_ordered = forcats::fct_reorder(species, as.numeric(evidence)))

rm(tmp2_sd, tmp2_ci, tmp3)

# plot_coeff_SD_year <-
#   ggplot(data = tmp) +
#   geom_vline(xintercept = 0, color = "grey25", linetype = "longdash") +
#   geom_point(aes(y = species_ordered, x = year_centred, color = evidence),
#              size = 2.5) + # color = "deepskyblue3"
#   geom_errorbar(aes(y = species_ordered, xmin = xmin, xmax = xmax, color = evidence)) +
#   scale_color_manual(values = c(hcl.colors(10, "Mako", rev = T)[c(2,3,6,7,10)])) +
#   ylab("") +  xlab("Coefficient \u00B1 S.D.") +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10, colour = "black"),
#         axis.title.y = element_text(size = 11, colour = "black"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 11),
#         legend.position = c(0.8, 0.16))

# ggsave(plot_coeff_SD_year,
#        filename = "./results/GLMM_gllvm-lv0-Binomial_spp-year-coeff-SDs-mako.pdf",
#        height = 19, width = 15, units = "cm", dpi = 300)

plot_coeff_CI_year <- 
  ggplot(data = tmp) +
  geom_vline(xintercept = 0, color = "grey25", linetype = "longdash") + 
  geom_point(aes(y = species_ordered, x = year_centred, color = evidence),
             size = 2.5) +
  geom_errorbar(aes(y = species_ordered, xmin = year_centred_CIlow, xmax = year_centred_CIup, color = evidence)) + 
  scale_color_manual(values = c(hcl.colors(10, "Mako", rev = T)[c(2,3,6,7,10)])) +
  ylab("") +  xlab("Coefficient \u00B1 C.I.") +
  theme_bw() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 11, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = c(0.8, 0.16))

ggsave(plot_coeff_CI_year,
       filename = "./results/GLMM_gllvm-lv0-Binomial_spp-year-coeff-CIs-mako.pdf",
       height = 19, width = 15, units = "cm", dpi = 300)

rm(tmp, plot_coeff_CI_year)
