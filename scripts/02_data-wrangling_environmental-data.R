## 
## Environmental data
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## This script wrangles the Temperature-Salinity and the Wind Stress data,
## and merges them with 'seabird_data_long'.

## See main text for full explanation, but briefly:

## 1 - Temperature-Salinity raw data consists of an average value for each 0.5 km
## while on transect, which we then need to average within our 5 km transects and
## classify into 'water masses'.

## 2 - Wind Stress raw data is a daily mean for the Munida transect area.
## We need to average the values using the 5 (inclusive) days previous the transect,
## and then classify into strong or weak, downwelling- or upwelling-favourable conditions.

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####

library(readxl)
library(dplyr)
library(janitor)

## Get Munida transect dates ####

seabird_data <- read.csv("./data-processed/seabird_data_long.csv")

munida_dates <- 
  seabird_data %>% 
  dplyr::distinct(date) %>% 
  dplyr::pull() %>% 
  as.Date(date, format = "%Y-%m-%d")

## Temperature-Salinity data ####

## Read the data
ts_data <- 
  readxl::read_xlsx("./data-raw/ts-and-windstress/Munida surface TS data.xlsx") %>% 
  janitor::clean_names() %>% 
  # Transform from `POSIXct` to `Date` class
  dplyr::mutate(date = as.Date(mon_day_yr), 
                .keep = "unused", .before = "distance_km_from_th") %>% 
  # Keep only dates in 'munida_dates'
  dplyr::filter(date %in% munida_dates)

# Check
# unique(ts_data$date) # --- OK

## Specify in which 5-km transect each value (row) is on
ts_data <- 
  ts_data %>% 
  dplyr::mutate(taiaroa_east = dplyr::case_when(
    distance_km_from_th <= 5 ~ "TaiaroaEast0.5km",
    distance_km_from_th > 5 & distance_km_from_th <= 10 ~ "TaiaroaEast5.10km",
    distance_km_from_th > 10 & distance_km_from_th <= 15 ~ "TaiaroaEast10.15km",
    distance_km_from_th > 15 & distance_km_from_th <= 20 ~ "TaiaroaEast15.20km",
    distance_km_from_th > 20 & distance_km_from_th <= 25 ~ "TaiaroaEast20.25km",
    distance_km_from_th > 25 & distance_km_from_th <= 30 ~ "TaiaroaEast25.30km",
    distance_km_from_th > 30 & distance_km_from_th <= 35 ~ "TaiaroaEast30.35km",
    distance_km_from_th > 35 & distance_km_from_th <= 40 ~ "TaiaroaEast35.40km",
    distance_km_from_th > 40 & distance_km_from_th <= 45 ~ "TaiaroaEast40.45km",
    distance_km_from_th > 45 & distance_km_from_th <= 50 ~ "TaiaroaEast45.50km",
    distance_km_from_th > 50 & distance_km_from_th <= 55 ~ "TaiaroaEast50.55km",
    distance_km_from_th > 55 & distance_km_from_th <= 60 ~ "TaiaroaEast55.60km",
  ), .before = "distance_km_from_th")

ts_data$taiaroa_east <-
  factor(ts_data$taiaroa_east, levels = c("TaiaroaEast0.5km",
                                          "TaiaroaEast5.10km",
                                          "TaiaroaEast10.15km",
                                          "TaiaroaEast15.20km",
                                          "TaiaroaEast20.25km",
                                          "TaiaroaEast25.30km",
                                          "TaiaroaEast30.35km",
                                          "TaiaroaEast35.40km",
                                          "TaiaroaEast40.45km",
                                          "TaiaroaEast45.50km",
                                          "TaiaroaEast50.55km",
                                          "TaiaroaEast55.60km"))

## Function to get the 'centered finite differences' (i.e. calculate gradients)

FUN_cfd <- function(x){
  
  ## Specify 
  t_j = lead(x)
  t_jminus2 = lag(x)
  dist_range = 1000 
      # In our particular case, each data point is separated by 500 m, 
      # so the range between 't_j' and 't_jminus2' is always fixed, 1000 m. 
  
  ## Calculate 'centered finite differences' (unit per METER)
  cfd_value = (t_j - t_jminus2) / dist_range
  
  ## Get the average value of CFD and return this value
  cfd_avg = round(mean(cfd_value, na.rm = TRUE), digits = 5)
  
  if(is.nan(cfd_avg) == TRUE){
    cfd_avg = NA_real_
  }
  
  ## Return average value per METER
  return(cfd_avg)
}


## Summarise TS values
ts_data_summarised <-
  ts_data %>% 
  dplyr::group_by(date, taiaroa_east) %>% 
  dplyr::summarise(sst = mean(temperature_o_c, na.rm = T),
                   sss = mean(salinity_psu, na.rm = T),
                   # Jillett's paper mention front gradient being 1.5--2 degrees C
                   avg_sst_grad_km = FUN_cfd(temperature_o_c) *1000, # 'per km'
                   avg_sss_grad_km = FUN_cfd(salinity_psu) *1000) %>% 
  # Add a *season* column
  dplyr::mutate(month = as.numeric(stringr::str_sub(date, start = 6, end = -4))) %>% 
  dplyr::mutate(season = dplyr::case_when(
    month <= 03 ~ "summer",
    month > 03 & month <= 06 ~ "autumn",
    month > 06 & month <= 09 ~ "winter",
    month > 09 ~ "spring",
  ), .keep = "unused", .after = "date") %>% 
  dplyr::ungroup(.)

## Classify water masses

## Classifying water masses off Otago Peninsula is tricky, and using TS data
## often do not solve the complexities of the system.
## Therefore we decided to classify water masses by salinity and distance
## from the coast only. See the main text for references and rationale.

## Main refs: 
## Jillett (1969) [NZ. J. Mar. Freshwater Res. 3: 349-375; "Identification of water mass boundaries", p. 360]
## Stevens et al. (2021) [NZ. J. Mar. Freshwater Res. 55: 6-45; Fig. 13]
## Johnson et al. (2024) [Cont. Shelf Res. 277: 105248; Fig. 3]

## Criteria
# Neritic Water (NW)                 == SSS < 34.6 & 'dist_coast' < 20km
# Subtropical Water (STW)            == SSS >= 34.6
# Sub-Antarctic Surface water (SASW) == SSS < 34.6 & 'dist_coast' > 20km

strg_taiaroa_east_0to20km <-
  c("TaiaroaEast0.5km",
    "TaiaroaEast5.10km",
    "TaiaroaEast10.15km",
    "TaiaroaEast15.20km")

ts_data_summarised <-
  ts_data_summarised %>% 
  dplyr::mutate(water_mass = dplyr::case_when(
    sss < 34.6 & taiaroa_east %in% strg_taiaroa_east_0to20km ~ "NW",
    sss >= 34.6 ~ "STW",
    sss < 34.6 & ! (taiaroa_east %in% strg_taiaroa_east_0to20km) ~ "SASW"
  ))

rm("strg_taiaroa_east_0to20km")

write.csv(ts_data_summarised, 
          file = "./data-processed/ts_data_summarised_watermasses.csv",
          row.names = FALSE)

## ---------------------------------------------------------------------------- #
## Below, a code that classifies water masses based on both, T and S. --------- #
## NOTE -- it doesn't work properly... ---------------------------------------- #
## ---------------------------------------------------------------------------- #
#
# # Assuming a linear transition between Summer/Winter seasons,
# # get the arithmetic mean for values provided in 
# # 'Table 1' of Jones et al. (2013) [Estuar. Coast. Shelf Sci. 124: 44-55],
# # originally described in Jillett (1969) [NZ J. Mar. Freshwater Res. 3: 349-375]
# 
# # SSTs ("c(winter, summer)" values)
# NW_sst_mid_seasons <- mean(c(10, 12))
# STW_sst_mid_seasons <- mean(c(9.5, 12))
# SASW_sst_mid_seasons <- mean(c(9.5, 12))
# 
# # SSSs ("c(winter, summer)" values)
# NW_sss_mid_seasons <- mean(c(34.5, 34.6))
# STW_sss_mid_seasons <- mean(c(34.5, 34.6))
# SASW_sss_mid_seasons <- mean(c(34.5, 34.5))
# 
# # --- Classify water masses
# seasons <- c("summer", "autumn", "winter", "spring")
# 
# ts_data_summarised_watermasses <- data.frame()
# 
# for(one_season in seasons){
#   
#   data <- 
#     ts_data_summarised %>% 
#     dplyr::filter(season == one_season)
#   
#   if(one_season == "summer"){
#     data <- 
#       data %>% 
#       dplyr::mutate(water_mass = dplyr::case_when(
#         sst > 12 & sss < 34.6 ~ "NW",
#         sst > 12 & sss > 34.6 ~ "STW",
#         sst < 12 & sss < 34.5 ~ "SASW",
#         TRUE ~ "NA_verify"
#       ))
#   }
#   
#   if(one_season == "winter"){
#     data <- 
#       data %>% 
#       dplyr::mutate(water_mass = dplyr::case_when(
#         sst < 10 & sss < 34.5 ~ "NW",
#         sst > 9.5 & sss > 34.5 ~ "STW",
#         sst < 9.5 & sss < 34.5 ~ "SASW",
#         TRUE ~ "NA_verify"
#       ))
#   }
#   
#   if(one_season == "autumn" | one_season == "spring"){
#     data <- 
#       data %>% 
#       dplyr::mutate(water_mass = dplyr::case_when(
#         sst > NW_sst_mid_seasons & sss < NW_sss_mid_seasons ~ "NW_verify", 
#         sst > STW_sst_mid_seasons & sss > STW_sss_mid_seasons ~ "STW",
#         sst < SASW_sst_mid_seasons & sss < SASW_sss_mid_seasons ~ "SASW",
#         TRUE ~ "NA_verify"
#       ))
#   }
#   
#   ts_data_summarised_watermasses <- 
#     dplyr::bind_rows(ts_data_summarised_watermasses, data)
#   
#   rm("data", "one_season")
# }
# 
# rm("seasons",
#    "NW_sst_mid_seasons", "STW_sst_mid_seasons", "SASW_sst_mid_seasons",
#    "NW_sss_mid_seasons", "STW_sss_mid_seasons", "SASW_sss_mid_seasons")
## ---------------------------------------------------------------------------- #
## ---------------------------------------------------------------------------- #

## Windstress data ####

windstress_data <- read.csv("./data-raw/ts-and-windstress/along_shelf_windstress.csv", 
                            header = FALSE)

colnames(windstress_data) <- c("day", "month", "year", "windstress")

windstress_data$date <-
  as.Date(paste0(windstress_data$year, "-", 
                 windstress_data$month, "-", 
                 windstress_data$day),
          format = "%Y-%m-%d")

## Loop through to calculate average windstress 
windstress_df <- data.frame()

for (munida_date in munida_dates) {
  # print(as.Date(munida_date, origin = "1970-01-01"))
  
  dates_avg <- (as.Date(munida_date, origin = "1970-01-01")-4):as.Date(munida_date, origin = "1970-01-01")
  dates_avg <- as.Date(dates_avg, origin = "1970-01-01")
  
  tmp <- 
    windstress_data %>% 
    dplyr::filter(date %in% dates_avg) %>% 
    dplyr::summarise(avg_windstress = mean(windstress, na.rm = T)) %>% 
    dplyr::mutate(date = as.Date(munida_date, origin = "1970-01-01"), .before = avg_windstress)
  
  windstress_df <- rbind(windstress_df, tmp)
  
  rm("munida_date", "dates_avg", "tmp")
}

## According to Johnson et al. (2023) [JGR Oceans 128:e2022JC019609; p. 7],
## upfront threshold (<= −0.04 N/m2) and downfront (>= 0.04 N/m2).
## Values ranging from zero to one of the above values, will be classified as 
## 'weak' upfront/downfront, whereas values equal or higher than those values as 'strong'

windstress_df <-
  windstress_df %>%
  dplyr::mutate(windstress_class = dplyr::case_when(
    avg_windstress <= -0.04 ~ "strong_upfront",
    avg_windstress < 0 & avg_windstress > -0.04 ~ "weak_upfront",
    avg_windstress > 0 & avg_windstress < 0.04 ~ "weak_downfront",
    avg_windstress >= 0.04 ~ "strong_downfront"
  ))

write.csv(windstress_df, 
          file = "./data-processed/windstress_data_summarised.csv",
          row.names = FALSE)

## Merge TS and Windstress data with 'seabird_data_long', and save it ####

all_data_long <-
  dplyr::left_join(seabird_data,
                   (windstress_df %>% 
                      dplyr::mutate(date = as.character(date))),
                   by = c("date")) %>% 
  dplyr::left_join(.,
                   (ts_data_summarised %>% 
                      dplyr::mutate(date = as.character(date),
                                    taiaroa_east = as.character(taiaroa_east),
                                    season = NULL)),
                   by = c("date", "taiaroa_east")) %>% 
  # Better nomenclature to 'direction'
  dplyr::mutate(direction = ifelse(direction == "out",
                                   yes = "outbound",
                                   no = "inbound"))

## Before saving it, check if there are any duplicates on the data
tmp <- 
  all_data_long %>%
  dplyr::group_by(id, taiaroa_east, direction, latitude, longitude, date, year, month, season, avg_windstress,
                  windstress_class, sst, sss, avg_sst_grad_km, avg_sss_grad_km, water_mass, species) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

# > YES, one. Deal with it:

# Excluded the 'cape pigeon' with count == 1 and kept cape pigeon with count == 3
all_data_long <- all_data_long[-c(1257),]

# Re-run 'tmp' object above to check -- fine now.
rm("tmp")

## Save it
write.csv(all_data_long,
          file = "./data-processed/all_data_long.csv",
          row.names = FALSE)
