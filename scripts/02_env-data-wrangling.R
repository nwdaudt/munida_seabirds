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
## classify into 'water masses' (Jones et al. 2013 Estuar. Coast. Shelf Sci. 124:44-55).

## 2 - Wind Stress raw data is an average, daily mean for the Munida transect area;
## which we need to average the values using the 5 (inclusive) days previous the
## transect, find out the quantiles, and then classify into strong or weak,
## downwelling- or upwelling-favourable conditions.

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
# unique(ts_data$mon_day_yr) # --- OK

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

## Summarise TS values
ts_data_summarised <-
  ts_data %>% 
  dplyr::group_by(date, taiaroa_east) %>% 
  dplyr::summarise(sst = mean(temperature_o_c, na.rm = T),
                   sss = mean(salinity_psu, na.rm = T),
                   # Jillett's paper mention front gradient being 1.5--2 degrees C
                   sst_grad = max(temperature_o_c, na.rm = T) - min(temperature_o_c, na.rm = T),
                   sss_grad = max(salinity_psu, na.rm = T) - min(salinity_psu, na.rm = T)) %>% 
  # Add a *season* column
  dplyr::mutate(month = as.numeric(stringr::str_sub(date, start = 6, end = -4))) %>% 
  dplyr::mutate(season = dplyr::case_when(
    month <= 03 ~ "summer",
    month > 03 & month <= 06 ~ "autumn",
    month > 06 & month <= 09 ~ "winter",
    month > 09 ~ "spring",
  ), .keep = "unused", .after = "date") %>% 
  dplyr::ungroup(.)

## Loop through the conditions to classify water masses

# Neritic Water (NW)
# Subtropical Water (STW)
# Sub-Antarctic Surface water (SASW)

# Assuming a linear transition between Summer/Winter seasons,
# get the arithmetic mean for values provided in 
# 'Table 1' of Jones et al. (2013) [Estuar. Coast. Shelf Sci. 124: 44-55],
# originally described in Jillett (1969) [NZ J. Mar. Freshwater Res. 3: 349-375]

# SSTs ("c(winter, summer)" values)
NW_sst_mid_seasons <- mean(c(10, 12))
STW_sst_mid_seasons <- mean(c(9.5, 12))
SASW_sst_mid_seasons <- mean(c(9.5, 12))

# SSSs ("c(winter, summer)" values)
NW_sss_mid_seasons <- mean(c(34.5, 34.6))
STW_sss_mid_seasons <- mean(c(34.5, 34.6))
SASW_sss_mid_seasons <- mean(c(34.5, 34.5))

# --- Classify water masses
seasons <- c("summer", "autumn", "winter", "spring")

ts_data_summarised_watermasses <- data.frame()

for(one_season in seasons){
  
  data <- 
    ts_data_summarised %>% 
    dplyr::filter(season == one_season)
  
  if(one_season == "summer"){
    data <- 
      data %>% 
      dplyr::mutate(water_mass = dplyr::case_when(
        sst > 12 & sss < 34.6 ~ "NW",
        sst > 12 & sss > 34.6 ~ "STW",
        sst < 12 & sss < 34.5 ~ "SASW",
        TRUE ~ "NA_verify"
      ))
  }
  
  if(one_season == "winter"){
    data <- 
      data %>% 
      dplyr::mutate(water_mass = dplyr::case_when(
        sst < 10 & sss < 34.5 ~ "NW",
        sst > 9.5 & sss > 34.5 ~ "STW",
        sst < 9.5 & sss < 34.5 ~ "SASW",
        TRUE ~ "NA_verify"
      ))
  }
  
  if(one_season == "autumn" | one_season == "spring"){
    data <- 
      data %>% 
      dplyr::mutate(water_mass = dplyr::case_when(
        sst > NW_sst_mid_seasons & sss < NW_sss_mid_seasons ~ "NW_verify", 
        sst > STW_sst_mid_seasons & sss > STW_sss_mid_seasons ~ "STW",
        sst < SASW_sst_mid_seasons & sss < SASW_sss_mid_seasons ~ "SASW",
        TRUE ~ "NA_verify"
      ))
  }
  
  ts_data_summarised_watermasses <- 
    dplyr::bind_rows(ts_data_summarised_watermasses, data)
  
  rm("data", "one_season")
}

write.csv(ts_data_summarised_watermasses, 
          file = "./data-processed/ts_data_summarised_watermasses.csv")

rm("seasons",
   "NW_sst_mid_seasons", "STW_sst_mid_seasons", "SASW_sst_mid_seasons",
   "NW_sss_mid_seasons", "STW_sss_mid_seasons", "SASW_sss_mid_seasons")

## Windstress data ####

windstress_data <- read.csv("./data-raw/ts-and-windstress/along_shelf_windstress.csv", 
                            header = FALSE)

colnames(windstress_data) <- c("day", "month", "year", "windstress")

windstress_data$date <-
  as.Date(paste0(windstress_data$year, "-", 
                 windstress_data$month, "-", 
                 windstress_data$day),
          format = "%Y-%m-%d")

qntls <- quantile(windstress_data$windstress, na.rm = T,
                  probs = c(0, 0.3, 0.5, 0.7, 1))

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

windstress_df <-
  windstress_df %>% 
  dplyr::mutate(windstress_class = dplyr::case_when(
    avg_windstress > qntls[1] & avg_windstress <= qntls[2] ~ "strong_upfront",
    avg_windstress > qntls[2] & avg_windstress <= qntls[3] ~ "weak_upfront",
    avg_windstress > qntls[3] & avg_windstress <= qntls[4] ~ "weak_downfront",
    avg_windstress > qntls[4] & avg_windstress <= qntls[5] ~ "strong_downfront"
  ))

write.csv(windstress_df, 
          file = "./data-processed/windstress_data_summarised.csv")

## Merge TS and Windstress data with 'seabird_data_long', and save it ####

all_data_long <-
  dplyr::left_join(seabird_data,
                   (windstress_df %>% 
                      dplyr::mutate(date = as.character(date))),
                   by = c("date")) %>% 
  dplyr::left_join(.,
                   (ts_data_summarised_watermasses %>% 
                      dplyr::mutate(date = as.character(date),
                                    taiaroa_east = as.character(taiaroa_east),
                                    season = NULL)),
                   by = c("date", "taiaroa_east")) %>% 
  # Better nomenclature to 'direction'
  dplyr::mutate(direction = ifelse(direction == "out",
                                   yes = "eastward",
                                   no = "westward"))

write.csv(all_data_long,
          file = "./data-processed/all_data_long.csv")
