## 
## Raw data to standardised format
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## We have two file sources here -- 'digitised' and 'ebird'. 

## In the former, Nico and Graeme digitised data living on Graemes's notebooks 
## in a format ready for Graeme to upload them into eBird. In the later, a few 
## Munida voyages were already uploaded into eBird, and notebooks were missing -- 
## so, we got the data from eBird itself. However, the format doesn't match 
## with the 'digitised' ones.

## This script wrangles, standardises, and merge these both datasets sources.

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(plyr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

## Read data raw ('digitised') and standardise ####

# Get file names
files <- dir(path = "./data-raw/digitised", pattern = "*.csv", ignore.case = TRUE, full.names = TRUE)

# Create a list to store all datasets intependently
dfs_list <- list()

# Loop through the files, standardise a few things, and store in 'dfs_list'
for (file in files) {
  # print(file)
  
  ## Read dataset, filter unused rows and columns ---
  tmp <- 
    # Read file
    read.csv(file = file) %>% 
    # Get rid of lines only useful for uploading to 'eBird' (Graeme's template)
    dplyr::filter(X != "Start Time", X != "State", X != "Country", 
                  X != "Protocol", X != "Num Observers", X != "Duration (min)", 
                  X != "All Obs Reported (Y/N)", X != "Dist Traveled (km)", 
                  X != "Area Covered (Acres)") %>% 
    # Keep only columns with data while in movement 
    # (i.e. exclude 'stationary' counts, such as CTD stations)
    dplyr::select(X, tidyr::starts_with("TaiaroaEast")) %>% 
    dplyr::select(where(~ !any(is.na(.))))
  
  ## Here we need to break down the process to check if there are any species ---
  ## names that are duplicated before reshaping the data ---
  
  # Get duplicated taxa
  duplicated_vars <- 
    plyr::count(tmp$X) %>% dplyr::filter(freq > 1) %>% dplyr::pull(x)
  
  # Select only species (i.e. remove 'header')
  spp <- 
    tmp[5:nrow(tmp), ] %>% 
    janitor::remove_empty(which = "cols")
  
  # Fix values
  spp <- 
    spp %>% 
    # Filter only duplicated taxa
    dplyr::filter(X %in% duplicated_vars)  %>%
    # Transform the columns to numeric to apply 'sum' function
    dplyr::mutate(across(colnames(spp[, -1]), as.numeric)) %>% 
    # Group by taxa
    dplyr::group_by(X) %>% 
    # Sum values
    dplyr::summarise(across(starts_with("TaiaroaEast"), ~sum(.x))) %>%
    # Back to 'character' to allow 'bind_rows' below
    dplyr::mutate(across(colnames(spp[, -1]), as.character))
  
  # Bind rows back to 'tmp'
  tmp <-
    tmp %>% 
    filter(! (X %in% duplicated_vars)) %>%
    dplyr::bind_rows(spp)
  
  ## Back to reshaping data --- 
  tmp <- 
    tmp %>% 
    # Transpose the data.frame (which will return a matrix), 
    # and then transform it back to data.frame again
    t() %>% 
    as.data.frame() %>% 
    # Old column 'X', now as a row, to column names 
    janitor::row_to_names(row_number = 1) %>% 
    # Row names to 1st column (transect '5 km bin')
    dplyr::as_tibble(rownames = "TaiaroaEast") %>% 
    # As transects are named with the same name, R included ".1" at the end of
    # the ones on our way 'back' when reading the CSV -- so create a column with this 
    # info (direction: out or back), and clean the names to not have ".1" at the end
    dplyr::mutate(direction = 
                    ifelse(grepl(x = TaiaroaEast, pattern = ".1$") == TRUE, yes = "back", no = "out"),
                  .after = TaiaroaEast) %>% 
    dplyr::mutate(TaiaroaEast = 
                    ifelse(grepl(x = TaiaroaEast, pattern = ".1$") == TRUE, 
                           yes = gsub(pattern = ".1$", replacement = "", x = TaiaroaEast), 
                           no = TaiaroaEast)) %>% 
    # Set all the column names to snake_case with {janitor}
    # This will help later on to standardise all the names as it ensures
    # all capital letter to be lower case, and special characters to be underlines
    janitor::clean_names()
  
  # Create a name label and save the dataset in the above defined list ('dfs_list')
  name <- paste0("munida_", gsub(pattern = "-", replacement = "_",
                                 x = tmp[1, "date"]))
  dfs_list[[name]] <- tmp
  
  ## Print off the duplicated spp names and dates,
  ## so I can correct before sending the CSV back to Graeme
  if(length(duplicated_vars) > 0){
    print(
      paste(name, unique(duplicated_vars))
    )
  }
  
  # Clean environment
  rm("file", "tmp", "spp", "duplicated_vars", "name")
}

# Clean global env.
rm("files")

## Check and standardise species names across datasets ####

## Bind all data into one single data.frame
all_data <- dplyr::bind_rows(dfs_list, .id = "id")

## Get 'spp' colnames, excluding the 'universal' colnames
cols_spp <- colnames(all_data)

cols_spp <- 
  cols_spp[! cols_spp %in% c("id", "taiaroa_east", "direction", "latitude", 
                             "longitude", "date", "notes")]

## Pivot to long format
all_data_long <-
  all_data %>% 
  tidyr::pivot_longer(cols = all_of(cols_spp),
                      names_to = "species",
                      values_to = "count")

## Check species unique names
# sort(unique(all_data_long$species))

## Standardise names across datasets (i.e. voyages)
all_data_long <-
  all_data_long %>% 
  dplyr::mutate(species = dplyr::case_when(
    # typo
    species == "cambell_albatross" ~ "campbell_albatross",
    species == "chatam_mollymawk" ~ "chatham_mollymawk",
    species == "short_tailed_shearwaters" ~ "short_tailed_shearwater",
    species == "souther_giant_petrel" ~ "southern_giant_petrel",
    species == "gannet" ~ "australasian_gannet",
    species == "dive_petrel" ~ "diving_petrel",
    # in NZ people call Cape Petrel as 'Cape Pigeon', 
    # change it to a worldwide recognisable common name
    species == "cape_pigen" ~ "cape_petrel", # + typo 'pigen' == 'pigeon'
    species == "cape_pigeon_southern" ~ "cape_petrel",
    species == "cape_pigeon" ~ "cape_petrel",
    # down-grade to 'wandering_albatross'
    species == "gibsons_albatross" ~ "wandering_albatross",
    # down-grade to 'group'
    species == "black_and_white_shearwater" ~ "unknown_shearwater",
    species == "cooklaria" ~ "unknown_gadfly",
    # change all 'groups' to 'unknown_*'
    species == "gadfly" ~ "unknown_gadfly",
    species == "giant_petrel" ~ "unknown_giant_petrel",
    species == "mollymawk" ~ "unknown_mollymawk",
    species == "petrel" ~ "unknown_petrel",
    species == "prion" ~ "unknown_prion",
    species == "shearwater" ~ "unknown_shearwater",
    species == "skua_stercorarius_sp" ~ "unknown_stercorarius_skua",
    species == "stercorarius_skua" ~ "unknown_stercorarius_skua",
    species == "storm_petrel" ~ "unknown_storm_petrel",
    species == "tern" ~ "unknown_tern",
    TRUE ~ species
  ))

## Check
# sort(unique(all_data_long$species)) # --- OK!

## Remove taxa that are not seabirds (i.e. 'godwit' and 'marine mammals')
all_data_long <-
  all_data_long %>% 
  dplyr::filter(! species %in% c("godwit", 
                                 "dusky_dolphin", "fur_seals", 
                                 "hectors_dolphins", "pilot_whales"))

## Clean global env.
rm("dfs_list", "all_data", "cols_spp")

## Check and filter TaiaroaEast 5 km transects across datasets ####

## Depending on the projects collecting samples on the way 'out' and
## minus/plus a couple of km's at the end of the transect, to keep it standard,
## we will only use data from `0 to 60` km away from Taiaroa Head

## Check the names to know what to filter away
unique(all_data_long$taiaroa_east)

all_data_long <-
  all_data_long %>% 
  dplyr::filter(! taiaroa_east %in% c("TaiaroaEast60.64km",
                                      "TaiaroaEast60.End",
                                      "TaiaroaEast15km"))

## Check
# unique(all_data_long$taiaroa_east) # --- OK!

## Check notes (and deal with them, if needed) ####
notes <- 
  all_data_long %>% 
  dplyr::group_by(id, taiaroa_east) %>% 
  dplyr::summarise(notes = unique(notes))

# [l.   1] munida_2015_03_10 TaiaroaEast0.5km Still dark – maybe disconsider for analysis
# [l. 264] munida_2020_01_14 TaiaroaEast15.20km Because of the dredge, this strech started at 18 km instead of 15 km
# [l. 309] munida_2020_10_13 TaiaroaEast30.35km Prion flock made up by broad-billed and fairy prions

all_data_long <-
  all_data_long %>% 
  # [l.   1] Too dark
  dplyr::filter(! c(id == "munida_2015_03_10" & 
                      taiaroa_east == "TaiaroaEast0.5km" & 
                      direction == "out")) %>% 
  # [l. 264] Too short (2 km sampled instead of 5 km)
  dplyr::filter(! c(id == "munida_2020_01_14" & 
                      taiaroa_east == "TaiaroaEast15.20km" & 
                      direction == "out"))

## [l. 309] I may need to come back to the 3rd note, as this may imply 2  
## 'species' instead of only a prion 'group'

rm("notes")

## Read data raw ('ebird') and standardise ####

ebird_data <- read.csv("./data-raw/ebird/AllGLebirdJune2022.csv")

## In the 'Location' column we can identify the "Munida study area" given the 
## "TaiaroaEast" string. Also, filter away any 'casual observations'
ebird_data <- 
  ebird_data %>%
  dplyr::filter(grepl("TaiaroaEast", Location)) %>%
  dplyr::filter(! Protocol == "eBird - Casual Observation")

## Check columns to stay
colnames(ebird_data)

cols <- c(
  # Bird/count info
  "Common.Name", "Scientific.Name", "Count", 
  # Geographic & date-time info
  "Location", "Latitude", "Longitude", "Date", "Time", 
  # Additional info to check further
  "Protocol", "All.Obs.Reported", "Number.of.Observers", "Observation.Details", 
  "Checklist.Comments")

ebird_data <- ebird_data[, colnames(ebird_data) %in% cols]

rm("cols")

## Check dates and cross-check with Munida dates archive
sort(unique(as.Date(ebird_data$Date, format = "%d/%m/%Y")))

# "2012-09-04", "2017-07-14", "2019-05-23", "2019-07-23" # --- These are Munida

ebird_data <- 
  ebird_data %>% 
  dplyr::filter(Date %in% c("4/09/2012",
                            "14/07/2017",
                            "23/05/2019",
                            "23/07/2019"))

## OK; time to make these data look like 'all_data_long' ----------------------#

ebird_data <- 
  ebird_data %>% 
  dplyr::select(Location, Latitude, Longitude, Date, Time, Common.Name, Count)

## 'Date' on the same format as in 'all_data_long'
ebird_data$Date <-
  as.Date(ebird_data$Date, format = "%d/%m/%Y") %>% as.character()

## 'Location' (== 'taiaroa_east') on the same format as in 'all_data_long'
ebird_data$Location <- 
  gsub(pattern = "-", replacement = ".", x = ebird_data$Location)

# Check
# unique(ebird_data$Location)

# Filter transects (/records) after 60 km (see details above in 'all_data_long')
ebird_data <- 
  ebird_data %>% 
  dplyr::filter(! Location %in% c("TaiaroaEast60.64km", "TaiaroaEast64kmEnd"))

# Check again
# unique(ebird_data$Location) # --- OK

## 'Common.Name' (== 'species') on the same format as in 'all_data_long'
ebird_data$Common.Name <- 
  janitor::make_clean_names(string = ebird_data$Common.Name, allow_dupes = TRUE)

# Check spp names
# unique(ebird_data$Common.Name)

ebird_data <- 
  ebird_data %>% 
  dplyr::mutate(species = dplyr::case_when(
    Common.Name == "silver_gull_red_billed" ~ "red_billed_gull",
    Common.Name == "south_black_backed_gull" ~ "black_backed_gull",
    Common.Name == "tern_sp" ~ "unknown_tern",
    Common.Name == "shy_mollymawk" ~ "white_capped_mollymawk",
    Common.Name == "shy_salvins_chatham_mollymawk" ~ "unknown_mollymawk",
    Common.Name == "black_browed_campbell_mollymawk" ~ "unknown_mollymawk",
    Common.Name == "wandering_albatross_gibsons" ~ "wandering_albatross",
    Common.Name == "storm_petrel_sp" ~ "unknown_storm_petrel",
    Common.Name == "southern_northern_giant_petrel" ~ "unknown_giant_petrel",
    Common.Name == "pterodroma_sp" ~ "unknown_gadfly",
    Common.Name == "prion_sp" ~ "unknown_prion",
    Common.Name == "diving_petrel_sp" ~ "diving_petrel",
    Common.Name == "stewart_island_shag" ~ "otago_shag",
    TRUE ~ Common.Name
  ), .before = Count) %>% 
  # Pipit is not a seabird
  dplyr::filter(! Common.Name == "australasian_pipit") %>% 
  # Remove 'Common.Name' now that we've got 'species'
  dplyr::select(- Common.Name)

# Check spp names
# unique(ebird_data$species) # --- OK

## Rename 'Location' to 'TaiaroaEast' and clean_names to snake_case
ebird_data <- 
  ebird_data %>% 
  dplyr::rename(TaiaroaEast = Location) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(id = paste0("munida_", date), .before = everything())

## Create column 'direction' -- a bit of a laborious one, check comments below

aux_table <- 
  ebird_data %>% 
  # Transform 'time' to 'date-time' format
  dplyr::mutate(time = strptime(time, format = "%I:%M %p")) %>% 
  # Get min/max time for each 'taiaroa_east/voyage', and how many times it happened
  # i.e. if there are samples on the way out and back, it should sum to '2', otherwise, '1'
  dplyr::group_by(id, taiaroa_east) %>% 
  dplyr::summarise(out = min(time), 
                   back = max(time), 
                   n = n_distinct(time)) %>% 
  # Long format
  tidyr::pivot_longer(cols = c("out", "back"),
                      names_to = "direction",
                      values_to = "time") %>% 
  # Adjust format of 'time' again
  dplyr::mutate(time = format(as.POSIXct(time), format = "%H:%M")) %>% 
  # Create 'direction' -- 
  # if `n = 2` (in summarise above), it should be right; however, if `n = 1`,
  # it needs a bit of a hack. I checked in the previous steps and all 
  # 'taiaroa_east' before midday were 'out' and all after were 'back'.
  # That's what this doggy sequence of ifelse is doing
  dplyr::mutate(direction = 
                  ifelse(n == 1 & time < format(as.POSIXct(x = "12:00", format = "%H:%M"), format = "%H:%M"),
                         yes = "out", 
                         no = ifelse(n == 1 & time > format(as.POSIXct(x = "12:00", format = "%H:%M"), format = "%H:%M"),
                                     yes = "back", no = direction))) %>% 
  # As we adjusted the rows with `n = 1`, they are now duplicates, so remove filter them
  dplyr::distinct() %>% 
  # Don't need 'n' any more, and `ungroup` this object
  dplyr::select(- n) %>% 
  dplyr::ungroup(.)

ebird_data <- 
  dplyr::left_join(
    # Make sure 'time' is the same class as in 'aux_table'
    (ebird_data %>% 
       dplyr::mutate(time = strptime(time, format = "%I:%M %p")) %>% 
       dplyr::mutate(time = format(as.POSIXct(time), format = "%H:%M"))), 
    aux_table,
    by = c("id", "taiaroa_east", "time"))

# Make sure 'lat/long' are the same class as in 'all_data_long' for next step
ebird_data <-
  ebird_data %>% 
  dplyr::mutate(latitude = as.character(latitude),
                longitude = as.character(longitude))

rm("aux_table")

## Merge both 'digitised' and 'ebird' datasets, add nice English species names (no 'underscore') & save it ####

all_data_long <- 
  # Merge both datasets
  dplyr::bind_rows(all_data_long, ebird_data) %>% 
  ## -- Last few bits of wrangling --
  # Remove time and notes columns
  dplyr::select(-c(time, notes)) %>% 
  # Add year and month columns
  dplyr::mutate(year = as.numeric(stringr::str_sub(date, start = 1, end = -7)),
                month = as.numeric(stringr::str_sub(date, start = 6, end = -4))) %>% 
  # Add season column
  dplyr::mutate(season = dplyr::case_when(
    month <= 03 ~ "summer",
    month > 03 & month <= 06 ~ "autumn",
    month > 06 & month <= 09 ~ "winter",
    month > 09 ~ "spring",
  )) %>% 
  # Replace NA values with 0, and filter them away
  dplyr::mutate(count = tidyr::replace_na(as.numeric(count), 0)) %>% 
  dplyr::filter(! count == 0)

## Add English names without the underscore to ease the process when plotting the data
all_data_long <- 
  all_data_long %>% 
  dplyr::mutate(species_nice_name = dplyr::case_when(
    species == "black_backed_gull" ~ "Black-backed gull",
    species == "red_billed_gull" ~ "Red-billed gull",
    species == "white_capped_mollymawk" ~ "White-capped albatross",
    species == "white_fronted_tern" ~ "White-fronted tern",
    species == "sooty_shearwater" ~ "Sooty shearwater",
    species == "cape_petrel" ~ "Cape petrel",
    species == "southern_royal_albatross" ~ "Southern royal albatross",
    species == "bullers_mollymawk" ~ "Buller's albatross",
    species == "white_chinned_petrel" ~ "White-chinned petrel",
    species == "bullers_shearwater" ~ "Buller's shearwater",
    species == "hutton_fluttering_shearwater" ~ "Hutton's/Fluttering shearwater",
    species == "northern_royal_albatross" ~ "Northern royal albatross",
    species == "salvins_mollymawk" ~ "Salvin's albatross",
    species == "black_browed_mollymawk" ~ "Black-browed albatross",
    species == "fairy_prion" ~ "Fairy prion",
    species == "black_bellied_storm_petrel" ~ "Black-bellied storm petrel",
    species == "campbell_albatross" ~ "Campbell albatross",
    species == "mottled_petrel" ~ "Mottled petrel",
    species == "otago_shag" ~ "Otago shag",
    species == "light_mantled_sooty_albatross" ~ "Light-mantled albatross",
    species == "black_fronted_tern" ~ "Black-fronted tern",
    species == "grey_petrel" ~ "Grey petrel",
    species == "broad_billed_prion" ~ "Broad-billed prion",
    species == "white_headed_petrel" ~ "White-headed petrel",
    species == "spotted_shag" ~ "Spotted shag",
    species == "chatham_mollymawk" ~ "Chatham albatross",
    species == "wilsons_storm_petrel" ~ "Wilson's storm petrel",
    species == "grey_backed_storm_petrel" ~ "Grey-backed storm petrel",
    species == "southern_giant_petrel" ~ "Southern giant petrel",
    species == "northern_giant_petrel" ~ "Northern giant petrel",
    species == "grey_faced_petrel" ~ "Grey-faced petrel",
    species == "soft_plumaged_petrel" ~ "Soft-plumaged petrel",
    species == "white_faced_storm_petrel" ~ "White-faced storm petrel",
    species == "wandering_albatross" ~ "Wandering albatross",
    species == "westland_petrel" ~ "Westland petrel",
    species == "australasian_gannet" ~ "Australasian gannet",
    species == "diving_petrel" ~ "Diving petrel",
    species == "black_shag" ~ "Black shag",
    species == "black_billed_gull" ~ "Black-billed gull",
    species == "antarctic_prion" ~ "Antarctic prion",
    species == "cooks_petrel" ~ "Cook's petrel",
    species == "black_winged_petrel" ~ "Black-winged petrel",
    species == "antarctic_fulmar" ~ "Antarctic fulmar",
    species == "brown_skua" ~ "Brown skua",
    species == "yellow_eye_penguin" ~ "Yellow-eyed penguin",
    species == "blue_penguin" ~ "Blue penguin",
    species == "subantarctic_little_shearwater" ~ "Subantarctic little shearwater",
    species == "south_polar_skua" ~ "South polar skua",
    species == "unknown_petrel" ~ "Unknown petrel",
    species == "unknown_stercorarius_skua" ~ "Unknown (Stercorarius) skua",
    species == "unknown_tern" ~ "Unknown tern", 
    species == "unknown_storm_petrel" ~ "Unknown storm petrel",
    species == "unknown_giant_petrel" ~ "Unknown giant petrel", 
    species == "unknown_shearwater" ~ "Unknown shearwater", 
    species == "unknown_prion" ~ "Unknown prion", 
    species == "unknown_gadfly" ~ "Unknown gadfly",
    species == "unknown_albatross" ~ "Unknown (great) albatross",
    species == "unknown_mollymawk" ~ "Unknown (small) albatross"
  ), .after = species)

write.csv(all_data_long,
          file = "./data-processed/seabird_data_long.csv",
          row.names = FALSE)
