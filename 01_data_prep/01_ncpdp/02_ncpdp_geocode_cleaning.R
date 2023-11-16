# NCPDP check geocoded data
# MSahu
# Oct 23, 2023

# ------------------------------------------------------------------------------

# SETUP

rm(list=ls())
pacman::p_load(tidyr, dplyr, ggplot2, sf, maps, mapdata, usmap)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")
out_dir <- paste0(data_dir, "geocoded_counties/")

# Read NCPDP data with geocoded addresses

ncpdp <- readRDS(paste0(data_dir, "ncpdp_with_coords.rds")) %>% 
  mutate(county_fips = county_fips.x) %>%  select(-county_fips.x, -county_fips.y) %>% 
  select(ncpdp_id, legal_name, ncpdp_name, state_code, county_fips, everything())

# NOTE : This file actually has an error in the way that the "active2018" to "active2021" variables are created
# The correct variables are added back at the end , as well as the variables including openings

# ------------------------------------------------------------------------------

# READ AND FORMAT DATA
 
ncpdp <- ncpdp %>% 
  
  mutate(Coordinates = I(coordinates)) %>% 
  mutate(Coordinates = gsub("c\\(|\\)", "", Coordinates)) %>% 
  separate(Coordinates, into = c("LONG", "LAT"), sep = ", ") 

# ------------------------------------------------------------------------------

# MANUALLY ADD MISSING ADDRESSES (FROM GOOGLE)

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "5616721", "41.48831616677966", LAT),
    LONG = ifelse(ncpdp_id == "5616721", "-120.54193061534421", LONG),
    
    LAT = ifelse(ncpdp_id == "5627596", "37.49213241267396", LAT),
    LONG = ifelse(ncpdp_id == "5627596", "-119.97413151826365", LONG),
    
    LAT = ifelse(ncpdp_id == "5631898", "37.48862509412614", LAT),
    LONG = ifelse(ncpdp_id == "5631898", "-119.97206153360783", LONG),
    
    LAT = ifelse(ncpdp_id == "5653161", "37.74894901592902", LAT),
    LONG = ifelse(ncpdp_id == "5653161", "-119.58268174781166", LONG),
    
    LAT = ifelse(ncpdp_id == "5662312", "41.494530602369615", LAT),
    LONG = ifelse(ncpdp_id == "5662312", "-120.55367284312192", LONG)
    
    ) %>% 
  
arrange(state_code)

# check <- ncpdp %>% filter(is.na(LAT))
# check <- ncpdp %>% filter(ncpdp_id %in% c("0100052", "5616721", "5627596", "5631898"))


# ------------------------------------------------------------------------------

# Convert to SF

ncpdp_sf <- st_as_sf(ncpdp, 
                     coords = c("LONG", "LAT"), 
                     crs = 4326)  # tells R that these coords should be interpreted as degrees on a sphere (WGS84)

ncpdp_numeric <- ncpdp %>% mutate(LONG = as.numeric(LONG), LAT = as.numeric(LAT))

# Shift the longitude for Alaska
#ncpdp_numeric$LONG[ncpdp_numeric$state_code == "AK"] <- ncpdp_numeric$LONG[ncpdp_numeric$state_code == "AK"] + 360

ncpdp_map <- usmap_transform(ncpdp_numeric, 
                             input_names = c("LONG", "LAT"),
                             output_names = c("x", "y")) 

# ------------------------------------------------------------------------------

# LOOP THROUGH EACH STATE AND MAP

# map function

state <- us_map(regions = "states") # boundaries from usmap package

state_map <- function(state_code) {
  
  data = ncpdp_map %>% filter(state_code == my_state_code)
  
  state_map <- ggplot() +
    
    geom_polygon(data = state, aes(x = x, y = y, group = group), fill = NA, color = "black") +
    
    geom_point(data = data, aes(x = x, y = y), color = "red", size = 1) +
    
    labs(title = paste0(my_state_code)) + 

    theme_void() +
    
    theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height=unit(1.35,"cm"),
        legend.text=element_text(size=6),
        plot.caption = element_text(hjust = 0)) 
    
return(state_map)
  
}


# loop and output

pdf(paste0(data_dir, "geocoding_cleaning/byState_map_pharmacies_prior_to_cleaning.pdf"), width = 12, height = 8, onefile = TRUE) 

  for(i in 1:length(unique(ncpdp$state_code))){
    
    my_state_code <- unique(ncpdp$state_code)[i]
    
    print(state_map(my_state_code))
    
  }
  
dev.off()

# ------------------------------------------------------------------------------

# FIX ALASKA [DONE IN QGIS]

# alaska <- read.csv(paste0(data_dir, "ncpdp_cleaned_with_coords.csv")) %>% 
#   filter(state_code == "AK") 
# 
# write.csv(alaska, file = paste0(data_dir, "geocoding_cleaning/ncpdp_cleaned_with_coords_AK_ONLY.csv"), row.names = F)
# 
# alaska_updated <- read.csv(paste0(data_dir, "geocoding_cleaning/ncpdp_cleaned_with_coords_AK_ONLY_updated.csv")) %>% 
#   mutate(LONG = lon, LAT = lat) %>% select(-c("lat", "lon")) %>% 
#   mutate(ncpdp_id = as.character(ncpdp_id))

alaska_qgis <- st_read(paste0(data_dir, "geocoding_cleaning/qgis_AK.shp")) %>% 
  separate(latlong, into = c("LAT", "LONG"), sep = ",") %>% 
  select(-c(result_num, status, formatted_, place_id, location_t)) %>% 
  st_drop_geometry() %>% 
  mutate(ncpdp_int = as.integer(ncpdp_id)) %>% 
  select(ncpdp_int, LONG, LAT) 

alaska_ncpdp <- ncpdp %>% filter(state_code == "AK") %>% select(-c(LAT, LONG)) %>%  
  mutate(ncpdp_int = as.integer(ncpdp_id)) %>% 
  left_join(alaska_qgis, by = "ncpdp_int") %>%
  select(-c(coordinates, ncpdp_int))

# check <- alaska_updated %>% select(ncpdp_id, LAT, LONG) %>% 
#   left_join(alaska_qgis, by = "ncpdp_id") %>% 
#   filter(LAT.x != "no_result" & LONG.x!="no_result") %>%  select(ncpdp_id, LAT.x, LAT.y, LONG.x, LONG.y) %>% 
#   mutate(diff.LAT = as.numeric(LAT.y) - as.numeric(LAT.x), 
#          diff.LONG = as.numeric(LONG.x) -as.numeric(LONG.y))  # THESE VERSIONS ARE SIMILAR BUT NOT IDENTICAL -- USE QGIS


ncpdp <- ncpdp %>% filter(state_code != "AK") %>% select(-coordinates) 

# rbind
ncpdp <- ncpdp %>% rbind(alaska_ncpdp) %>% arrange(state_code)

# ------------------------------------------------------------------------------

# CLEAN IF POINT FALLS OUTSIDE STATE

# California - 2 wrong coordinates with 1 address [937 FRANKLIN BLVD, LEMOORE, CA, 93246]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "5651561", "36.264145237505055", LAT),
    LONG = ifelse(ncpdp_id == "5651561", "-119.89323808940162", LONG),
    
    LAT = ifelse(ncpdp_id == "5654834", "36.264145237505055", LAT),
    LONG = ifelse(ncpdp_id == "5654834", "-119.89323808940162", LONG)
  ) 

# MA - 1 wrong [KAEHLER MEMORIAL MEDICAL CLINIC, CAPE COD, MA, 02542]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "2239792", "41.66047176025008", LAT),
    LONG = ifelse(ncpdp_id == "2239792", "-70.56843687386238", LONG)

  ) 

# Maryland - 1 wrong [1071 MD 3 NORTH, GRAM BILLS, MD, 21054]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "2140363", "39.02573492239841", LAT),
    LONG = ifelse(ncpdp_id == "2140363", "-76.68894797444916", LONG)
    
  ) 

# Hawaii - 4 wrong

ncpdp <- ncpdp %>% 
  
  mutate(
    
    #15-2660 Pahoa Village Rd, Pahoa, HI, 96778
    LAT = ifelse(ncpdp_id == "1242039", "19.50507774449108", LAT), 
    LONG = ifelse(ncpdp_id == "1242039", "-154.9576995744786", LONG),
    
    #BLDG 676, SCHOFIELD, HI, 96857
    LAT = ifelse(ncpdp_id == "1241518", "21.49702028075009", LAT),
    LONG = ifelse(ncpdp_id == "1241518", "-158.0580548044041", LONG),
    
    #200 W KAWILI, HILO, HI, 96720
    LAT = ifelse(ncpdp_id == "1241378", "19.702038096088973", LAT),
    LONG = ifelse(ncpdp_id == "1241378", "-155.07925726284063", LONG),
    
    #74-5465 KAMAKA'EHA AVENUE, KONA, HI, 96740
    LAT = ifelse(ncpdp_id == "1203291", "19.653080558632347", LAT),
    LONG = ifelse(ncpdp_id == "1203291", "-155.99905293400496", LONG)
    
  ) 

# Illinois - 1 wrong [NORTHERN ILLINOIS UNIVERSITY, DE KALB, IL, 60115]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "1421318", "41.9352874259395", LAT),
    LONG = ifelse(ncpdp_id == "1421318", "-88.77345796036148", LONG)
    
  ) 

# New York - 1 wrong [BLDG 606, WEST POINT, NY, 10996]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "3308221", "41.37990637858621", LAT),
    LONG = ifelse(ncpdp_id == "3308221", "-73.96397225805795", LONG)
    
  ) 

# NE - 1 wrong [BLDG 107 RM 101, OFFUTT, NE, 68113]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "2815718", "41.12572797218761", LAT),
    LONG = ifelse(ncpdp_id == "2815718", "-95.91566435364531", LONG)
    
  ) 

# MS - 1 wrong [201 INDEPENDENCE DR BLDG 1100, COLUMBUS AFB, MS, 39710]

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LAT = ifelse(ncpdp_id == "2520179", "33.62587191011822", LAT),
    LONG = ifelse(ncpdp_id == "2520179", "-88.45366363366908", LONG)
    
  ) 

# ------------------------------------------------------------------------------

# Convert to SF

ncpdp_sf <- st_as_sf(ncpdp, 
                     coords = c("LONG", "LAT"), 
                     crs = 4326)  # tells R that these coords should be interpreted as degrees on a sphere (WGS84)

ncpdp_numeric <- ncpdp %>% mutate(LONG = as.numeric(LONG), LAT = as.numeric(LAT))

ncpdp_map <- usmap_transform(ncpdp_numeric, 
                             input_names = c("LONG", "LAT"),
                             output_names = c("x", "y")) 

# ------------------------------------------------------------------------------

# LOOP CLEANED DATA AND SAVE

pdf(paste0(data_dir, "geocoding_cleaning/byState_map_pharmacies_cleaned.pdf"), width = 12, height = 8, onefile = TRUE) 

for(i in 1:length(unique(ncpdp$state_code))){
  
  my_state_code <- unique(ncpdp$state_code)[i]
  
  print(state_map(my_state_code))
  
}

dev.off()

# ------------------------------------------------------------------------------

# Add back the correct cleaned variables

ncpdp_geo_only <- ncpdp %>% select(ncpdp_id, address, LONG, LAT)

ncpdp_updated <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds")) %>% left_join(ncpdp_geo_only, by = "ncpdp_id")

# ------------------------------------------------------------------------------

# SAVE

saveRDS(ncpdp_updated, paste0(data_dir, "ncpdp_with_coords_CLEAN.rds")) 
