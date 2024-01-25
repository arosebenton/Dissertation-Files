#---------------------Spatial Visualization of EPA Superfund sites--------------
#-Author: A. Rose Benton---------------------------------Created: July 17, 2022-#
#-R Version: 4.1.3---------------------------------Revised: September, 25, 2022-# 



# Load the necessary libraries----
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "sf",
  "tigris",
  install = FALSE
)


# Load the helper functions
source("../../helpers/plot-themes.R")


#-----------------------------shape file build----------------------------------

#load the location data
npl <- readxl::read_xlsx("NPL_List(2).xlsx") %>% 
  transmute(
    site_name = `Site Name`, 
    epe_id = `Site EPA ID`,
    list_date = `Listing Date`,
    hrs_score = `Site Score`,
    fed = `Federal\r\nFacility\r\nIndicator`,
    city = City
    )

counties <- counties(cb = TRUE) %>% 
  mutate(county_name = NAME,
         state_name = STATE_NAME)

cities <- read_csv("uscities.csv") %>% 
  mutate(city = city, 
         state = state_name,
         county = county_name)

npl_temp <- npl %>% full_join(cities, by = "city", "state")

npl_df <- npl_temp %>% full_join(counties, by = "county") %>% 
  drop_na(site_name) %>% 
  distinct(site_name, list_date, .keep_all = TRUE) 
  
npl_df$list_date = as.character.Date(npl_df$list_date)

# temp <- npl_df %>% select(site_name:county_name)
# 
# write_csv(temp, "temp.csv")

temp <- read.csv("temp_altered.csv")
 temp$list_date = as.character.Date(temp$list_date)

npl_df <- temp %>% 
  full_join(counties, by = c("county_name", "state_name")) %>% 
  drop_na(site_name, county_name, geometry) %>% 
  distinct(site_name, list_date, .keep_all = TRUE) %>% 
  select(site_name:county_name, GEOID, geometry)


write_rds(npl_df, "npl_df_adjusted.rds")


