#-------------------------------- Chapter 1 Survey Preprocessing---------------------------
#-Author: A. Rose Benton---------------------------Created: September, 26, 2022-#
#-R Version: 4.1.3---------------------------------Revised:  March, 5, 2023-#

# Loading Required Packages -- run install.packages("pacman") first
pacman::p_load(
  "tidyverse",
  "tidycensus",
  "data.table",
  "dplyr",
  "sf",
  "tigris",
  "sjlabelled",
  "datawizard",
  "dtplyr",
  "haven",
  "forcats",
  "stringr",
  install = FALSE
)


# Load the helper functions

source("../../helpers/svy-dict.R")


##----------------------------------------------------------------------------##
#------------------------------ANES Survey Data------------------------------
##----------------------------------------------------------------------------##

#-----------------------------------2016---------------------------------------


# Load the 2016 ANES Time Series File
anes2016 <-
  read_dta("../..//Data/ANES/2016/anes_timeseries_2016.dta")


anes2016_panel <- anes2016 %>%
  # Transmute the variables of interest
  transmute(
    # Panel ID
    panel_id = V160001_orig,
    # Year of the Survey
    year = 2016,
    # Panel PSU
    psu = V160202,
    # Panel Strata
    strata = V160201,
    # Survey Pre-Election Weight
    weight = V160101,
    # Survey Mode
    svy_mode = V160501,
    # State Fips Code
    state_fips = V163001a,
    # State Postal Abbreviation
    state_postal = V163001b,
    # Congressional District (115th)
    cdist = V163002,
    # Respondent Sex
    sex = factor(
      case_when(
        V161342 %in% 1:2 ~ as.numeric(V161342),
        TRUE ~ as.numeric(V161002)
      ),
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    # Respondent Race
    race = factor(
      V161310x,
      levels = 1:6,
      labels = c(
        "White",
        "Black",
        "Asian or Pacific Islander",
        "Native American/Alaska Native",
        "Hispanic",
        "Two or More Races, Non-Hispanic"
      )
    ),
    # Respondent Education
    educ = factor(
      case_when(
        V161270 %in% 1:8 ~ 1,
        V161270 %in% 9 ~ 2,
        V161270 %in% 10 ~ 3,
        V161270 %in% 11 ~ 4,
        V161270 %in% 12 ~ 5,
        V161270 %in% 13 ~ 6,
        V161270 %in% 14 ~ 7,
        V161270 %in% 15:16 ~ 8
      ),
      levels = 1:8,
      labels = c(
        "Less than High School",
        "High School Graduate",
        "Some College",
        "Vocational Degree",
        "Associate Degree",
        "Bachelor's Degree",
        "Master's Degree",
        "Professional/Doctoral Degree"
      )
    ),
    # Respondent Age
    age = case_when(V161267 < 0 ~ NA_real_,
                    TRUE ~ as.numeric(V161267)),
    # Age recoded into ten categories
    age_cat = factor(
      case_when(
        between(age, 18, 24) ~ 1,
        between(age, 25, 29) ~ 2,
        between(age, 30, 34) ~ 3,
        between(age, 35, 44) ~ 4,
        between(age, 45, 54) ~ 5,
        between(age, 55, 64) ~ 6,
        between(age, 65, 100) ~ 7
      ),
      levels = 1:7,
      labels = c("18-24", #6
                 "25-29", #4
                 "30-34", #4
                 "35-44", #9
                 "45-54", #9
                 "55-64", #9
                 "65+")
    ),
    # Respondent Income (Pre or Post Election)
    income = factor(
      case_when(
        V161361x %in% 1:2 | V162309x %in% 1:2 ~ 1,
        # <$10,000
        V161361x %in% 3:6 |
          V162309x %in% 3:6 ~ 2,
        # $10,000 - $19,999
        V161361x %in% 7:10 |
          V162309x %in% 7:10 ~ 3,
        # $20,000 - $29,999
        V161361x %in% 11:12 |
          V162309x %in% 11:12 ~ 4,
        # $30,000 - $39,999
        V161361x %in% 13:14 |
          V162309x %in% 13:14 ~ 5,
        # $40,000 - $49,999
        V161361x %in% 15:16 |
          V162309x %in% 15:16 ~ 6,
        # $50,000 - $59,999
        V161361x %in% 17:18 |
          V162309x %in% 17:18 ~ 7,
        # $60,000 - $69,999
        V161361x %in% 19:20 |
          V162309x %in% 19:20 ~ 8,
        # $70,000 - $79,999
        V161361x %in% 21:22 |
          V162309x %in% 21:22 ~ 9,
        # $80,000 - $99,999
        V161361x %in% 23:25 |
          V162309x %in% 23:25 ~ 10,
        # $100,000 - $149,999
        V161361x %in% 26:27 |
          V162309x %in% 26:27 ~ 11,
        # $150,000 - $249,999
        V161361x == 28 | V162309x == 28 ~ 12 # $250,000 or more
      ),
      levels = 1:12,
      labels = c(
        "<$10,000",
        "$10,000 - $19,999",
        "$20,000 - $29,999",
        "$30,000 - $39,999",
        "$40,000 - $49,999",
        "$50,000 - $59,999",
        "$60,000 - $69,999",
        "$70,000 - $79,999",
        "$80,000 - $99,999",
        "$100,000 - $149,999",
        "$150,000 - $249,999",
        "$250,000 or more"
      )
    ),
    # Respondent Partisan Identification
    pre_partisan = factor(
      V161158x,
      levels = 1:7,
      labels = c(
        "Strong Democrat",
        "Democrat",
        "Independent-Democrat",
        "Independent",
        "Independent-Republican",
        "Republican",
        "Strong Republican"
      )
    ),
    #flip coding on linked fate variables
    h_lfate = factor(
      case_when(V162224 == 1 ~ 4,
                V162224 == 2 ~ 3,
                V162224 == 3 ~ 2,
                V162224 == 4 ~ 1),
      levels = 1:4,
      labels = c("Not at all",
                 "Not very much",
                 "Some",
                 "A lot"),
      ordered = TRUE
    ),
    b_lfate = factor(
      case_when(V162225 == 1 ~ 4,
                V162225 == 2 ~ 3,
                V162225 == 3 ~ 2,
                V162225 == 4 ~ 1),
      levels = 1:4,
      labels = c("Not at all",
                 "Not very much",
                 "Some",
                 "A lot"),
      ordered = TRUE
    ),
    a_lfate = factor(
      case_when(V162226 == 1 ~ 4,
                V162226 == 2 ~ 3,
                V162226 == 3 ~ 2,
                V162226 == 4 ~ 1),
      levels = 1:4,
      labels = c("Not at all",
                 "Not very much",
                 "Some",
                 "A lot"),
      ordered = TRUE
    ),
    #congressional district
    district = as.character(V161010f)
  )



anes2016 <- anes2016_panel %>%
  mutate(
    #combine linked fate into one vector
    lfate = factor(
      case_when(
        race == "Black" ~ b_lfate,
        race == "Asian or Pacific Islander" ~ a_lfate,
        race == "Hispanic" ~ h_lfate
      ),
      ordered = TRUE
    ),
    year = 2016,
    #add 0 before fips and district #s <10
    state_fips = case_when(
      nchar(state_fips) == 1 ~ paste(0, state_fips, sep = ""),
      TRUE ~ as.character(state_fips)
    ),
    district = case_when(
      nchar(district) == 1 ~ paste(0, district, sep = ""),
      TRUE ~ as.character(district)
    ))%>%
  drop_na(lfate) %>%
  select(-b_lfate, -h_lfate, -a_lfate, -cdist)

# write_rds(anes2016, "anes2016_fp_specs.rds")

anes2016 <- read_rds("anes2016_fp_specs.rds")


# Build a data dictionary object
anes2016_dict <- tibble(
  var_name = colnames(anes2016),
  var_lab = get_label(anes2016),
  var_vals = get_values(anes2016),
  val_labs = get_labels(anes2016)
)

#----------------------------------2020---------------------------------------

# Load the 2020 ANES Time Series File
anes2020 <-
  read_csv("../../Data/ANES/2020/anes_timeseries_2020.csv")


# Pre-Election Wave
anes2020_panel <- anes2020 %>%
  # Transmute the variables of interest
  transmute(
    # Panel ID
    panel_id = V200001,
    #in_panel
    in_panel = V160001_orig,
    # Panel PSU
    psu = V200010c,
    # Panel Strata
    strata = V200010d,
    # Panel Survey Pre-Election Weight
    weight = V200010b,
    # Survey Mode
    svy_mode = V200002,
    # State Fips Code
    state_fips = V203000,
    # State Postal Abbreviation
    state_postal = V203001,
    # Congressional District (117th)
    cdist = V203002,
    # Respondent Sex
    sex = factor(
      V201600,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    # Respondent Race
    race = factor(
      V201549x,
      levels = 1:6,
      labels = c(
        "White",
        "Black",
        "Hispanic",
        "Asian or Pacific Islander",
        "Native American/Alaska Native",
        "Two or More Races, Non-Hispanic"
      )
    ),
    # Respondent Education
    educ = factor(
      V201510,
      levels = 1:8,
      labels = c(
        "Less than High School",
        "High School Graduate",
        "Some College",
        "Vocational Degree",
        "Associate Degree",
        "Bachelor's Degree",
        "Master's Degree",
        "Doctoral Degree"
      )
    ),
    # Respondent Age
    age = na_if(V201507x, -9),
    # Age recoded into ten categories
    age_cat = factor(
      case_when(
        between(age, 18, 24) ~ 1,
        between(age, 25, 29) ~ 2,
        between(age, 30, 34) ~ 3,
        between(age, 35, 44) ~ 4,
        between(age, 45, 54) ~ 5,
        between(age, 55, 64) ~ 6,
        between(age, 65, 100) ~ 7
      ),
      levels = 1:7,
      labels = c("18-24", #6
                 "25-29", #4
                 "30-34", #4
                 "35-44", #9
                 "45-54", #9
                 "55-64", #9
                 "65+")
    ),

    # Respondent Income (Pre or Post Election)
    income = factor(
      case_when(
        V202468x == 1 ~ 1,
        # <$10,000
        V202468x %in% 2:3 ~ 2,
        # $10,000 - $19,999
        V202468x %in% 4:5 ~ 3,
        # $20,000 - $29,999
        V202468x %in% 6:7 ~ 4,
        # $30,000 - $39,999
        V202468x %in% 8:9 ~ 5,
        # $40,000 - $49,999
        V202468x == 10 ~ 6,
        # $50,000 - $59,999
        V202468x %in% 11:12 ~ 7,
        # $60,000 - $69,999
        V202468x %in% 13:14 ~ 8,
        # $70,000 - $79,999
        V202468x %in% 15:16 ~ 9,
        # $80,000 - $99,999
        V202468x %in% 17:19 ~ 10,
        # $100,000 - $149,999
        V202468x %in% 20:21 ~ 11,
        # $150,000 - $249,999
        V202468x == 22 ~ 12  # $250,000 or more
      ),
      levels = 1:12,
      labels = c(
        "<$10,000",
        "$10,000 - $19,999",
        "$20,000 - $29,999",
        "$30,000 - $39,999",
        "$40,000 - $49,999",
        "$50,000 - $59,999",
        "$60,000 - $69,999",
        "$70,000 - $79,999",
        "$80,000 - $99,999",
        "$100,000 - $149,999",
        "$150,000 - $249,999",
        "$250,000 or more"
      )
    ),
    # Respondent Partisan Identification
    pre_partisan = factor(
      V201231x,
      levels = 1:7,
      labels = c(
        "Strong Democrat",
        "Democrat",
        "Independent-Democrat",
        "Independent",
        "Independent-Republican",
        "Republican",
        "Strong Republican"
      )
    ),
    #flip coding on linked fate variables
    h_lfate = factor(
      case_when(V202506 == 1 ~ 4,
                V202506 == 2 ~ 3,
                V202506 == 3 ~ 2,
                V202506 == 4 ~ 1),
      levels = 1:4,
      labels = c("Not at all",
                 "Not very much",
                 "Some",
                 "A lot"),
      ordered = TRUE
    ),
    b_lfate = factor(
      case_when(V202507 == 1 ~ 4,
                V202507 == 2 ~ 3,
                V202507 == 3 ~ 2,
                V202507 == 4 ~ 1),
      levels = 1:4,
      labels = c("Not at all",
                 "Not very much",
                 "Some",
                 "A lot"),
      ordered = TRUE
    ),
    a_lfate = factor(
      case_when(V202508 == 1 ~ 4,
                V202508 == 2 ~ 3,
                V202508 == 3 ~ 2,
                V202508 == 4 ~ 1),
      levels = 1:4,
      labels = c("Not at all",
                 "Not very much",
                 "Some",
                 "A lot"),
      ordered = TRUE
    ),
    #congressional district
    district = as.character(V203002)
  )



anes2020 <- anes2020_panel %>%
  mutate(
    #combine linked fate into one vector
    lfate = factor(
      case_when(
        race == "Black" ~ b_lfate,
        race == "Asian or Pacific Islander" ~ a_lfate,
        race == "Hispanic" ~ h_lfate
      ),
      ordered = TRUE
    ),
    year = 2020,
    #add 0 before fips and district #s <10
    state_fips = case_when(
      nchar(state_fips) == 1 ~ paste(0, state_fips, sep = ""),
      TRUE ~ as.character(state_fips)
    ),
    district = case_when(
      nchar(district) == 1 ~ paste(0, district, sep = ""),
      TRUE ~ as.character(district)
    )) %>%
  #merge into geoid
  unite("GEOID", state_fips, district, sep = "", remove = FALSE) %>%
  drop_na(lfate) %>%
  select(-b_lfate, -h_lfate, -a_lfate, -cdist)

write_rds(anes2020, "anes2020_fp_specs.rds")

anes2020 <- read_rds("anes2020_fp_specs.rds")

# # Build a data dictionary object
# anes2020_dict <- tibble(
#   var_name = colnames(anes2020),
#   var_lab = get_label(anes2020),
#   var_vals = get_values(anes2020),
#   val_labs = get_labels(anes2020)
# )


##----------------------------------------------------------------------------##
#------------------------Leg-Dist Centroid Point Geom-----------------------------
##----------------------------------------------------------------------------##
# 
# # load house districts from tigris api
# house_districts_16 <-
#   congressional_districts(year = 2016) %>%
#   select(STATEFP:GEOID, INTPTLAT:geometry) %>%
#   transmute(
#     state_fip = STATEFP,
#     district = CD115FP,
#     GEOID = GEOID,
#     latitude = INTPTLAT,
#     longitude = INTPTLON,
#     geometry = geometry
#   )
# 
# #turn into sf
# house_sf_16 <- st_as_sf(house_districts_16)
# 
# #find centerpoint point geometry
# centers_16 <- st_centroid(house_sf_16) %>%
#   select(GEOID, latitude, longitude)
# 
# #join with anes
# anes2016_df <- left_join(anes2016, centers_16, by = "GEOID")
# 
# write_rds(anes2016_df, file = "anes_2016_centers.rds")

anes2016_df <- read_rds("anes_2016_centers.rds")

# 
# # load house districts from tigris api
# house_districts_20 <-
#   congressional_districts() %>%
#   select(STATEFP:GEOID, INTPTLAT:geometry) %>%
#   transmute(
#     state_fip = STATEFP,
#     district = CD116FP,
#     GEOID = GEOID,
#     latitude = INTPTLAT,
#     longitude = INTPTLON,
#     geometry = geometry
#   )
# 
# #turn into sf
# house_sf_20 <- st_as_sf(house_districts_20)
# 
# #find centerpoint point geometry
# centers_20 <- st_centroid(house_sf_20) %>%
#   select(GEOID, latitude, longitude)
# 
#  #join with anes
# anes2020_df <- left_join(anes2020, centers_20, by = "GEOID")
# 
# write_rds(anes2020_df, file = "anes_2020_centers.rds")

anes2020_df <- read_rds("anes_2020_centers.rds")
# 
# full_anes_df <- bind_rows(anes2016_df, anes2020_df)

# 
# write_rds(full_anes_df, file = "full_anes_centers.rds")

full_anes_df <- read_rds("full_anes_centers.rds")
##----------------------------------------------------------------------------##
#----------------------------------NPL data ------------------------------------
##----------------------------------------------------------------------------##
# #load npl data
# npl_df <- read_rds("npl_df_adjusted.rds")
# 
# npl_cdist_temp <- npl_df %>%
#   select(epe_id, list_date, state_name, hrs_score, geometry)
# 
# #download fips codes from tigris
# fips_codes <- fips_codes %>% 
#   select(state_name, state_code)
# 
# #join with npl
# npl_fips_temp <- left_join(npl_cdist_temp, fips_codes, by = "state_name") %>%
#   mutate(
#     epa_id = epe_id
#   ) %>% 
#   unique()
# 
# npl_fips <- npl_fips_temp %>%
#   st_as_sf()%>%
#   st_centroid() 
# 
# #df of open sites in 2016    
# temp_16 <- npl_fips %>%
#   filter(list_date < 2016 - 01 - 01)
# 
# #df of open sites in 2020
# temp_20 <- npl_fips 
# 
# #join with house districts
# npl_2016 <- st_join(temp_16, house_districts_16)

write_rds(npl_2016, "npl_2016.rds")

# npl_2020 <- st_join(temp_20, house_districts_20)

write_rds(npl_2020, "npl_2020.rds")

##----------------------------------------------------------------------------##
#-----------------------------------Proximity----------------------------------
##----------------------------------------------------------------------------##
# npl_anes_16 <- npl_2016 %>%
#   #
#   drop_na(district) %>%
#   #
#   filter(state_code %in% unique(anes2016_df$state_fips)) %>%
#   #
#   rename(npl_coords = geometry,
#          state_fips = state_code) %>% 
#   as_tibble()
# 
# #
# anes_centroids_2016 <- anes2016_df %>%
#   #
#   distinct(state_fips, district, .keep_all = TRUE) %>%
#   #
#   arrange(state_fips, district) %>%
#   #
#   rename(center = geometry) %>%
#   #
#   as_tibble()
# 
# center_by_sites_16 <- inner_join(
#   anes_centroids_2016,
#   npl_anes_16,
#   by = "state_fips",
#   suffix = c("_anes", "_npl")
# ) %>%
#   #
#   mutate(
#     in_district = (district_anes == district_npl),
#     dist_from_center = st_distance(center, npl_coords, by_element = TRUE),
#     km_from_center = as.numeric(dist_from_center / 1000),
#     hrs_score = case_when(
#       is.na(hrs_score) ~ 28.5,
#       TRUE ~ hrs_score
#     )
#   ) %>%
#   #
#   group_by(state_fips, district_anes) %>%
#   #
#   summarise(
#     across(
#       dist_from_center:km_from_center,
#       list(
#         min = ~ min(.x, na.rm = TRUE),
#         max = ~ max(.x, na.rm = TRUE),
#         mean = ~ mean(.x, na.rm = TRUE)
#       )
#     ),
#     total_district = sum(in_district, na.rm = TRUE),
#     in_district = max(in_district, na.rm = TRUE),
#     avg_hrs = mean(hrs_score)
#   )
# 
# anes_npl_proximity_16 <-
#   left_join(
#     anes_centroids_2016,
#     center_by_sites_16,
#     by = c("state_fips" = "state_fips", "district" = "district_anes")
#   )
# 
# write_rds(anes_npl_proximity_16, "anes_npl_proximity_16.rds")

anes_npl_proximity_16 <- read_rds("anes_npl_proximity_16.rds")

# 
# npl_anes_20 <- npl_2020 %>%
#   #
#   drop_na(district) %>%
#   #
#   filter(state_code %in% unique(anes2020_df$state_fips)) %>%
#   #
#   rename(npl_coords = geometry,
#          state_fips = state_code) %>%
#   #
#   as_tibble()
# 
# #
# anes_centroids_2020 <- anes2020_df %>%
#   #
#   distinct(state_fips, district, .keep_all = TRUE) %>%
#   #
#   arrange(state_fips, district) %>%
#   #
#   rename(center = geometry) %>%
#   #
#   as_tibble()
# 
# center_by_sites_20 <- inner_join(
#   anes_centroids_2020,
#   npl_anes_20,
#   by = "state_fips",
#   suffix = c("_anes", "_npl")
# ) %>%
#   #
#   mutate(
#     in_district = (district_anes == district_npl),
#     dist_from_center = st_distance(center, npl_coords, by_element = TRUE),
#     km_from_center = as.numeric(dist_from_center / 1000),
#     hrs_score = case_when(
#        is.na(hrs_score) ~ 28.5,
#        TRUE ~ hrs_score
#     )
#   ) %>%
#   #
#   group_by(state_fips, district_anes) %>%
#   #
#   summarise(
#     across(
#       dist_from_center:km_from_center,
#       list(
#         min = ~ min(.x, na.rm = TRUE),
#         max = ~ max(.x, na.rm = TRUE),
#         mean = ~ mean(.x, na.rm = TRUE)
#       )
#     ),
#     total_district = sum(in_district, na.rm = TRUE),
#     in_district = max(in_district, na.rm = TRUE),
#     avg_hrs = mean(hrs_score)
#   )
# 
# anes_npl_proximity_20 <-
#   left_join(
#     anes_centroids_2020,
#     center_by_sites_20,
#     by = c("state_fips" = "state_fips", "district" = "district_anes")
#   )
# 
# write_rds(anes_npl_proximity_20, "anes_npl_proximity_20.rds")

anes_npl_proximity_20 <- read_rds("anes_npl_proximity_20.rds")



anes_npl_proximity_full <- bind_rows(anes_npl_proximity_16, anes_npl_proximity_20) %>% 
  select(year, state_fips, district, dist_from_center_min:in_district, avg_hrs)


full_anes_npl <- left_join(full_anes_df, anes_npl_proximity_full) %>% 
  filter(state_fips != 11)

write_rds(full_anes_npl, "anes_npl_full.rds")

