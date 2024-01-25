#--------------Chapter 2 Average Model Effects Calculation-------------
#-Author: A. Rose Benton--------------------------------Created: February 3, 2023 -#
#-R Version: 4.1.2--------------------------------Last Modified: February 17, 2022-#



#devtools::install_github("fate-ewi/bayesdfa")


# Load the necessary libraries----
pacman::p_load(
  "sf", # Simple Features for R
  "haven", # Import and Export 'SPSS', 'Stata' and 'SAS' Files
  "sp", #Classes and Methods for Spatial Data
  "sjlabelled", #Labelled Data Utility Functions
  "tidyverse", # Suite of packages for data management
  "maps", #maps data
  "patchwork", # Combining multiple plots into one
  install = FALSE
)


source("../assets/svy-dict.R")


wave6_raw <-
  haven::read_sav(
    "../Data/Chapter 2/merged_r6_data_2016_36countries2.sav",
    encoding = "latin1"
  )

wave6 <- wave6_raw %>%
  separate(                                       
    RESPNO,
    #isolate state abbreviation from longer string
    into = c("state_ab", "respno_delete"),
    sep = 3,
    remove = FALSE
  ) %>%
  mutate(
    wave = 0,
    #create identity factor & center @ 0
    identity = factor(case_when(              
      Q88B == 1 ~ -2,
      Q88B == 2 ~ -1,
      Q88B == 3 ~ 0,
      Q88B == 4 ~ 1,
      Q88B == 5 ~ 2
    ),
      levels = -2:2,
      labels = c(
        "I feel only (ethnic group)",
        "I feel more (ethnic group) than (national identity)",
        "I feel equally (national identity) and (ethnic group)",
        "I feel more (national identity) than (ethnic group)",
        "I feel only (national identity)"
      )),
    #age transformed to reflect decades from 40 for easier interpretation
    age = (Q1 - 40) / 10 ,
    #factorize gender
    female = factor(case_when(
      Q101 == 1 ~ 0,
      Q101 == 2 ~1),
      levels = 0:1,
      labels=c(
        "male",
        "female"
      )),
    #factorize region
    region = factor(
      REGION,
      levels = get_values(REGION),
      labels = get_labels(REGION)
    ),
  ) %>%
  select(RESPNO, state_ab, region, wave, DATEINTR, age, female, identity) %>% 
  separate(DATEINTR, into = c("year", "month", "day"), sep = "-") %>% 
  filter(!state_ab %in% c("ALG", "GAM", "SWZ")) %>% 
  drop_na(identity)


wave7_raw <-
  haven::read_sav(
    "../Data/Chapter 2/r7_merged_data_34ctry.release.sav",
    encoding = "latin1"
  )

wave7 <- wave7_raw %>%
  #isolate state abbreviation from longer string
  separate(
    RESPNO,
    into = c("state_ab", "respno_delete"),
    sep = 3,
    remove = FALSE
  ) %>%
  #factorize identity and center at 0
  mutate(wave = 1,
         identity = factor(case_when(
           Q85B == 1 ~ -2,
           Q85B == 2 ~ -1,
           Q85B == 3 ~ 0,
           Q85B == 4 ~ 1,
           Q85B == 5 ~ 2
         ),
         levels = -2:2,
         labels = c(
           "I feel only (ethnic group)",
           "I feel more (ethnic group) than (national identity)",
           "I feel equally (national identity) and (ethnic group)",
           "I feel more (national identity) than (ethnic group)",
           "I feel only (national identity)"
         )),
         #age transformed to reflect decades from 40 for easier interpretation
         age = (Q1 - 40) / 10,
         #factorize gender
         female = factor(case_when(
           Q101 == 1 ~ 0,
           Q101 == 2 ~1),
           levels = 0:1,
           labels=c(
             "male",
             "female"
           )),
         #factorize region
         region = factor(
           REGION,
           levels = get_values(REGION),
           labels = get_labels(REGION)
         )) %>%
  select(RESPNO, state_ab, region, wave, DATEINTR, age, female, identity) %>% 
  separate(DATEINTR, into = c("year", "month", "day"), sep = "-") %>% 
  filter(!state_ab %in% c("ALG", "GAM", "SWZ")) %>% 
  drop_na(identity)


x <- wave6 %>% 
  filter(year > 2014)

y <- wave7 %>% 
  filter(state_ab %in% x$state_ab)

post_2015 <- bind_rows(x,y) %>% 
  mutate(
    #merge country code formats
    state_ab = case_when(                             
      state_ab == "MOZ" ~ "MZM",
      state_ab == "NGR" ~ "NIG",
      state_ab == "SRL" ~ "SIE",
      state_ab == "CAM" ~ "CAO",
      state_ab == "MAD" ~ "MAG",
      TRUE ~ state_ab
    )
  )

map_afb <- post_2015 %>% 
  group_by(state_ab) %>% 
  summarise(
    n = n()
  )

cc_afb <- map_afb %>% 
  mutate(state = countrycode::countrycode(state_ab, "cowc", "country.name"))







##-----------------------------------------------------------------------------##
#---------------------------------------Map-------------------------------------
##-----------------------------------------------------------------------------##


world <- map_data("world")

#merge
coverage_map <- cc_afb %>%
  full_join(world, by = c("state" = "region")) %>%
  filter(between(lat, -37, 40),
         between(long, -20, 54)) %>% 
  unique()


base_map <-  coverage_map %>%
  #initialize plot object
  ggplot(aes(long, lat)) +
  #add in aesthetic variables (shape/fill)
  geom_polygon(aes(group = group, fill = n)) +
  #color choice
  scale_fill_viridis_c() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.text = element_text(family = "serif", size = 10),
    legend.title = element_text(
      family = "serif",
      size = 10,
      face = "bold"
    )) +
  labs(
    title = "Figure 1: Map of Analysis Coverage",
    x = "",
    y = "",
    fill = "N Respondents"
  )+
  plot_annotation(
    caption = "Analysis includes the following States: Burkina Faso, Cameroon, 
    Gabon, Guinea, Libya, Madagascar, Morocco, Mozambique, Nigeria, South Africa, 	
    Sierra Leone, Sao Tome and Principe and Uganda"
  )

ggsave("output/figures/coverage_map.jpg",
       plot = base_map,
       width = 7.2,
       height = 7)
  