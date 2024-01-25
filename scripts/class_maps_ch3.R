#-------------------Donnelly Data Pre-Processing & maps-------------------------
#-Author: A. Rose Benton---------------------------Created: October, 10 , 2023-#
#-R Version: 4.2.2---------------------------------Revised: October, 10 , 2023-#

# Loading Required Packages -- run install.packages("pacman") first
pacman::p_load(
  "tidyverse",
  "viridis", #color pallette
  "geojsonio",
  "geojsonsf",
  "sf"
)

source("assets/plot-themes.R")

##------------------------------Donnelly data------------------------------

#load donnelly replication data for germany and add country id
germany <- read_csv("data/Chapter 3/donnelly/gerreplication.csv") %>% 
  mutate(
    country = "ger"
  )

#load donnelly replication data for the uk and add country id
uk <- read_csv("data/Chapter 3/donnelly/ukreplication.csv") %>% 
  mutate(
    country = "uk"
  )

#bind dataframes together
full <- rbind(germany, uk) %>% 
  mutate(
    country = factor(
      case_when(
        country == "uk" ~ 0,
        country == "ger" ~ 1
      ), 
      levels = 0:1, 
      labels = c(
        "United Kingdom",
        "Germany"
      )
    ),
    #correct for spelling discrepencies
    shapeName =   case_when(
      region == "Baden-Wurttemberg" ~ "Baden-Württemberg",
      region == "Mecklenburg-Western Pomerania" ~ "Mecklenburg-Vorpommern",
      TRUE ~ region
    )
    )

#get sample sizes for each region
regional <- full %>% 
  group_by(country, shapeName) %>% 
  summarise(
    n = n(),
    mean_lf = mean(classlink, na.rm = TRUE)
  )

#---------------------------------plots--------------------------------------
#histogram of n-respondents by region
n_hist <- ggplot(regional, aes(x = n))+
  geom_histogram(fill = "blue")+
  facet_wrap(regional$country)+
  labs(
    y = "",
    x = "",
    title = "Histogram of Respondent Distribution",
    subtitle = "Range: 18 - 412"
  )

ggsave("n_hist.jpg", plot = n_hist, path = "output/figures") 

#histogram of mean linked fate responses by region
lf_hist <- ggplot(regional, aes(x = mean_lf))+
  geom_histogram(fill = "blue")+
  facet_wrap(regional$country)+
  labs(
    y = "",
    x = "",
    title = "Histogram of Respondent Mean Linked Fate by Region"
  )

ggsave("lf_hist.jpg", plot = lf_hist, path = "output/figures") 
#----------------------------------maps--------------------------------------

ger_geo <- geojson_sf("C:/Users/amber/OneDrive - UNT System/Projects/Dissertation/data/Chapter 3/Germany_geodata/DEU_ADM1.geojson")

#join geodata to donnelly data
ger_map_sf <- left_join(ger_geo, regional, by = "shapeName") %>% 
  select(shapeName, n, mean_lf, geometry)

#map build for n respondents
ggplot()+
  geom_sf(data = ger_map_sf, aes(fill = n))+
  scale_fill_viridis()+
  labs(
    x = "",
    y = "",
    fill = "N Respondents",
    title = "Map of Respondent Distribution",
    subtitle =  "Range: 18 - 412"
  )+
  theme_void()

#map build for linked fate
ggplot()+
  geom_sf(data = ger_map_sf, aes(fill = mean_lf))+
  scale_fill_viridis()+
  labs(
    x = "",
    y = "",
    fill = "N Respondents",
    title = "Map of Respondent Respondent Mean Linked Fate"
  )+
  theme_void()


uk_geo <- geojson_sf("C:/Users/amber/OneDrive - UNT System/Projects/Dissertation/data/Chapter 3/uk_geodata/GBR_ADM1.geojson")

#join geodata to donnelly data
uk_map_sf <- left_join(uk_geo, regional, by = "shapeName") %>% 
  select(shapeName, n, mean_lf, geometry)


#these maps work, but it made me realize that the regional variable for this data would 
# not be appropriate for what I am trying to do. Based on advise from my committee 
#chapter 3 will no longer include the UK and will not be a regional analysis of Germany only

# #map build for n respondents
# ggplot()+
#   geom_sf(data = uk_map_sf, aes(fill = n))+
#   scale_fill_viridis()+
#   labs(
#     x = "",
#     y = "",
#     fill = "N Respondents",
#     title = "Map of Respondent Distribution",
#     subtitle =  "Range: 18 - 412"
#   )+
#   theme_void()
# 
# #map build for linked fate
# ggplot()+
#   geom_sf(data = uk_map_sf, aes(fill = mean_lf))+
#   scale_fill_viridis()+
#   labs(
#     x = "",
#     y = "",
#     fill = "Mean Linked Fate",
#     title = "Map of Respondent Distribution"
#   )+
#   theme_void()
