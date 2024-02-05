#--------------------------------Chapter 3 data build---------------------------
#-Author: A. Rose Benton---------------------------Created: November, 15, 2023-#
#-R Version: 4.1.3---------------------------------Revised:  January, 15, 2024-#


#this project is still in process, at the moment this file serves more as a list of the 
#data I have gotten access to and what form it is in than a functional script, Eventually
#this will include all of the environmental measures at a regional level merged in with 
#donnellys linked fate data (see class maps or data/chapter3/donnelly for more detail)

pacman::p_load("tidyverse", 
               "tidync", #nc files
               "viridis", #color pallette
               "geojsonio", #geojson files
               "geojsonsf",  #geojson sf converter
               "sf",  #special features files
               "raster", #raster, .nc, .tif files
               "tidyverse",
               install = FALSE
               )

#--------------------------------------Aid_data--------------------------------

aid_raw <- read.csv("data/Chapter 3/ch3_aiddata_germany/germany_aiddata_raw.csv")

#pm25 and c02
aid_2 <- aid_raw %>% 
  transmute(
    region = shapeName,
    pop = worldpop_pop_count_1km_mosaic.2018.sum,
    pm_mean = surface_pm25_annual_v5gl03.2018.mean,
    pm_max = surface_pm25_annual_v5gl03.2018.max,
    pm_min = surface_pm25_annual_v5gl03.2018.min,
    oco2_mean = oco2_v10r_xco2_yearly.2018.mean,
    oco2_max = oco2_v10r_xco2_yearly.2018.max,
    oco2_min = oco2_v10r_xco2_yearly.2018.min,
    shapeID
  )

no2 <- read.csv("data/Chapter 3/ch3_aiddata_germany/no2_2018.csv") %>% 
  rename(no2_mean = Jahres.mittelwert.in.µg.m.,
         region = X) %>% 
  group_by(region) %>% 
  summarise(no2_mean = mean(as.numeric(no2_mean), na.rm = TRUE))

aid_3 <- left_join(aid_2, no2, by = "region")

#--------------------------------ch4--------------------------------------------

ch4 <- tidync("data/Chapter 3/ch4.nc") %>% hyper_tibble() %>% 
  transmute(
    anth_ch3 = anth_flux_opt, 
    tot_ch3 = total_flux_opt,
    longitude,
    latitude, 
    time
  ) %>% 
  filter(
    between(latitude, 47, 55) & between(longitude, 5, 15)
  )

ch4_sf <- ch4 %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
ger_geo <- geojson_sf("data/Chapter 3/Germany_geodata/DEU_ADM1.geojson")


#plot the points to determine if NA values found later are being caused by the join or are actually missing
ggplot() +
  geom_sf(data = ger_geo) +
  geom_point(data = ch4_sf, aes(x = st_coordinates(geometry)[, "X"], y = st_coordinates(geometry)[, "Y"]))


ch4_join <- st_join(ger_geo, ch4_sf, join = st_contains) %>% 
  dplyr::select(shapeName, anth_ch3, tot_ch3) 

ch4_join <- as.data.frame(ch4_join)

write_csv(ch4_join, "data/Chapter 3/ch4_join.csv")

ch4_region <- ch4_join  %>% 
  group_by(shapeName) %>% 
  summarize(anth_ch3 = mean(anth_ch3),
            tot_ch3 = mean(tot_ch3))

aid_4 <- left_join(aid_3, ch4_region, by = c("region" = "shapeName"))

#-------------------------------------tree cover loss-------------------------------

tcloss <- readxl::read_xlsx("data/Chapter 3/tc_loss.xlsx", sheet = 4) %>% 
  group_by(subnational1) %>% 
  summarise(mean_tcl = mean(tc_loss_ha_2018)) %>% 
  transmute(
    region = case_when(
      subnational1 == "Niedersachsen" ~ "Lower Saxony",
      subnational1 == "Rheinland-Pfalz" ~ "Rhineland-Palatinate",
      subnational1 == "Sachsen" ~ "Saxony",
      subnational1 == "Sachsen-Anhalt" ~ "Saxony-Anhalt",
      subnational1 == "Hessen" ~ "Hesse",
      subnational1 == "Bayern" ~ "Bavaria",
      subnational1 == "Thüringen" ~ "Thuringia",
      subnational1 == "Nordrhein-Westfalen" ~ "North Rhine-Westphalia",
      TRUE ~ subnational1
    ),
    mean_tcl
  )
  
aid_5 <- left_join(aid_4, tcloss, by = "region")


#-------------------------------total organic carbon----------------------------

toc <- read_csv("data/Chapter 3/toc/c_annual.csv") %>% 
  filter(Year == 2015) 

stations <- st_read("data/Chapter 3/toc/stations.shp")

toc_1 <- left_join(toc, stations, by = "OBJECTID")

toc_2 <- st_transform(toc_1, crs = 4326) %>% 
  dplyr::select(median_TOC, geometry)

toc_geo <- st_join(ger_geo, toc_2, join = st_nearest_feature) 

toc_regions <- as.data.frame(toc_geo)%>% 
  group_by(shapeName) %>% 
  summarise(
    med_toc = median(median_TOC)
  )


aid_6 <- left_join(aid_5, toc_regions, by = c("region" = "shapeName"))
#------------------------------soil degredation--------------------------------

soil <- raster("data/Chapter 3/De_Rosa.et.al2023.g_C_kg_y.tif")

extracted_values <- extract(soil, ger_geo, fun = mean, na.rm = TRUE)

soil_change <- as.data.frame(ger_geo$shapeName)

soil_change$mean_value <- extracted_values

soil_region <- soil_change %>% 
  transmute(
    region = `ger_geo$shapeName`,
    soil_change = as.numeric(mean_value)
  )

full_df <- left_join(aid_6, soil_region, by = "region") %>% 
  dplyr::select(region, pm_mean, oco2_mean, no2_mean, tot_ch3, mean_tcl, med_toc, soil_change)


write_csv(full_df, "data/Chapter 3/factor_analysis_df_full.csv")

# 
# #------------------------------tropospheric ozone--------------------------------
# #### this doesnt work yet, cut for immediate deadline (texas comparative circle conferrence)
# #### will be added back in for full dissertation
# 
# nc_files_directory <- "data/Chapter 3/trop_03"
# 
# # List all .nc files in the directory
# nc_files <- list.files(path = nc_files_directory, pattern = "//.nc$", full.names = TRUE)
# 
# # Create an empty list to store data frames
# df_list <- list()
# 
# # Specify the variable name
# variable_name <- "ozone_anomaly_instrument"
# 
# # Loop through each .nc file, read it, and convert it to a data frame
# for (file in nc_files) {
#   raster_data <- raster(file, varname = variable_name)
#   raster_df <- as.data.frame(raster_data, xy = TRUE)
#   
#   # Add a column for the file name or any other identifier
#   raster_df$file_name <- basename(file)
#   
#   df_list[[length(df_list) + 1]] <- raster_df
# }
# 
# # Combine all data frames into one
# combined_df <- bind_rows(df_list)
