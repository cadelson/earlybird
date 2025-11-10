# PURPOSE: Create map of cases and infections for South Sudan since 2018
# AUTHOR: Cody Adelson | Data Science Consultant
# LICENSE: MIT
# DATE: November 11, 2025
# NOTES: 

pacman::p_load(tidyverse, openxlsx, readxl, glitr, extrafont, sf, geodata, gwepr, janitor, glue, ggtext, gwepr, ggspatial,
               maptiles, raster)

get_country_map <- function(country, year) {
  
  country_iso_mapping <- c("Ethiopia" = "ETH", "South Sudan" = "SSD", "Cameroon" = "CMR", "Chad" = "TCD", "Angola" = "AGO", "Mali" = "MLI")
  
  country_iso <- country_iso_mapping[country]
  
  # Check if the country name is valid
  if (is.null(country_iso)) {
    stop("The selected country is not supported. Please choose one of: Angola, Cameroon, Chad, Ethiopia, Mali, South Sudan.")
  }
  
  #country_map <- st_as_sf(gadm(country = country_iso, level = 2, path = tempdir()))
  country_map <- st_read("~/Github/gis/Data/gis_data/ssd_admbnda_adm2_imwg_nbs_20180401.shp")
  
  # Grab and load cases from Azure
  gwep_load("cases_infections")
  
  df_cases_map <- df_cases_infections %>% 
    filter(#year_event > as.numeric({{year}})-8,
           year_event > 2017,
           #country == {{country}},
           country == "South Sudan",
           worm_number == 1,
           #is.na(subcutaneous) | subcutaneous == 0,
           #!is.na(latitude_detect),
           #!is.na(longitude_detect)
    ) %>% 
    mutate(
      host_color = case_when(
        host == "Human" ~ "#c43d4d",
        subcutaneous == 1 ~ "#287c6f",
        TRUE ~ "#8980cb"),
      host_color_human = case_when(
        host == "Human" ~ host_color),
      host_color_animal = case_when(
        is.na(subcutaneous)|subcutaneous == 0|host != "Human" ~ host_color),
      host_color_subcu = case_when(
        subcutaneous == 1 ~ host_color)
    )
  
  # cases_total <- pull(df_cases_map %>% filter(host == "Human") %>% distinct(id_event) %>% tally())
  # infections_total <- pull(df_cases_map %>% filter(host != "Human") %>% distinct(id_event) %>% tally())
  
  # Define bounding box from your country map
  bbox <- st_bbox(country_map)
  
  # Download tiles using maptiles
  tiles <- get_tiles(x = country_map, provider = "OpenStreetMap", zoom = 10, crop = TRUE)
  
  # Convert tiles to raster for ggplot
  tile_raster <- raster(tiles)
  
  ###################
  tiles <- get_tiles(country_map, provider = "OpenStreetMap", zoom = 12, crop = TRUE)
  
  # Plot with ggspatial
  ggplot() +
    layer_spatial(tiles) +
    geom_sf(data = country_map, fill = "transparent", color = "#2057a7", size = 1)
  ########################
  
  ggplot(data = country_map) +
    #geom_sf(fill = "#bfddff", color = "#2057a7", size = 1) +
    annotation_raster(
      as.raster(tile_raster),
      xmin = bbox["xmin"], xmax = bbox["xmax"],
      ymin = bbox["ymin"], ymax = bbox["ymax"]
    ) +
    # annotation_map_tile(type = "osm", zoom = 8) +
    geom_sf(fill = "transparent", color = "#2057a7", size = 1) +
    geom_point(data = df_cases_map, 
               aes(x = longitude_detect, y = latitude_detect, color = "white"), size = 4) +
    geom_point(data = df_cases_map, 
               aes(x = longitude_detect, y = latitude_detect, color = host_color_subcu), size = 3.5) +
    geom_point(data = df_cases_map, 
               aes(x = longitude_detect, y = latitude_detect, color = host_color_animal), size = 3.5) +
    geom_point(data = df_cases_map, 
               aes(x = longitude_detect, y = latitude_detect, color = host_color_human), size = 3.5) +
    #ggtitle(glue("{country} <span style='color:#c43d4d;'>Cases</span> (n={cases_total}) and <span style='color:#8980cb;'>Infections</span> (n={infections_total}), {year}")) +
    si_style_void() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_markdown(size = 30, family = "Source Sans Pro", face = "bold", hjust = 0.2)) + 
    scale_color_identity()
  
  ggsave(
    filename = glue("Images/south_sudan/arm_gr/2025_arm/gw_map_{tolower(country)}_{year}.png"),
    height = 14,
    width = 19,
    )
  
  
  data %>% 
    filter(
      q1 == "yes" & is.na(q11)
    )
  
  df_cases_infections %>% 
    filter(year_event == 2025 & host == "Human")
  
}

get_country_map("South Sudan", 2025)
