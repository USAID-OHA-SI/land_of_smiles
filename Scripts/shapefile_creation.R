# PROJECT: Land of smiles Minoria creation
# PURPOSE: Create Minoria shapefiles and attribute data
# AUTHOR:  Tim Esssam | SI
# REF ID:  5042ffa3
# LICENSE: MIT
# DATE:   2023-09-12
# NOTES:  For rounding out R training

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)

    
  # Census Shapefile
    census <- list.files("GIS/Minoria_base", pattern = ".shp", full.names = T)

  # REF ID for plots
    ref_id <- "5042ffa3"
    
  # Minoria MSD crosswalk to attach attribute data to shapefile
    ou_ids <- tibble::tribble(
                ~psnu,      ~psnuuid,            ~snu1,      ~snu1uid, ~operatingunit, ~operatingunituid,  ~country, ~OBJECTID,
             "Peoria", "icPkb3tE61B",        "Midwest", "Dh3xiXWw0Z7",      "Minoria",     "kAYJjRwuXG6", "Minoria",      102L,
        "Great Lakes", "gwmjspIcLT4",        "Midwest", "Dh3xiXWw0Z7",      "Minoria",     "kAYJjRwuXG6", "Minoria",      100L,
          "Wisconsin", "XwREtn25Uj1",        "Midwest", "Dh3xiXWw0Z7",      "Minoria",     "kAYJjRwuXG6", "Minoria",       99L,
        "Quad Cities", "FBzC1U92lJn",        "Midwest", "Dh3xiXWw0Z7",      "Minoria",     "kAYJjRwuXG6", "Minoria",      101L,
          "Hillsboro", "rI4Uq8vHclA",      "Northwest", "esEThbnCgpX",      "Minoria",     "kAYJjRwuXG6", "Minoria",       10L,
            "Spokane", "yfQtNuF35qk",      "Northwest", "esEThbnCgpX",      "Minoria",     "kAYJjRwuXG6", "Minoria",       11L,
             "Eugene", "pRaM01SfP5c",      "Northwest", "esEThbnCgpX",      "Minoria",     "kAYJjRwuXG6", "Minoria",        9L,
          "Vancouver", "NEI2PZqp3gc",      "Northwest", "esEThbnCgpX",      "Minoria",     "kAYJjRwuXG6", "Minoria",       22L,
         "Sugar Land", "w1wxehdfFVl",  "Pacific Coast", "ktTZxuCoYaj",      "Minoria",     "kAYJjRwuXG6", "Minoria",       21L,
      "Oklahoma City", "b1kJw6EiN2b",  "Pacific Coast", "ktTZxuCoYaj",      "Minoria",     "kAYJjRwuXG6", "Minoria",       59L,
        "Albuquerque", "FT7OhpWLZoA",  "Pacific Coast", "ktTZxuCoYaj",      "Minoria",     "kAYJjRwuXG6", "Minoria",       58L,
          "Salt Lake", "XhYbqN9Befa",  "Pacific Coast", "ktTZxuCoYaj",      "Minoria",     "kAYJjRwuXG6", "Minoria",        6L,
      "Winston-Salem", "adzjTKQI4Me", "South Atlantic", "qOV5HjD8Rsb",      "Minoria",     "kAYJjRwuXG6", "Minoria",      112L,
      "Hudson Valley", "kMLAdyekuFP", "South Atlantic", "qOV5HjD8Rsb",      "Minoria",     "kAYJjRwuXG6", "Minoria",      111L,
         "Wilmington", "w8zAQgbBeHP", "South Atlantic", "qOV5HjD8Rsb",      "Minoria",     "kAYJjRwuXG6", "Minoria",        8L,
         "Greensboro", "gki1ZesqzfQ", "South Atlantic", "qOV5HjD8Rsb",      "Minoria",     "kAYJjRwuXG6", "Minoria",      110L
      )

    
    plot_minoria <- function(df, fillvar, labelvar) {
      df %>% 
        ggplot() + 
        geom_sf(aes(geometry = geometry, fill = {{fillvar}})) +
        si_style_map() +
        geom_sf_label(aes(label = {{labelvar}}), size = 7/.pt) +
        scale_fill_si(palette = "siei", discrete = TRUE)
    }
  

# LOAD DATA ============================================================================  

    # Read in shapefile of Alaska Census tracts
    sf_df <- st_read(census)
    
    
    # List of FIPS codes used to filter the shapefile down to Minoria
    fips_list <- c("02185000100", "02185000300", "02185000200", 
                   "02188000100", "02188000200", "02290000200", 
                   "02290000300", "02290000100", "02180000100", 
                   "02180000200", "02290000400", "02270000100", 
                   "02050000300", "02270000100", "02050000200", 
                   "02050000100", "02070000100")
    
    minoria_psnu <- sf_df %>% 
      filter(FIPS %in% fips_list) %>% 
      select(FIPS, OBJECTID, geometry) %>% 
      left_join(ou_ids)
    
    plot_minoria(minoria_psnu, fillvar = snu1, labelvar = FIPS)

# Visualize ============================================================================
  
  minoria_psnu %>%
    plot_minoria(., psnu, psnu)

# MANIPULATE SHAPEFILE =================================================================
    
    minoria_snu1 <- 
      minoria_psnu %>% 
      st_make_valid() %>% 
      group_by(snu1uid, snu1, operatingunituid, operatingunit, country) %>% 
      summarise(geometry = sf::st_union(geometry), .groups = "drop")
    
    minoria_snu1 %>%
      plot_minoria(., snu1, snu1) 
    
    minoria_ou <- 
      minoria_psnu %>% 
      st_make_valid() %>% 
      group_by(operatingunituid, operatingunit, country) %>% 
      summarise(geometry = sf::st_union(geometry), .groups = "drop")
    
    minoria_ou %>% 
      plot_minoria(., operatingunit, country)
    

# TAKE IT FOR A DRIVE -----------------------------------------------------

  ggplot() +
      geom_sf(data = minoria_psnu, aes(geometry = geometry, fill = snu1), color = "white", linewidth = 0.25, alpha = 0.5) +
      geom_sf(data = minoria_ou, aes(geometry = geometry), fill = NA, color = "white", linewidth = 1) +
      geom_sf_text(data = minoria_psnu, aes(geometry = geometry, label = psnu), size = 6/.pt) +
      scale_fill_si(palette = "siei", discrete = TRUE) +
      scale_color_si(palette = "siei", discrete = TRUE) +
      si_style_map() +
      labs(title = "INTRODUCING MINORIA, THE NEWEST OU TO PEPFAR",
           subtitle = "PSNUs labeled on map",
           caption = "Source: Minoria faux shapefiles") +
      theme(panel.background = element_rect(fill = "#edf5fc", color = "white"), 
            legend.position = "none") 

# SAVE SHAPEFILES ============================================================================

    st_write(minoria_psnu, "GIS/MNA_psnu.shp")
    st_write(minoria_snu1, "GIS/MNA_snu1.shp")
    st_write(minoria_ou, "GIS/MNA_operatingunit.shp")
    
    saveRDS(minoria_psnu, "Data/MNA_psnu_geo")
    saveRDS(minoria_snu1, "Data/MNA_snu1_geo")
    saveRDS(minoria_ou, "Data/MNA_operatingunit_geo")
    