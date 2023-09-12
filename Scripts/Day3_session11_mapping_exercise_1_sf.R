# Exercise: Day3_session11_sf_exercises


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  library(scales)
  library(sf)

  # Data - Make sure to downlaod `MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-60_20230616_v2_1.zip` file and move it to `Data` folder
  

  # GIS - Make sure the GIS folder has the proper shapefiles
  # Load PSNU x IM data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Load sf data
  psnu_sf <- st_read("GIS/MNA_psnu.shp")
  ou_sf   <- st_read("GIS/MNA_operatingunit.shp")
  
  # Set core data frame for exercises and examples
  # Sequence of operations
  # 1) Filter HTS POS indicator and 2060 fiscal year
  # 2) summarize targets and cumulative results by fiscal year, psnu, snu1 and indicator
  # 3) Create a new variable (fy) that transform fiscal year variable as character type
  df_tst_psnu <- df_msd %>%
    filter(indicator == "HTS_TST_POS") %>%
    group_by(fiscal_year, snu1, snu1uid, psnu, psnuuid, indicator) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(fy = as.character(fiscal_year)) 
  
  # Take a look at the structure of the spatial data
  glimpse(psnu_sf)
  names(psnu_sf)   
  

# EXERCISE ----------------------------------------------------------------

  #Instruction: Join the sf data frame to the testing (df_tst_psnu) data frame
  # Using a left_join with the sf data frame as the base

    df_psnu_tst_geo <- psnu_sf %>% 
      left_join(., "...")

  # Try making map of the testing targets
    df_psnu_tst_geo %>% 
      ggplot() +
      geom_sf(aes(fill = "...")) +
      labs(title = "...") 
  
  # Try faceting the same map above by fiscal year
    df_psnu_tst_geo %>% 
      ggplot() +
      geom_sf(aes(fill = "...")) +
      labs(title = "...") +
      facet_wrap(~ "...")
  
  
  
  # Review the help sections for geom_sf() and geom_sf_label()
  
  
  