# Exercise: Day2_session03_data_inspection_isolation_exercise_2_isolating_data

# Project Setup & Data Prep -----------------------------------------------
  library(tidyverse)
  library(gagglr)
  
  # Create a path to data
  msd_path <- subnat_path <- list.files("Data", pattern = "PSNU", full.names = T)
  
  # Load the data
  df_msd <- read_psd(msd_path)
  

# RENAMING ----------------------------------------------------------------

  # Check the names of the data frame
  names(df_msd)
  
  # Rename the "source_name" column to "sourcename"
  df_msd %>% 
    rename("..." = "..." ) %>% 
    names("...")
  

# FILTERING ---------------------------------------------------------------

  # Practice filtering the MSD
  
  # Filter the indicator column to only "HTS_TST
  
  df_msd %>% 
    filter(indicator == "...") %>% 
    count("...")
  
  # Try the same filter, but now filter the fiscal_year to 2060
  df_msd %>% 
    filter(indicator == "...",
           "..." == "...") %>% 
    count("...")
  

# ARRANGING ---------------------------------------------------------------

  # Count the number of indicators in the dataset and arrange the 
  # frequency in descending order  
  df_msd %>% 
    count("...") %>% 
    arrange("...")
  
  # Try arranging the results in ascending order
  df_msd %>% 
    count("...") %>% 
    arrange("...")
  

# SELECTING ---------------------------------------------------------------
  
  # Create a new data frame that only contains (select) the 
  # variables mech_code and mech_name
  df_msd_mech <- 
    df_msd %>% 
    select("...")
  
  # Check the names of the columns in the df_msd_mech data frame

  