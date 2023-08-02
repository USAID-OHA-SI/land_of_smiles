# Exercise: Day2_session05_joining_and_appending_exercise_2_appending


# SETUP -------------------------------------------------------------------

  # Library
  # install.packages("readxl")
  library(readxl)
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  df_subnat <- return_latest("Data", "SUBNAT") %>% 
    read_psd()


# Exercise ----------------------------------------------------------------

  # Instructions: Try row binding the df_subnat to the df_msd
  
  df_msd_subnat <- bind_rows("...", "...")

  # How many rows does the new data set have?
  # How many columns?