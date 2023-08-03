# Exercise: Day2_session04_summarizing_exercise_2_count_plus_filter


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Filter dataset to just HTS data
  df_hts <- df_msd %>% 
    filter(indicator == "HTS_TST") 

# EXERCISE ----------------------------------------------------------------

  # Practice by counting the testing modalities and applying additional criteria
  # to filter() or the other parameters of count().
  
  # Add the modalities variable as one of the parameters
  df_hts %>% 
    count("...", wt = "...")
  
  # Review the data frame and consider what other variables could be interesting or what should be excluded (i.e. with filter())
  glimpse(df_hts)
  
  df_hts %>%
    filter("...") %>% 
    count("...", wt = "...")
  
  # Review the other parameters for count() and see if you can apply any in your code
  ?dplyr::count
  
  df_hts %>%
    filter("...") %>% 
    count("...", "..." = "...", wt = "...")
  