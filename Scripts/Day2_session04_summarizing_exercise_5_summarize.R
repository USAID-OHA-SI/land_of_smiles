# Exercise: Day2_session04_summarizing_exercise_5_summarize.R


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Filter dataset to just HTS data
  df_index <- df_msd %>% 
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           modality %in% c("Index", "IndexMod")) 

# EXERCISE ----------------------------------------------------------------

  # Determine which partners conducted the most index tests in 2059
  # use filter(), summarize(), and arrange()
  
  df_index %>% 
    filter("..." == "...") %>% 
    group_by(fiscal_year, "...") %>% 
    summarise(cumulative = sum("...", na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange("...")