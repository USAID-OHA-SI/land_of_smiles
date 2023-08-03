# Exercise: Day2_session06_mutating_exercise_1_achv.R


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Filter & summarize dataset to just HTS_POS index data by psnu
  df_index_psnu <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets)) 

# EXERCISE ----------------------------------------------------------------

  # What is the expected achievement by the end of FY60Q2?
  # Calculate expected achievement, achievement_exp (.5 * targets)
  
    df_index_psnu %>% 
      mutate("..." = "...")