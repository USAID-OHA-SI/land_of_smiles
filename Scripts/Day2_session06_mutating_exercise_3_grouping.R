# Exercise: Day2_session06_mutating_exercise_3_grouping.R

# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Filter & summarize dataset to just Index Pos by PSNU and Year
  df_index_psnu_trend <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% 
             c("Index", "IndexMod")) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets))
  


# EXERCISE ----------------------------------------------------------------

  # Try running the following code and review the results. 
  # What is the code trying to do?
  # What do you notice is going on? Does it work?
    
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           targets_prior = lag(targets),
           max_targets = max(targets)) %>% 
    arrange(psnu, fiscal_year)
  
  
  