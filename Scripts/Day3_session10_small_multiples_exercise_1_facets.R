# Exercise: Day3_session10_small_multiples_exercise_1_facets


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  library(scales)
  library(glitr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Set core data frame for exercises and examples
  df_tst_psnu <- df_msd %>%
    filter(indicator == "HTS_TST_POS",
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu, indicator, snu1) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    arrange(psnu) %>%
    slice(1:10)
  
  glimpse(df_tst_psnu)
  names(df_tst_psnu)
  

# EXERCISE ----------------------------------------------------------------

  #Instruction: Experiment with facet_grid() and facet_wrap() calls
  # What happens when you only include psnu? 
  # What changes when you add snu?
  # What is different about each plot?
  
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy, fill = snu1)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_grid(snu1 ~ psnu  ) 
  
  
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy, fill = snu1)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_grid(  ~ ) 
  
  
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy, fill = snu1)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_wrap( ~ ) 
  
  # Experiment with your own plot
  
  
  
  # Review the help sections for facet_grid() and facet_wrap()
  # What differences do you notice?
  
  
  