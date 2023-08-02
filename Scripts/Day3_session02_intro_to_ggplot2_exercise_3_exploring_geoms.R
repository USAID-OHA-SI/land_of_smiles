# Exercise: Day3_session02_intro_to_ggplot2_exercise_3_exploring_geoms


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Set core data frame for exercises and examples
  df_tst_psnu <- df_msd %>%
    filter(indicator == "HTS_TST_POS") %>%
    summarize(
      cumulative = sum(cumulative, na.rm = T),
      .by = c("fiscal_year", "indicator", "psnu")
    ) %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    arrange(psnu) %>%
    slice(1:9)
  
  glimpse(df_tst_psnu)
  names(df_tst_psnu)
  
# EXERCISE ----------------------------------------------------------------
  
  # Instructions: Experiment with different aesthetic mappings
  # Using the starter code below, try applying different geoms
  ?geom_line()
  
  
  # columns
  df_tst_psnu %>%
    ggplot(aes(x = fy, y = cumulative, fill = "...")) +
    geom_col()
  
  
  
  # points (scatterplot)
  #
  df_tst_psnu %>%
    ggplot(aes(x = fy, y = cumulative, color = "...")) +
    geom_point()
  
  
  
  # points (scatterplot)
  df_tst_psnu %>%
    ggplot(aes(x = fy, y = cumulative, group = psnu, size = "...")) +
    geom_line()
  
  
  
  # Area (scatterplot)
  df_tst_psnu %>%
    ggplot(aes(x = fy, y = cumulative, 
               group = psnu, fill = "...")) +
    geom_area() +
  facet_wrap(~psnu)
  
  
  # Try combining some of the aesthetics
  