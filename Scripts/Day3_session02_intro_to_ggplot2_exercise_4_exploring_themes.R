# Exercise: Day3_session02_intro_to_ggplot2_exercise_4_exploring_themes


# SETUP -------------------------------------------------------------------

  # install glitr if it doesn't exist
  # install.packages("remotes")
  # remotes::install_github("USAID-OHA-SI/glitr", build_vignettes = TRUE)

  # Library
  library(tidyverse)
  library(gagglr)
  library(glitr)
  
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
    arrange(psnu) %>%
    slice(1:9)
  
  glimpse(df_tst_psnu)
  names(df_tst_psnu)
  
  
# EXERCISE ----------------------------------------------------------------

  # Instructions: Experiment with different theme settings
  # Using the starter code below, try adding different themes to plots
  # What do you notice about the different themes?
  # Try changing some of the theme arguments, what happens?
  ?theme_dark()
  
  
  # Theme dark
  df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative)) +
    geom_col() + 
    facet_wrap(~psnu) +
    theme_
  
  
  # Theme bw
  df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative)) +
    geom_col() + 
    facet_wrap(~psnu) +
    theme_
  
  
  # Theme SI style
  df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative)) +
    geom_col() + 
    facet_wrap(~psnu) +
    si_style
  
  
  # Theme SI ygrid
  df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative)) +
    geom_col() + 
    facet_wrap(~psnu) +
    si_style_
  
  
  # Theme SI xgrid
  df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative)) +
    geom_col() + 
    facet_wrap(~psnu) +
    si_style_
  
  # What does the help for theme() return?
  