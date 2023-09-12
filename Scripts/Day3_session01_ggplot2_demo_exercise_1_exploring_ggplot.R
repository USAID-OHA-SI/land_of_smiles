# Exercise: Day2_session01_ggplot2_demo_exercise_1_exploring_ggplot
#Day-[day]_[session #]-[session name]_[exercise #]_[exercise short name].r

# SETUP -------------------------------------------------------------------


  # Library
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  df_tst <- df_msd %>%
    filter(indicator == "HTS_TST_POS") %>%
    summarize(
      cumulative = sum(cumulative, na.rm = T),
      .by = c("fiscal_year", "indicator", "psnu")
    ) 
  
  # To view data
  names(df_tst)
  View(df_tst)


# EXPERIMENT --------------------------------------------------------------

  # Instructions: Try using the following geoms in a plot: geom_line(), geom_point(), geom_area()
  # Using the starter code below, add in different geoms and explore
  # different types of aesthetic mappings
  
  # Starter code
    ggplot(
      data = df_tst,
      mapping = aes(x = fiscal_year, y = cumulative)
    ) 
  
  
  # geom_line()
  ?geom_line
  
  
  
  
  # geom_point()
  
  
  
  
  # geom_area()
  
  
  
  
  
  # What does the labs function do?
