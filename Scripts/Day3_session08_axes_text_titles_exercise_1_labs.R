# Exercise: Day3_session08_axes_text_titles_exercise_1_labs


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  library(scales)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Set core data frame for exercises and examples
  df_tst_psnu <- df_msd %>%
    filter(indicator == "HTS_TST_POS",
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu, indicator) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    arrange(psnu) %>%
    slice(1:5)
  
  glimpse(df_tst_psnu)
  names(df_tst_psnu)
  

# EXERCISE ----------------------------------------------------------------

  # Instructions: Practice manipulating text on ggplot2 graphs
  
  # Base plot
  p <- df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col()
   
  # Print the base plot to the screen 
  p
  
  # Base plot -- add a subtitle
  p + 
  
  
  
  # Modify the x and y axes
  p +
  
  
  # Add a caption
  p  

  
  # Using the df_tst_psnu data frame, create a new plot
  # Add in a meaningful title, caption, subtitle and axis labels
  # Make sure any continuous variable axis texts use commas
  df_tst_psnu %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
  