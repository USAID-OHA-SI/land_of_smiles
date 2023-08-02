# Exercise: Day3_session09_reducing_clutter_exercise_1_theme_settings


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  library(scales)
  library(glitr)
  library(glamr)
  
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

  # Instructions: Using the starter chunk below, make the following changes:
  # TIP: Use the Theme System Cheatsheet to help you identify the element names
  
  p <- df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + 
    labs(title = "TESTING RESULTS",
         subtitle = "Cumulative results across psnus") +
    facet_wrap(~psnu) 
  
  # Visualize the plot
  print(p)
    
  # Move (align) the facet labels to be left aligned
  ?theme()
  
  p + theme( = element_text(hjust = ))
  
  
  # Remove the minor grid lines completely
  p + theme(panel.grid.minor = element_blank())
  
  # Change the plot subtitle color to be "gray60"
  # use 
  p + theme()
  
  
  
  #Look at the differences between theme_bw() and si_style()
  
  bw <- theme_bw()$strip.text
  bw
  
  si <- si_style()$strip.text
  si
  
  # What does this return?
  si_list <- si_style()
  si_list %>% names()  

  # Look up strip.placement
  si_list$legend.position
  