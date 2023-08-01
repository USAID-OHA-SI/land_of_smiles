# Exercise: Day3_session07_applying_color_exercise_1_color


# SETUP -------------------------------------------------------------------
  
  # Library
  library(tidyverse)
  library(gagglr)
  
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

  # Instructions: Practice applying color to different types of plots
  
  df_tst_psnu %>% 
    ggplot(aes(y = psnu, x = cumulative, 
               fill = )) +
    geom_col()
  
  
  # Complete the code below.
  # Create a new variable that takes the the following values
  # for Oklahoma City, Hillsboro and Great Lakes --> lightblue
  # for all other psnus "grey50"
  # Use the new variable to apply a fill to the column graph
  df_tst_psnu %>% 
    mutate(psnu_fill = case_when(
      psnu %in% c("", "", "") ~ ,
      TRUE ~ ,
    )) %>% 
    ggplot(aes(y = psnu, x = cumulative, 
               fill = )) +
    geom_col() +
    scale_fill_
  
  
  
  # Shapes and color
  # Using the slide on color or fill, add the correct aesthetic value
  
  # color or fill
  df_tst_psnu %>% 
    ggplot(aes(y = targets, x = cumulative)) +
    geom_point(aes( = psnu), shape = 15, size = 5)
  
  # color or fill
  df_tst_psnu %>% 
    ggplot(aes(y = targets, x = cumulative)) +
    geom_point(aes( = psnu), shape = 8, size = 5)
  
  # color or fill
  df_tst_psnu %>% 
    ggplot(aes(y = targets, x = cumulative)) +
    geom_point(aes( = psnu), shape = 21, size = 5)
  
  # color or fill
  df_tst_psnu %>% 
    ggplot(aes(y = targets, x = cumulative)) +
    geom_point(aes( = psnu), shape = 25, size = 7,   = "black", stroke = 1)
  
  # Experiment with your own plots, using color to fill in bars, areas or points
  
  
  
  
  # Examples of continuous mapping of color
  df_tst_psnu %>% 
    ggplot(aes(y = targets, x = cumulative)) +
    geom_point(aes(fill = cumulative), shape = 25, size = 7, stroke = 1) +
    scale_fill_viridis_c()
  
  df_tst_psnu %>% 
    pivot_longer(cols = c(targets, cumulative),
                 values_to = "value",
                 names_to = "type") %>% 
    ggplot(aes(y = type, x = psnu)) +
    geom_tile(aes(fill = value), color = "white") +
    scale_fill_viridis_c(option = "A")
  
  