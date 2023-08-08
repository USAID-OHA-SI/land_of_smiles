# Exercise: Day3_session05a_practice_1_exploration


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  
  #msd path
  path_msd <- return_latest("Data", "PSNU_IM")
  
  #grab metadata about MSD
  get_metadata(path_msd) #list of MSD metadata elements
  
  # Load data
  df_msd <- read_psd(path_msd)
  
  # Filter and aggregate index data by PSNU
  df_index_psnu_trend <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% c("Index", "IndexMod")) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets))
  
  # Calculate achievement and flag for under achievement
  df_index_psnu_trend <- df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           low_achv = ifelse(fiscal_year == metadata$curr_fy, 
                             achievement < (metadata$curr_qtr/4) - .1,
                             achievement < .9),
           achv_color = case_when(low_achv == TRUE ~ "purple",
                                  is.infinite(achievement) ~ NA_character_,
                                  TRUE ~ "#909090"))
  
  # Create a separate dataset for just the current fiscal year
  df_index_psnu <- df_index_psnu_trend %>% 
    filter(fiscal_year == metadata$curr_fy)

# EXERCISE ----------------------------------------------------------------
    
  # Instructions: Think about the questions that lend themselves to these 
  # different types of plots. Before generating, try sketching out what you 
  # want to do. Try generating basic plots using appropriate geoms:
  #   geom_col()
  #   geom_line()
  #   geom_area()
  #   geom_point()
  #   facet_wrap() or facet_grid()
  # Experiment with aesthetics (shape, size, fill/color, alpha (transparency))   
    
    #review data structure
    glimpse(df_index_psnu_trend)
    glimpse(df_index_psnu)
  
    #review helpfiles for ggplot and geoms
    ?ggplot
    ?geom_col
    
    #generate basic plots
    "..." %>% 
      ggplot(aes("...", "...")) +
      geom_"..."()
  
    
    "..." %>% 
      ggplot(aes("...", "...")) +
      geom_"..."()
    
    
    "..." %>% 
      ggplot(aes("...", "...")) +
      geom_"..."()
    
    "..." %>% 
      ggplot(aes("...", "...")) +
      geom_"..."()
    