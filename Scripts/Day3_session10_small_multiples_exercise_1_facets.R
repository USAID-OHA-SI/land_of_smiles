# Exercise: Day3_session10_small_multiples_exercise_1_facets


# SETUP -------------------------------------------------------------------

  # Library
  library(tidyverse)
  library(gagglr)
  library(scales)

  # Data - Make sure to downlaod `MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-60_20230616_v2_1.zip` file and move it to `Data` folder
  
  # Load PSNU x IM data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  # Set core data frame for exercises and examples
  # Sequence of operations
  # 1) Filter HTS POS indicator and 2060 fiscal year
  # 2) summarize targets and cumulative results by fiscal year, snu1, psnu, and indicator
  # 3) Create a new variable (fy) that transform fiscal year variable as character type
  df_tst_psnu <- df_msd %>%
    filter(indicator == "HTS_TST_POS") %>%
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    arrange(psnu) %>%
    slice(1:15)
  
  # Take a look at the structure of the result
  glimpse(df_tst_psnu)
  names(df_tst_psnu)   
  

# EXERCISE ----------------------------------------------------------------

  #Instruction: Experiment with facet_grid() and facet_wrap() calls
  # What happens when you only include psnu? 
  # What changes when you add snu1?
  # What is different about each plot?
  
  df_tst_psnu %>% 
    ggplot(aes(y = cumulative, x = fy, fill = snu1)) +
    geom_col(width = 0.5) + 
    labs(title = "TESTING RESULTS") +
    facet_grid(snu1 ~ psnu) 
  
  
  df_tst_psnu %>% 
    ggplot(aes(y = cumulative, x = fy, fill = snu1)) +
    geom_col(width = 0.5) + 
    labs(title = "TESTING RESULTS") +
    facet_grid( ~ ) 
  
  
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy, fill = snu1)) +
    geom_col(width = 0.5) + 
    labs(title = "TESTING RESULTS") +
    facet_wrap( ~ ) 
  
  # Experiment with your own plot
  
  
  
  # Review the help sections for facet_grid() and facet_wrap()
  # What differences do you notice?
  
  
  