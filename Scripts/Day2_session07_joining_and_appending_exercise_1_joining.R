# Exercise: Day2_session07_joining_and_appending_exercise_1_joining


# SETUP -------------------------------------------------------------------

  # Library
  # install.packages("readxl")
  library(readxl)
  library(tidyverse)
  library(gagglr)
  
  # Load data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  
  df_msd_tst <- df_msd %>% 
    filter(indicator == "HTS_TST", fiscal_year == 2060,
           standardizeddisaggregate == "Total Numerator") %>% 
    summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
              .by = c(mech_code, mech_name, indicator))

  
  # Load in the partner data
  df_lp <- read_excel("Data_Public/Minoria_local_partners.xlsx") %>% 
    relocate(mech_name, .after = mech_code)


# EXERCISE ----------------------------------------------------------------

  # Instructions
  # Investigate the columns of each data set.
  # What columns might you try merging on?
  
  
  # How can we compare the columns in each data set?
  names("...")  
  names("..." )  

  # What columns provide common overlap? 
  intersect(names("..."), names("..."))

  # Try joining the two data sets together
  left_join("..." , "...")
  right_join("..." , "...")
  
  
  # Why is this not working? 
  
  # What do you notice about the data types?
  
  
  #devtools::install_github("reconhub/linelist")
  #library(linelist)
  #compare_data(df_msd_tst, df_lp) 
  
  #// Comparison of variable classes /
  #  `mech_code` has changed from `character` to `numeric`
  

# LOAD LP CROSSWALK CORRECTED ---------------------------------------------

  #install.packages("tidylog")
  library(tidylog)
  
  # Load in the partner data
  df_lp_fixed <- read_excel("Data_Public/Minoria_local_partners.xlsx", sheet = "lp_crosswalk_fixed") %>% 
    relocate(mech_name, .after = mech_code)

  # We can see that both
  str(df_lp_fixed$mech_code)
  str(df_msd_tst$mech_code)  
  
  # Can we join now?
  tidylog::left_join("..." , "...")
  tidylog::right_join("..." , "...")
  
  