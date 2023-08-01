# Exercise: Day2_session03_data_inspection_isolation_exercise_1_inspecting_data

# Project Setup & Data Prep -----------------------------------------------
  library(tidyverse)
  library(gagglr)

  # Create a path to data
  msd_path <- subnat_path <- list.files("Data", pattern = "PSNU", full.names = T)
  
  # Load the data
  df_msd <- read_psd(msd_path)
  

# INVESTIGATING -----------------------------------------------------------

  # Print the data frame to the screen
  df_msd
  
  # For the examples below, insert the correct dataframe in the function  
  
  # head() -Returns the first or last parts of a vector, matrix, table, data frame or function.
  head()
  tail()
  
  
  # names() - Functions to get or set the names of an object.
  names()
  
  
  # View() - Invoke a spreadsheet-style data viewer on a matrix-like R object.
  view()
  
    
  # str() - Compactly display the internal structure of an R object
  str()
  
  # glimpse() - glimpse() is like a transposed version of print(): columns run down the page, and data runs across. 
  glimpse()
  
  
  # Summary - What does this do? How to use it?
  

# UNIQUENESS --------------------------------------------------------------

  # Check for unique observations using distinct, unique or count
  # country, snu1, indicator
  unique()
  
  df_msd %>% 
    distinct()
  
  df_msd %>% 
    count() %>% 
    prinf(n = Inf)
  
  #How many unique snu1s are there?
    
  #How many unique indicators are there?
    
  #What is different about the three functions (unique, distinct, and count)?
    
  
  