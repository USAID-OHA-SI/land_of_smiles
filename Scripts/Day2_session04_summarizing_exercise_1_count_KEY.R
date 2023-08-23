# Exercise: Day2_session04_summarizing_exercise_1_count_KEY


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

#filter dataset to just HTS data
df_hts <- df_msd %>% 
  filter(indicator == "HTS_TST") 

# EXERCISE ----------------------------------------------------------------

# Review the help file for count
?dplyr::count

# How many people were tested for HIV?
# Use count() to sum up the cumulative column in the MSD
df_hts %>% 
  count(indicator, wt = cumulative)  #2,466,661
