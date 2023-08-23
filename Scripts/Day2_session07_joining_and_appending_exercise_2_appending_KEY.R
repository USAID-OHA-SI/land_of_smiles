# Exercise: Day2_session07_joining_and_appending_exercise_2_appending_KEY


# SETUP -------------------------------------------------------------------

# Library
# install.packages("readxl")
library(readxl)
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

df_subnat <- return_latest("Data", "SUBNAT") %>% 
  read_psd()


# Exercise ----------------------------------------------------------------

# Instructions: Try row binding the df_subnat to the df_msd

df_msd_subnat <- bind_rows(df_msd, df_subnat)

# How many rows does the new data set have?
str(df_msd_subnat)#41745 rows
glimpse(df_msd_subnat)

# How many columns?
names(df_msd_subnat) #40 columns 

# How about bind_cols? 
?bind_cols #similar to joins, binds dataframes by columns 
