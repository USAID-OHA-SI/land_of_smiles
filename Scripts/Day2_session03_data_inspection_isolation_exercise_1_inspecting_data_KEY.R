# Exercise: Day2_session03_data_inspection_isolation_exercise_1_inspecting_data_KEY

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
head(df_msd)
tail(df_msd)


# names() - Functions to get or set the names of an object.
names(df_msd)


# View() - Invoke a spreadsheet-style data viewer on a matrix-like R object.
view(df_msd)


# str() - Compactly display the internal structure of an R object
str(df_msd)

# glimpse() - glimpse() is like a transposed version of print(): columns run down the page, and data runs across. 
glimpse(df_msd)


# Summary - What does this do? How to use it?
summary(df_msd)

# UNIQUENESS --------------------------------------------------------------

# Check for unique observations using distinct, unique or count
# country, snu1, indicator
unique(df_msd$country)
unique(df_msd$snu1)
unique(df_msd$indicator)


df_msd %>% 
  distinct(snu1)

df_msd %>% 
  distinct(indicator)

df_msd %>% 
  count(indicator) %>% 
  prinf()

#How many unique snu1s are there? 
4 snu1s - Midwest, Northwest, Pacific Coast, South Atlantic  

#How many unique indicators are there?
6 indicators - HTS_TST, TX_CURR, HTS_TST_POS, TX_NET_NEW, TX_PVLS, TX_NEW

#What is different about the three functions (unique, distinct, and count)?
unique -  lists unique values and removes duplicates
distinct - lists distinct values 
count - counts the unique values of a variable 


