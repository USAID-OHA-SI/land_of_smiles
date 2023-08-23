# Exercise: Day2_session03_data_inspection_isolation_exercise_2_isolating_data_KEY

# Project Setup & Data Prep -----------------------------------------------
library(tidyverse)
library(gagglr)

# Create a path to data
msd_path <- subnat_path <- list.files("Data", pattern = "PSNU", full.names = T)

# Load the data
df_msd <- read_psd(msd_path)


# RENAMING ----------------------------------------------------------------

# Check the names of the data frame
names(df_msd)

# Rename the "source_name" column to "sourcename"
df_msd %>% 
  rename(sourcename = source_name ) %>% 
  names()


# FILTERING ---------------------------------------------------------------

# Practice filtering the MSD

# Filter the indicator column to only "HTS_TST

df_msd %>% 
  filter(indicator == "HTS_TST") %>% 
  count(indicator)

# Try the same filter, but now filter the fiscal_year to 2060
df_msd %>% 
  filter(indicator == "HTS_TST",
         fiscal_year == 2060) %>% 
  count(indicator)


# ARRANGING ---------------------------------------------------------------

# Count the number of indicators in the dataset and arrange the 
# frequency in descending order  
df_msd %>% 
  count(indicator) %>% 
  arrange(desc(n))

# Try arranging the results in ascending order
df_msd %>% 
  count(indicator) %>% 
  arrange(n)


# SELECTING ---------------------------------------------------------------

# Create a new data frame that only contains (select) the 
# variables mech_code and mech_name
df_msd_mech <- 
  df_msd %>% 
  select(mech_code, mech_name)

# Check the names of the columns in the df_msd_mech data frame
names(df_msd_mech)
