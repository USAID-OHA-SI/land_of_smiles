# PROJECT:  land_of_smiles
# AUTHOR:   T. Essam | USAID
# PURPOSE:  snippets from day 2
# LICENSE:  MIT
# DATE:     2023-07-24
# UPDATED: 

# Project Setup & Data Prep -----------------------------------------------
  library(tidyverse)
  library(gagglr)


# Working directory screenshot
getwd()

# Relative path
getwd()
dir()

# Folder setup
library(glamr)
glamr::folder_setup()

# list files
dir()
list.files("Data")

# list files pattern
dir()
list.files("Data", pattern = ".zip")
list.files("Data", pattern = "PSNU")

# list files full names
dir()
list.files("Data", pattern = "PSNU", full.names = TRUE)

# list files for pointing to path
list.files("Data")
subnat_path <- list.files("Data", pattern = "NAT_SUBNAT", full.names = T)
subnat_path

# Exercise
msd_path <- list.files("Data", pattern = "PSNU", full.names = T)

read_psd(msd_path)

df_msd <- read_psd(msd_path)



# Data Inspection ---------------------------------------------------------

# Inspecting data
df_msd
head(df_msd)
names(df_msd)
View(df_msd)
str(df_msd)
glimpse(df_msd)

# Checking for uniqueness
unique(df_msd$mech_code)

df_msd %>% 
  distinct(mech_code)

df_msd %>% 
  count(mech_code) %>% 
  print(n = 10)


# Exercise solutions ------------------------------------------------------

unique(df_msd$country)

unique(df_msd$snu1) %>% length()

df_msd %>% count(indicator)

# Renaming
names(df_msd)[1:10]
df_msd_renamed <- 
  df_msd %>% 
  rename(ou = operatingunit)
names(df_msd_renamed)[1:10]


# Isolating Data ----------------------------------------------------------

# filter
df_msd %>% count(indicator)
df_msd %>% 
  filter(indicator == "TX_CURR") %>% 
  count(indicator)

df_msd %>% 
  count(indicator) %>% 
  arrange(n)

df_msd %>% select(1:10) %>% names()
df_msd %>% select(c(operatingunit, psnu))

# Exercise solutions

# Rename
df_msd %>% 
  rename(sourcename = source_name) %>% 
  names()

df_msd %>% 
  select(sourcename = source_name) %>% 
  names()

# Filter
df_msd %>% 
  filter(indicator == "HTS_TST")

# Arrange
df_msd %>% 
  count(indicator, sort = T)

df_msd %>% 
  count(indicator) %>% 
  arrange(desc(n))

# Select
df_mechs <- 
  df_msd %>% 
  select(c(mech_name, mech_code))

df_mechs <- 
  df_msd %>% 
  select(contains("mech"))

# Fin



# Joining -----------------------------------------------------------------

  #load the df_subnat data
  subnat_path <- list.files("Data", pattern = "SUBNAT", full.names = TRUE)
  df_subnat <- read_psd(subnat_path)
  
  # Practice joining the interational partner labels to the mechanisms
  df_msd %>% distinct(funding_agency, prime_partner_name, mech_code, mech_name)
  
  # Should this be a URL on github?
  # An excel file?
  library(googlesheets4)
  load_secrets()
  df_lp <- read_sheet(ss = "17AN5LjuPk7lMBitf4UPmEXEPdf4T-qWBTFTvjISiPYI")

  df_msd_subset <- 
    df_msd %>% 
    filter(indicator == "HTS_TST")
  
  # Should break because of incompatible types
  left_join(df_msd_subset, df_lp)
  
  
  # Try this again with proper columns
  # Should this be a URL on github?
  # An excel file?
  library(googlesheets4)
  load_secrets()
  df_lp <- read_sheet(ss = "17AN5LjuPk7lMBitf4UPmEXEPdf4T-qWBTFTvjISiPYI", sheet = 2)
  
  df_msd_subset <- 
    df_msd %>% 
    filter(indicator == "HTS_TST")
  
  # Should break because of incompatible types
  tidylog::left_join(df_msd_subset, df_lp)
  tidylog::right_join(df_msd_subset, df_lp)
  tidylog::full_join(df_msd_subset, df_lp)
  
  # How many partners do we expect to match?
  df_msd_mechs <- df_msd_subset %>% distinct(mech_code, mech_name)
  df_lp_mechs <- df_lp %>% distinct(mech_code, mech_name)
  
  # How to check overlap?
  intersect(df_lp_mechs$mech_code, df_msd_mechs$mech_code)
  setdiff(df_lp_mechs$mech_code, df_msd_mechs$mech_code)
  setequal(df_lp_mechs$mech_code, df_msd_mechs$mech_code)
  
  