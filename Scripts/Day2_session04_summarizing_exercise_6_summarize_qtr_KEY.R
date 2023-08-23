# Exercise: Day2_session04_summarizing_exercise_6_summarize_qtr_KEY.R


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

# Filter dataset to just HTS Index data
df_index <- df_msd %>% 
  filter(indicator == "HTS_TST",
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         modality %in% c("Index", "IndexMod")) 

# EXERCISE ----------------------------------------------------------------

# What do each quarters’ index testing results look like in Eugene in FY59?
# Use filter() and summarize() with across()

df_index %>% 
  filter(psnu == "Eugene") %>% #filter for specified psnu
  group_by(fiscal_year) %>% #group by year 
  summarize(across(c(targets, qtr1,qtr2, qtr3, qtr4, cumulative), #summarize across listed variables
                   \(x) sum(x, na.rm = TRUE)), 
            .groups = "drop") #drop grouping 
