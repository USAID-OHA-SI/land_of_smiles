# Exercise: Day2_session04_summarizing_exercise_5_summarize_KEY.R


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

# Determine which partners conducted the most index tests in 2059
# use filter(), summarize(), and arrange()

df_index %>% 
  filter(fiscal_year == 2059) %>% #filter for year
  group_by(fiscal_year, prime_partner_name) %>% #group by year and partner
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% #summarize cumulative by grouping
  ungroup() %>% 
  arrange(cumulative) #ascending order 
