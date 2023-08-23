# Exercise: Day2_session04_summarizing_exercise_4_summarize_na_KEY.R


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

# Run the following function. What happened?
df_index %>%
  summarize(cumulative = sum(cumulative, na.rm = TRUE)) #must remove NA values before calculating 
