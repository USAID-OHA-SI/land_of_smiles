# Exercise: Day2_session04_summarizing_exercise_3_sorting_KEY


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

# Filter dataset to just HTS data
df_hts <- df_msd %>% 
  filter(indicator == "HTS_TST") 

# EXERCISE ----------------------------------------------------------------

# Practice by identifying all the partners conducting testing in 2060 and 
# sort them on a different variable

#Filter dataset to be for the focus year
df_hts_2060 <- df_hts %>% 
  filter(standardizeddisaggregate == "Modality/Age/Sex/Result",
         fiscal_year == 2060) 

# Add the partner variable as one of the parameters
names(df_hts)

df_hts_2060 %>% 
  count(indicator, fiscal_year, prime_partner_name, wt = cumulative, sort = TRUE)

# Test out using different function like count(), distinct(), and unique(), and arrange()
df_hts_2060 %>% 
  distinct(prime_partner_name) 

df_hts_2060 %>% 
  count(prime_partner_name) %>% 
  arrange(n)

unique(df_hts_2060$prime_partner_name)
