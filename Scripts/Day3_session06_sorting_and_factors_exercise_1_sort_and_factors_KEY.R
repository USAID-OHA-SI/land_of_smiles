# Exercise: Day3_session06_sorting_and_factors_exercise_1_sort_and_factors_KEY


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

# Set core data frame for exercises and examples
df_tst_psnu <- df_msd %>%
  filter(indicator == "HTS_TST_POS",
         fiscal_year == 2060) %>%
  group_by(fiscal_year, psnu, indicator) %>% 
  summarize(across(c(targets, cumulative), 
                   \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(fy = as.character(fiscal_year)) %>% 
  arrange(psnu) %>%
  slice(1:5)

glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Explore the data frame and review the help for fct_reorder
# Practice creating factors and sorting plots by them

# Review the help for fct_reorder
?fct_reorder
# What package is the function from? 
forcats::fct_reorder()

# Create a factor for the psnus, where the levels are mapped to cumulative results
df_tst_fct <- 
  df_tst_psnu %>% 
  mutate(psnu_cmltv = fct_reorder(psnu,cumulative)) 

# How would you investigate the new data frame df_tst_fct?
glimpse(df_tst_fct)    


# Create a factor for the psnus, where the levels are mapped to cumulative results
# and create a column plot with the psnus plotted in order

df_tst_psnu %>% 
  mutate(psnu_cmltv = fct_reorder(psnu, cumulative)) %>% 
  ggplot(aes(y = psnu_cmltv, x = cumulative)) +
  geom_col()


# Create a factor for the psnus, where the levels are mapped to targets
# and create a column plot with the psnus plotted in order

df_tst_psnu %>% 
  mutate(psnu_trgt = fct_reorder(psnu , targets)) %>% 
  ggplot(aes(y = psnu_trgt, x = targets)) +
  geom_col()


# SORTING A FACETING VARIABLE
# Try creating a faceting variable to sort the PSNUS in order of targets
# when using small multiples

df_tst_psnu %>% 
  mutate(psnu_trgt  = fct_reorder(psnu, targets, .desc = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = targets, y = indicator), width = 0.5, fill = grey10k) + #fills by targets 
  geom_col(aes(x = cumulative, y = indicator), width = 0.5, fill = scooter) + #fills by cumulative 
  facet_wrap(~ psnu_trgt, nrow = 5) + #sorts psnu in order of targets
  theme_minimal()

