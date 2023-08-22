# Exercise: Day3_session02_intro_to_ggplot2_exercise_2_exploring_aesthetics_KEY


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

# Set core data frame for exercises and examples
df_tst_psnu <- df_msd %>%
  filter(indicator == "HTS_TST_POS") %>%
  summarize(
    cumulative = sum(cumulative, na.rm = T),
    .by = c("fiscal_year", "indicator", "psnu")
  ) %>%
  mutate(fy = as.character(fiscal_year)) %>% 
  arrange(psnu) %>%
  slice(1:9)

glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Experiment with different aesthetic mappings
# Using the starter code below, try different mappings for 
# size, shape, color, fill, and alpha
?aes()


# Starter columns
# provide columns for the aesthetic mapping below
df_tst_psnu %>%
  ggplot(mapping = aes(x = fy, y = cumulative, fill = psnu )) +
  geom_col()


# Starter points
# provide columns for the aesthetic mapping below
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, 
             size = cumulative, 
             color = psnu,
             shape = psnu)) +
  geom_point()


# Starter lines
# provide columns for the aesthetic mapping below
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, group = psnu, color = psnu)) + 
  geom_line()


# Explore -- try your own combinations