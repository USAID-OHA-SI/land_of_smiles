# Exercise: Day3_session02_intro_to_ggplot2_exercise_4_exploring_themes_KEY


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

# Instructions: Experiment with different theme settings
# Using the starter code below, try adding different themes to plots
# What do you notice about the different themes?
# Try changing some of the theme arguments, what happens?
?theme_dark()

# Theme dark
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~psnu) +
  theme_gray() #gray grid

# Theme bw
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~psnu) +
  theme_bw() #white grid

# Theme SI style
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~psnu) +
  si_style() #clear grid 

# Theme SI ygrid
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~psnu) +
  si_style_ygrid() #y-axis grid lines only

# Theme SI xgrid
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~psnu) +
  si_style_xgrid() #x-axis grid lines only

# What does the help for theme() return?
?theme #showcases all the parameters 
