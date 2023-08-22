# Exercise: Day2_session01_ggplot2_demo_exercise_1_exploring_ggplot_KEY

# SETUP -------------------------------------------------------------------


# Library
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

df_tst <- df_msd %>%
  filter(indicator == "HTS_TST_POS") %>%
  summarize(
    cumulative = sum(cumulative, na.rm = T),
    .by = c("fiscal_year", "indicator")
  )  

# To view data
names(df_tst)
View(df_tst)


# EXPERIMENT --------------------------------------------------------------

# Instructions: Try using the following geoms in a plot: geom_line(), geom_point(), geom_area()
# Using the starter code below, add in different geoms and explore
# different types of aesthetic mappings

# Starter code
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) 


# geom_line()
?geom_line

ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2058, 2060, 1))

# geom_point()
?geom_point

ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(2058, 2060, 1))

# geom_area()
?geom_area 

ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)) + 
  geom_area() +
  scale_x_continuous(breaks = seq(2058, 2060, 1))

# What does the labs function do?
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) + 
  geom_col(fill = "#5BB5D5", width = 0.5) + 
  labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA") + 
  scale_x_continuous(breaks = seq(2058, 2060, 1)) + 
  scale_y_continuous(labels = scales::comma) + si_style_ygrid()