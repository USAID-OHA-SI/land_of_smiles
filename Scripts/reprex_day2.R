# PROJECT:  land_of_smiles
# AUTHOR:   T. Essam | USAID
# PURPOSE:  snippets from day 3
# LICENSE:  MIT
# DATE:     2023-07-24
# UPDATED:

# Project Setup & Data Prep -----------------------------------------------
library(tidyverse)
library(gagglr)

df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()

df_tst <- df_msd %>%
  filter(indicator == "HTS_TST_POS") %>%
  summarize(
    cumulative = sum(cumulative, na.rm = T),
    .by = c("fiscal_year", "indicator")
  )

# size
h <- 3.25
w <- 5.75

# Empty plot
ggplot(data = df_tst)
si_save("Images/ggplot_1", height = h, width = w)

# Add in mapping
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
)
si_save("Images/ggplot_2", height = h, width = w)

# Add in geom
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) +
  geom_col()
si_save("Images/ggplot_3", height = h, width = w)

# Add in geom
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) +
  geom_col() +
  labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA")
si_save("Images/ggplot_4", , height = h, width = w)


# Add in geom
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) +
  geom_col() +
  labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA") +
  theme_minimal()
si_save("Images/ggplot_5", , height = h, width = w)

# Change bar color
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) +
  geom_col(fill = "#5BB5D5", width = 0.5) +
  labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA") +
  theme_minimal()
si_save("Images/ggplot_6", height = h, width = w)


# Change bar color
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
) +
  geom_col(fill = "#5BB5D5", width = 0.5) +
  labs(
    title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA",
    x = NULL, y = NULL
  ) +
  scale_x_continuous(breaks = seq(2058, 2060, 1)) +
  scale_y_continuous(labels = scales::comma) +
  si_style_ygrid()

si_save("Images/ggplot_7", height = h, width = w)




# Starter code
# Test different geoms
ggplot(
  data = df_tst,
  mapping = aes(x = fiscal_year, y = cumulative)
)


# Intro to ggplot2 --------------------------------------------------------

# Will it plot?
df_tst_psnu <- df_msd %>%
  filter(indicator == "HTS_TST_POS") %>%
  summarize(
    cumulative = sum(cumulative, na.rm = T),
    .by = c("fiscal_year", "indicator", "psnu")
  ) %>%
  arrange(psnu) %>%
  slice(1:9)

# Test 1
df_tst_psnu %>%
  ggplot()

# Test 2
df_tst_psnu %>%
  ggplot(aes(x = fiscal_year, y = indicator))

# Test 3
df_tst_psnu %>%
  ggplot() +
  geom_col() +
  scale_x_continuous() +
  theme_minimal()


df_tst_psnu %>%
  ggplot(aes(
    x = fiscal_year,
    y = cumulative
  )) +
  geom_col() +
  scale_x_continuous() +
  facet_wrap(~psnu) +
  theme_minimal()
si_save("Images/ggplot_8", height = h, width = w)


# AESTHETICS ------------------------------------------------------

# Make fy a character for plotting ease
df_tst_psnu <- df_tst_psnu %>% mutate(fy = as.character(fiscal_year))

df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_point()
si_save("Images/ggplot_aes_plain", height = h, width = w)




# Color
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative,
    color = psnu
  )) +
  geom_point()
si_save("Images/ggplot_aes_color", height = h, width = w)

# Size
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative,
    size = cumulative
  )) +
  geom_point()
si_save("Images/ggplot_aes_size", height = h, width = w)

# Shapes
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative,
    shape = psnu
  )) +
  geom_point()
si_save("Images/ggplot_aes_shape", height = h, width = w)

# Alpha
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative,
    alpha = cumulative
  )) +
  geom_point(size = 5)
si_save("Images/ggplot_aes_transparency", height = h, width = w)

# Combining color, shape and size
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative,
    color = psnu,
    size = cumulative,
    shape = psnu
  )) +
  geom_point()
si_save("Images/ggplot_aes_combining", height = h, width = w)


# GEOMS -------------------------------------------------------------------


# geom_col
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_col() +
  facet_wrap(~psnu)
si_save("Images/ggplot_geoms_col", height = h, width = w)

# geom_point()
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_point() +
  facet_wrap(~psnu)
si_save("Images/ggplot_geoms_point", height = h, width = w)

# geom_line()
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative, group = psnu
  )) +
  geom_line() +
  facet_wrap(~psnu)
si_save("Images/ggplot_geoms_line", height = h, width = w)

# geom_area()
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative, group = psnu
  )) +
  geom_area() +
  facet_wrap(~psnu)
si_save("Images/ggplot_geoms_area", height = h, width = w)


# Combine geoms and aestethics
df_tst_psnu %>%
  ggplot(aes(
    x = fy, y = cumulative,
    group = psnu
  )) +
  geom_area(fill = "#d1d3d4", alpha = 0.75) +
  geom_line(linetype = "dotted", size = 0.5, color = "#414042") +
  geom_point(aes(size = cumulative)) +
  facet_wrap(~psnu) +
  si_style_ygrid()
si_save("Images/ggplot_geoms_combined", height = h, width = w, scale = 1.25)



# THEMES ------------------------------------------------------------------

# theme_gray
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_col() +
  facet_wrap(~psnu) +
  theme_gray()
si_save("Images/ggplot_theme_gray", height = h, width = w)


# theme_bw
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_col() +
  facet_wrap(~psnu) +
  theme_bw()
si_save("Images/ggplot_theme_bw", height = h, width = w)

# theme_minimal
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_col() +
  facet_wrap(~psnu) +
  theme_minimal()
si_save("Images/ggplot_theme_minimal", height = h, width = w)

# theme si
df_tst_psnu %>%
  ggplot(aes(
    x = fy,
    y = cumulative
  )) +
  geom_col() +
  facet_wrap(~psnu) +
  si_style()
si_save("Images/ggplot_theme_si", height = h, width = w)
