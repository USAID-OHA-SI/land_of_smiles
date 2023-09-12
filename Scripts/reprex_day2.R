# PROJECT:  land_of_smiles
# AUTHOR:   T. Essam | USAID
# PURPOSE:  snippets from day 3
# LICENSE:  MIT
# DATE:     2023-07-24
# UPDATED:


# Global Settings ---------------------------------------------------------
  
  # Libraries needed
  library(tidyverse)
  library(gagglr)
  library(scales)

  # Plot size
  h <- 3.25
  w <- 5.75

# GGPLOT2 Demo Data -------------------------------------------------------


  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()
  
  df_tst <- df_msd %>%
    filter(indicator == "HTS_TST_POS") %>%
    summarize(
      cumulative = sum(cumulative, na.rm = T),
      .by = c("fiscal_year", "indicator")
    )
  
  str(df_tst)



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

  # Add in a title
  ggplot(
    data = df_tst,
    mapping = aes(x = fiscal_year, y = cumulative)
  ) +
    geom_col() +
    labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA")
  si_save("Images/ggplot_4", height = h, width = w)


  # Add in geom, title, and theme
  ggplot(
    data = df_tst,
    mapping = aes(x = fiscal_year, y = cumulative)
  ) +
    geom_col() +
    labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA") +
    theme_minimal()
  si_save("Images/ggplot_5", height = h, width = w)

  # Change bar color
  ggplot(
    data = df_tst,
    mapping = aes(x = fiscal_year, y = cumulative)
  ) +
    geom_col(fill = "#5BB5D5", width = 0.5) +
    labs(title = "2058 HAD THE MOST POSITIVE TESTS IN MINORIA") +
    theme_minimal()
  si_save("Images/ggplot_6", height = h, width = w)


  # Change bar color, clean up axes, add si_style
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


# INTRO TO GGPLOT2 --------------------------------------------------------

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

# Starter Code for Exercises


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

# Starter Code for Exercises


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

# Starter Code for Exercises


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

# Starter Code for Exercises



# SORTING -----------------------------------------------------------------

  # Sort or depends on the data type.
  # When dealing with categorical variables, use factors to make your live easier.

  
  df_tst_psnu %>% 
  filter(fiscal_year == 2060) %>% 
  ggplot(aes(y = psnu, x = cumulative)) +
  geom_col()
si_save("Images/ggplot_sort_default_order", height = h, width = w)

# Creating a factor
  df_tst_fct <- df_tst_psnu %>% 
    mutate(psnu_fct = factor(psnu)) 
  
  str(df_tst_fct)
  levels(df_tst_fct$psnu_fct)
  
  # Forcats to order from largest to smallest
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col()
  
  tmp <- df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative))
    levels(tmp$psnu_order)
  
  si_save("Images/ggplot_sort_forcats", height = h, width = w)
  
  # Starter Code for Exercises
  

# COLORS ------------------------------------------------------------------

  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    ggplot(aes(y = psnu, x = cumulative, 
               fill = psnu)) +
    geom_col() 
  
  si_save("Images/ggplot_color_default", height = h, width = w)
  
  # Manually overriding
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    ggplot(aes(y = psnu, x = cumulative, 
               fill = psnu)) +
    geom_col() +
    scale_fill_manual(values = c("Great Lakes" = "coral",
                                 "Albuquerque" = "orange",
                                 "Eugene" = "gray20"))
  
  si_save("Images/ggplot_color_manual", height = h, width = w)
  
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_color = ifelse(psnu == "Eugene", 
                               "#939598", "#5BB5D5")) %>% View()
    ggplot(aes(y = psnu, x = cumulative, 
               fill = psnu_color)) +
    geom_col() +
    scale_fill_identity()
  
  si_save("Images/ggplot_color_identity", height = h, width = w)

  # Continuous
  set.seed(42)
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(x = fy, y = cumulative,
               color = cumulative)) +
    geom_point(size = 10, position = position_jitter(width = 0.1)) +
    scale_color_viridis_c()
  si_save("Images/ggplot_color_continous", height = h, width = w)
  
  # Starter Code for Exercises
  

# LABELS ------------------------------------------------------------------

  
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col() +
    labs(x = "HTS_TST_POS", y = "PSNU",
         title = "EUGENE LEADS IN TESTING",
         caption = "Source: Faux MSD Training Data")
  
  si_save("Images/ggplot_labs", height = h, width = w)
  
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col() +
    labs(x = "HTS_TST_POS", y = "PSNU",
         title = "EUGENE LEADS IN TESTING",
         caption = "Source: Faux MSD Training Data")
  
  # Starter Code for Exercises
  
  

# AXES TICKS --------------------------------------------------------------

  ?scale_x_continuous()
  ?scale_y_discrete()
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col() +
    scale_x_continuous(breaks = seq(0, 1000, 100),
                       limits = c(0, 1000), 
                       position = "top") +
    scale_y_discrete(labels = str_to_upper)
  
  si_save("Images/ggplot_axis_ticks_continous", height = h, width = w)
  
  # Scales package
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative),
           cumulative = cumulative * 1000) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col() +
    scale_x_continuous(labels = scales::comma,
                       position = "top") 
  si_save("Images/ggplot_axis_ticks_scales", height = h, width = w)
  
  df_tst_psnu %>% 
    filter(fiscal_year == 2060) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ggplot(aes(y = psnu_order, x = cumulative)) +
    geom_col() +
    scale_y_discrete(labels = str_to_upper, 
                     position = "right")
  
  si_save("Images/ggplot_axis_ticks_discrete", height = h, width = w)
  
 # Starter Code for Exercises
  

# REDUCE CLUTTER ----------------------------------------------------------
  
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_wrap(~psnu) 
  si_save("Images/ggplot_axis_clutter_base", height = h, width = w)
  
  
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_wrap(~psnu) +
    theme(plot.title = element_text(family = "Times New Roman", color = "red"),
          panel.grid.major  = element_line(colour = "gray50", size = 0.5),
          panel.background = element_rect(fill = "gray99"))
  
  si_save("Images/ggplot_axis_clutter", height = h, width = w)
  
  # Starter Code & Exercises
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + 
    labs(title = "TESTING RESULTS",
         subtitle = "Change my color",
         caption = "can you change my font?") +
    facet_wrap(~psnu) 
  
  # Move facet labels to the left
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + 
    labs(title = "TESTING RESULTS",
         subtitle = "Change my color",
         caption = "can you change my font?") +
    facet_wrap(~psnu) +
    theme()
  
# Comparing themes
  theme_bw()$strip.text
  si_style()$strip.text
  
  # Save list as an object
  # Look through different list values
  df_si <- si_style()
  df_si$plot.title
 
  

# SMALL MULTIPLES ---------------------------------------------------------
  
  # Facet wrap
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_wrap(psnu ~ ., scales = "free_y", strip.position = "bottom")   
  
  si_save("Images/ggplot_facet_wrap", height = h, width = w)
  
  # Facet grid
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_col(width = 0.5) + labs(title = "TESTING RESULTS") +
    facet_grid(psnu ~ ., switch = "y", scales = "free_y", space = "free")
  
  si_save("Images/ggplot_facet_grid", height = h, width = w)
  
 

# SF ----------------------------------------------------------------------

  # Call in the sf package via library function
  library(sf)
  
  # Load the snu1 shapefile from the GIS folder
  snu1_df <- st_read("GIS/MNA_snu1.shp")
  
  # Plot the SNU1 boundaries on a simple map
  snu1_df %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = snu1), color = grey90k) +
    labs(title = "SNU1 MAP CREATED BY SF") +
    si_style_map() +
    theme(legend.text = element_text(size = 7))
  
  si_save("Images/sf_snu1_map.png", height = h, width = w)
  
  
  # Join in the relevant data
  df_tst_snu1 <- df_msd %>%
    filter(indicator == "HTS_TST_POS", fiscal_year == 2060) %>%
    group_by(fiscal_year, snu1, snu1uid, indicator) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(fy = as.character(fiscal_year)) 
  
  snu1_tst_df <- 
    snu1_df %>% 
    left_join(., df_tst_snu1)
  
  snu1_tst_df %>% 
    ggplot() +
    geom_sf(aes(fill = cumulative), 
            color = "white", size = 1) +
    geom_sf_label(aes(label = snu1), 
            size = 8/.pt) +
    scale_fill_viridis_c(direction = -1) +
    si_style_map() +
    si_legend_fill() +
    labs(x = NULL, y = NULL,
         title = "NORTHWEST WITH LARGEST TESTING RESULTS")
  si_save("Images/sf_snu1_map_testing.png", height = h, width = w, 
          scale = 1.4)
  

# GT ----------------------------------------------------------------------


  

# GLITR DEMO --------------------------------------------------------------

  #install
  install.packages("remotes")
  remotes::install_github("USAID-OHA-SI/glitr", build_vignettes = TRUE)
  
  # Load
  library(glitr) 
  
  # Facet grid
  df_tst_psnu %>% 
    mutate(fy = fiscal_year %>% as.character()) %>% 
    ggplot(aes(y = cumulative, x = fy)) +
    geom_point(size = 4, aes(color = psnu),
               position = position_jitter(width = 0.1)) +
    si_style() +
    scale_color_si(palette = "old_rose", discrete = T) +
    labs(title = "Great Lakes leads the way in testing",
         caption = "Source: 2023 Faux MSD data")
  
  si_save("Images/ggplot_si_style", height = h, width = w)
  
  si_palettes$siei_pairs %>% show_col(labels = T, borders = T)
  si_save("Images/ggplot_si_pairs.png", height = h, width = w)
  
  old_rose
  "#2057a7" "#c43d4d" "#8980cb" "#e07653" "#1e87a5" "#f2bc40" "#287c6f" 
  
  
# Printing all shapes
  
  d <- data.frame(p=c(0:25))
  ggplot() +
    scale_y_continuous(name="") +
    scale_x_continuous(name="") +
    scale_shape_identity() +
    geom_point(data = d, mapping=aes(x = p%%16, y = p%/%16, shape = p), size = 5, fill = "coral") +
    geom_text(data = d, mapping=aes(x = p%%16, y = p%/%16+0.25, label = p), size = 3) +
    theme_void()
  
  si_save("Graphics/ggplot2_shapes.svg")
  