# PROJECT: Chart Taxonomy code chunks
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  21843434
# LICENSE: MIT
# DATE:   2023-07-14
# NOTES:   

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(kableExtra)
  library(knitr)

# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "21843434" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements
  
  metadata$caption <- str_replace(metadata$caption, metadata$curr_fy_lab, "FY60")


# Custom function to show every nth tick mark
  # https://stackoverflow.com/questions/52919899/display-every-nth-value-on-discrete-axis
  every_nth = function(n) {
    return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  }
  
# Function to save data view as a png
  make_data_jpg <- function(df, viz_name = ""){
    df %>% 
    kable() %>% 
      kable_styling(bootstrap_options = "condensed", 
                    font_size = 12, 
                    full_width = F) %>% 
      save_kable(file = glue("Images/data_{viz_name}.png"),
                 zoom = 4)
  }
  
  
# IMPORT ------------------------------------------------------------------

#trainign data available for download from GDrive
# https://drive.google.com/drive/folders/1Dmo7NqtBCbexsaq6XpkN1MZlTn-s2Jkz
  df <- return_latest("Data", "PSNU") %>% 
    read_psd()   

  data_source <- "Source: Faux training MSD 2023"
  
# Magnitude ============================================================================

  # Collapse data to SNU1 level and summarize tst_pos results
  df_bar <- df %>%
    filter(
      indicator %in% c("HTS_TST_POS"),
      standardizeddisaggregate == "Total Numerator",
      fiscal_year == 2060
    ) %>%
    summarise(HTS_TST_POS = sum(cumulative, na.rm = T), .by = "snu1")

  # Create a sorted bar graph
  df_bar %>%
    mutate(snu1 = fct_reorder(snu1, HTS_TST_POS)) %>%
    ggplot(aes(x = HTS_TST_POS, y = snu1)) +
    geom_col() +
    labs(title = "NORTHWEST LEADS IN POSITIVE CASES IN 2060",
         caption = glue("{data_source}"), 
         x = NULL, y = NULL) +
    scale_x_continuous(labels = comma) +
    si_style_xgrid() 
  
  si_save("Images/magnitude.png", scale = 1.3, height = 4, width = 4)
  
  make_data_jpg(df_bar, "magnitude")
  

# Time-series ============================================================================

  # Collapse data to national level to show testing trends
  df_time <- df %>%
    filter(
      indicator %in% c("HTS_TST_POS"),
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    summarize(across(c(contains("qtr")), \(x) sum(x, na.rm = T)), 
              .by = "fiscal_year") %>%
    reshape_msd() 

  # Create time-series graph shaded below
  df_time %>%
    ggplot(aes(x = period, y = value, group = "a")) +
    geom_area(fill = grey20k, alpha = 0.75) +
    geom_line() +
    scale_x_discrete(breaks = every_nth(2)) +
    labs(title = "POSITIVE TESTS HAVE DECLINED OVER TIME",
         caption = glue("{data_source}")) +
    si_style()

  si_save("Images/change_over_time.png", scale = 1.3, height = 4, width = 4)

  make_data_jpg(df_time, "time_series")
  
# Ranking  ----------------------------------------------------------------

  # Collapse data to sub-nation level to show testing trends
  df_time_snu1 <- df %>%
    filter(
      indicator %in% c("HTS_TST_POS"),
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    summarize(across(c(contains("qtr")), \(x) sum(x, na.rm = T)), 
              .by = c("snu1", "fiscal_year")) %>%
    reshape_msd() %>%
    filter(period == min(period) | period == max(period))

 # Create a slop graph
 df_time_snu1 %>%
   mutate(value_label = case_when(
     period == max(period) ~ paste(comma(value), snu1),
     TRUE ~ comma(value)
   )) %>%
   ggplot(aes(x = period, y = value, group = snu1)) +
   geom_line() +
   geom_point() +
   ggrepel::geom_text_repel(aes(label = value_label), 
                            hjust = 1, force = 4) +
   si_style_xline() +
   theme(axis.text.y = element_blank()) +
   labs(
     y = NULL, x = NULL,
     title = "TESTING HAS FLATLINED IN THE MIDWEST",
     caption = glue("{data_source}")
   )
 
 si_save("Images/ranking.png", scale = 1.3, height = 4, width = 4)

 make_data_jpg(df_time_snu1, "ranking")
 
# PARTS-TO-WHOLE ----------------------------------------------------------
  
  # How much does index testing contribute to overall positives?
  df_index <- df %>%
    filter(
      indicator %in% c("HTS_TST_POS"),
      standardizeddisaggregate == "Modality/Age/Sex/Result",
      fiscal_year == max(fiscal_year)
    ) %>%
    group_by(indicator, modality) %>%
    summarise(
      results = sum(cumulative, na.rm = T),
      .groups = "drop"
    ) %>%
    mutate(
      share = results / sum(results),
      modality = fct_reorder(modality, share, .desc = T)
    ) %>%
    arrange(modality)

  # Need to get a proportion back to pass to waffle
  prop_fill <- df_index %>%
    filter(modality == "OtherPITC") %>%
    transmute(fill_number = round(share * 100, 0)) %>%
    pull()
  
  waffle_input <- c(100 - prop_fill, prop_fill)
  
  waffle_input %>%
    waffle::waffle(
      color = c(grey20k, scooter_med), flip = T,
      reverse = T, size = 0.5
    ) +
    geom_text(aes(x = 2.5, y = 2, label = percent(prop_fill / 100)),
      size = 36 / .pt
    ) +
    theme(legend.position = "none") +
    labs(title = str_to_upper(glue("{prop_fill}% of all positive tests are\nfrom Other PITC testing")),
         caption = glue("{data_source}"))
  
  si_save("Images/waffle.png", scale = 1.3, height = 4, width = 4)
  
  make_data_jpg(df_index, "waffle")

# Stacked Bar graph showing proportions (plot top 5)
  df_index %>%
    slice_max(share, n = 5) %>%
    mutate(benchmark = 1) %>%
    ggplot(aes(x = indicator, y = share)) +
    geom_col(aes(y = benchmark), fill = grey20k, alpha = 0.5) +
    geom_col(aes(fill = modality)) +
    geom_hline(yintercept = 1, color = grey40k, linetype = "dotted") +
    geom_text(aes(label = percent(share, 1)),
      vjust = -0.25
    ) +
    facet_wrap(~modality, ncol = 5) +
    scale_fill_si(palette = "old_rose", discrete = T) +
    si_style_ygrid(facet_space = 0.5) +
    scale_y_continuous(labels = percent) +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    ) +
    labs(
      x = NULL, y = NULL,
      title = str_to_upper("Other PITC is an effective testing modality"),
      caption = "Source: Faux training MSD 2023"
    )
  
  si_save("Images/small_multiples.png", scale = 1.3, height = 4, width = 4)


# DISTRIBUTION ------------------------------------------------------------

  # Create a population pyramid of test positive to show missing populations
  df_pyramid <- df %>%
    filter(
      indicator %in% c("HTS_TST_POS"),
      standardizeddisaggregate == "Modality/Age/Sex/Result",
      fiscal_year == 2060,
      ageasentered != "Unknown Age"
    ) %>%
    summarise(
      cumulative = sum(cumulative, na.rm = T),
      .by = c(indicator, sex, ageasentered)
    ) %>%
    mutate(
      cumulative = ifelse(sex == "Female", -cumulative, cumulative),
      fill_color = ifelse(sex == "Female", moody_blue, genoa)
    )

  # Use bars to create pyramid
  df_pyramid %>%
    ggplot(aes(cumulative, ageasentered, group = "sex", fill = fill_color)) +
    geom_col(alpha = 0.85) +
    # Filter original data down to just men the focus category
    geom_col(
      data = . %>% filter(ageasentered %in% c("15-19", "20-24", "25-29"), sex == "Male"),
      fill = "#004137"
    ) +
    geom_blank(aes(-cumulative)) +
    geom_vline(xintercept = 0, color = "white") +
    scale_x_continuous(labels = abs) +
    scale_fill_identity() +
    labs(
      x = NULL, y = NULL,
      title = "MINORIA IS MISSING FINDING 15-29 YEAR OLD <span style = 'color:#287c6f;'>MEN</span>",
      caption = glue("{data_source}")
    ) +
    si_style_xgrid() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      plot.title = element_markdown()
    )

  
  si_save("Images/distribution.png", scale = 1.3, height = 4, width = 4)
  
  make_data_jpg(df_pyramid, "distribution")
