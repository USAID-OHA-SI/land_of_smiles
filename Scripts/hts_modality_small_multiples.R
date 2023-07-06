# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  recreate small mulitples viz
# REF ID:   96048255 
# LICENSE:  MIT
# DATE:     2023-07-06
# UPDATED: 
# Note:     Adapted from groundhog_day/FY20Q4_TZA_HTS_POART.R

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "96048255" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements
  
  metadata$caption <- str_replace(metadata$caption, metadata$curr_fy_lab, "FY60")
  

# IMPORT ------------------------------------------------------------------
  
  #trainign data available for download from GDrive
  # https://drive.google.com/drive/folders/1Dmo7NqtBCbexsaq6XpkN1MZlTn-s2Jkz
  df <- return_latest("Data", "PSNU") %>% 
    read_psd()   
  

# MUNGE -------------------------------------------------------------------
  
  #filter to HTS modalities and aggregate
  df_mod <- df %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    group_by(fiscal_year, indicator, modality) %>%
    summarise(across(starts_with("qtr"), ~sum(.x, na.rm = TRUE)),
              .groups = "drop")
  
  
  df_mod <- df_mod %>% 
    reshape_msd(include_type = FALSE) %>% 
    arrange(indicator, modality, period) %>% 
    mutate(order = ifelse(period == max(period) & indicator == "HTS_TST", value, 0),
           modality = recode(modality, "OtherPITC" = "Other PITC",),
           mod_lump = fct_lump(modality, n = 5, w = order, other_level = "All Other")) %>% 
    group_by(period, indicator, mod_lump) %>% 
    summarise(across(c(value, order), ~sum(.x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(mod_lump = factor(mod_lump, c("Other PITC", "PMTCT ANC",
                                         "Post ANC1", "VCT",
                                         "Index", "All Other")),
           fill_color = ifelse(mod_lump == "Index", burnt_sienna, "gray60"))

  
  df_mod_yield <- df_mod %>% 
    select(-order) %>% 
    spread(indicator, value) %>% 
    mutate(value = HTS_TST_POS/HTS_TST,
           indicator = "Positivity",
           start = case_when(period == min(period) ~ value),
           end = case_when(period == max(period) ~ value)
    ) 
  
  v_mod_hts <- df_mod %>% 
    filter(indicator == "HTS_TST") %>% 
    ggplot(aes(period, value, fill = fill_color)) +
    geom_blank() +
    annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = Inf,
             fill = "gray60", alpha = .2) +
    geom_col() +
    facet_grid(indicator ~ mod_lump, switch = "y") +
    scale_y_continuous(label = comma) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    si_style_ygrid() +
    theme(strip.text.y = element_text(hjust = .5),
          axis.text.x = element_blank(),
          strip.placement = "outside")
  
  v_mod_pos <- df_mod %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    ggplot(aes(period, value, fill = fill_color)) +
    geom_blank() +
    annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = Inf,
             fill = "gray60", alpha = .2) +
    geom_col() +
    facet_grid(indicator ~ mod_lump, switch = "y") +
    scale_y_continuous(label = comma) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    si_style_ygrid() +
    theme(strip.text.y = element_text(hjust = .5),
          axis.text.x = element_blank(),
          strip.text.x = element_blank(),
          strip.placement = "outside")

  
  v_mod_yield <- df_mod_yield %>%
    ggplot(aes(period, value, group = mod_lump, color = fill_color)) +
    geom_blank() +
    annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = .28,
             fill = "gray60", alpha = .2) +
    geom_line(linewidth = 1.1) +
    geom_point(aes(y = start), size = 3, na.rm = TRUE) +
    geom_point(aes(y = end), shape = 21, fill = "white", stroke = 1.5,
               size = 3, na.rm = TRUE) +
    facet_grid(indicator ~ mod_lump, switch = "y") +
    scale_y_continuous(label = percent) +
    scale_x_discrete(labels = ifelse(str_detect(unique(df_mod$period), 'Q1'), unique(df_mod$period), "")) +
    scale_color_identity() +
    labs(x = NULL, y = NULL) +
    si_style_ygrid() +
    theme(strip.text.y = element_text(hjust = .5),
          strip.text.x = element_blank(),
          strip.placement = "outside")
  
  v_mod_hts / v_mod_pos / v_mod_yield +
    plot_annotation(title = "IMPRESSIVE INDEX POSITIVITY ACROSS MINORIA CONTINUES DESPITE Q4 SETBACK",
                    caption = metadata$caption) &
    si_style_ygrid() &
    theme(strip.text.y = element_text(hjust = .5),
          # strip.text.x = element_blank(),
          strip.placement = "outside")
  
  si_save("Images/hts_mods.png")
  
