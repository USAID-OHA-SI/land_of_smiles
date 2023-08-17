# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  use case example
# REF ID:   8d629b8e 
# LICENSE:  MIT
# DATE:     2023-08-17
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(gagglr)
library(grabr)
library(glue)
library(scales)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(ggrepel)
library(googledrive)


# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "8d629b8e" #id for adorning to plots, making it easier to find on GH
  
  metadata$caption <- glue("Source: FY60Q3p Faux Training DATIM Genie | Ref id: {ref_id}")
  
  #access credentials
  load_secrets()
  
  #datim url for API
  baseurl <- "https://final.datim.org/"
  
  #create a temp folder to store outputs
  temp_folder(TRUE)

  #GDrive folder for uploading any outputs
  drive_id <- "1TPPnZbWicpJB0x07u1IloTL_5p3OWDqO"
  
# API FUNCTION ------------------------------------------------------------

  # datim_dimensions() %>% arrange(dimension) %>% prinf()
  # 
  # datim_dim_items("Disaggregation Type") %>% arrange(item) %>% prinf()
  # 
  # datim_dim_items("Disaggregation Type") %>% arrange(item) %>%
  #   filter(item %in% c("Age/Sex/Indication/HIVStatus",
  #                      "Age/Sex/HIVStatus")) %>%
  #   pull(id) %>%
  #   paste0(collapse = ";")
  
  pull_facility_data <- function(orgunit, baseurl = "https://final.datim.org/"){
    
    url <- paste0(baseurl,
                  "api/analytics.json?",
                  "dimension=pe:2021Q2;2021Q3;2021Q4;2022Q1;2022Q2;2022Q3;2022Q4;2023Q1;2023Q2&", #periods
                  "dimension=ou:OU_GROUP-POHZmzofoVx;", orgunit,"&", #facilities under X orgunit
                  "dimension=LxhLO68FcXm:MvszPTQrUhy;bZOF8bon1dD&", #Technical Area: TX_CURR, TX_PVLS
                  "filter=HWPJnUTMjEq:pxz2gGSIQhG;PxGprLSHtqv&", #Disaggregation Type  
                  "dimension=jyUTj5YC3OK&", #Cascade Sex
                  "dimension=e485zBiR7vG&", #Age: Cascade Age bands
                  "dimension=lD2x0c8kywj&", #Numerator / Denominator
                  "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=false"
    )
    
    df_api <- datim_process_query(url) 
    
    df_api <- df_api %>% mutate(psnuuid = {orgunit}, .before = 1)
    
    return(df_api)
  } 
  
# RUN API -----------------------------------------------------------------

  #identify which PSNUs to run API over
  df_org_map <- read_csv("../themask/Dataout/geo_map.csv") 
    
  #run the API to pull VLC data
  df_api_pull <- map_dfr(df_org_map$psnuuid,
                         pull_facility_data)
  
  #clean extract to match MSD
  df_api_pull_clean <- df_api_pull %>% 
    convert_datim_pd_to_qtr() %>% 
    rename(orgunit = `Organisation unit`,
           indicator = `Technical Area`,
           sex = `Cascade sex`,
           ageasentered = `Age: Cascade Age bands`,
           numeratordenom = `Numerator / Denominator`) %>% 
    mutate(numeratordenom = str_sub(numeratordenom, end = 1),
           ageasentered = ageasentered %>% str_remove(" \\(.*") %>% str_remove(" "),
           ageasentered = case_match(ageasentered,
                                     c("50-54", "55-59", "60-64", "65+") ~ "50+",
                                     "<1" ~ "<01",
                                     "1-4" ~ "01-04",
                                     "5-9" ~ "05-09",
                                     .default = ageasentered)) %>% 
    rename_all(tolower)
  
  #export data
  write_csv(df_api_pull_clean, 
            "Dataout/datim-vlc.csv",
            na = "")


# REIMPORT DATA -----------------------------------------------------------

  #read in data
  df_vlc <- read_csv("Dataout/datim-vlc.csv")
  
  #add date to meta data from file create date
  metadata$caption <- str_replace(metadata$caption, "Genie", glue("Genie [Pulled {file.info('Dataout/datim-vlc.csv')$ctime %>% as.Date}]"))
  
# MUNGE -------------------------------------------------------------------
  
  #review data structure
  glimpse(df_vlc)

  #clean up indicator, adding _D to denominator variables
  df_vlc <- df_vlc %>% 
    clean_indicator() 
  

# VIZ - PSNU TRENDS -------------------------------------------------------

  #aggregate data to psnu level and reshape wide to calc vlc
  df_vlc_psnu <- df_vlc %>% 
    group_by(psnu, period, indicator) %>% 
    summarise(value = sum(value, na.rm = TRUE),
              .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(psnu) %>% 
    mutate(vlc = tx_pvls_d / lag(tx_curr, n = 2, order_by = period)) %>% 
    ungroup() %>% 
    filter(!is.na(vlc))
  
  #visualize trends across time
  df_vlc_psnu %>% 
    ggplot(aes(period, vlc, group = psnu)) +
    geom_line(color = scooter) +
    geom_point(color = scooter) +
    geom_area(alpha = .3, fill = scooter) +
    facet_wrap(~fct_reorder2(psnu, period, tx_curr)) +
    scale_y_continuous(label = label_percent()) +
    labs(x = NULL, y = NULL,
         title = "Are we seeing declines in any PSNU?",
         caption = glue("VLC = TX_PVLS_D / TX_CURR_lag2
                        {metadata$caption}")) +
    si_style_ygrid()
  
  #export
  si_save(file.path(folderpath_tmp, "fy60q3_vlc_01_psnu-trends.png"))

# VIZ - SITE DISTRIBUTION -------------------------------------------------

  #aggregate data to site level and reshape wide to calc vlc (only keep curr pd)
  df_vlc_site <- df_vlc %>% 
    group_by(psnu, orgunit, period, indicator) %>% 
    summarise(value = sum(value, na.rm = TRUE),
              .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(psnu, orgunit) %>% 
    mutate(vlc = tx_pvls_d / lag(tx_curr, n = 2, order_by = period)) %>% 
    ungroup() %>% 
    filter(!is.na(vlc),
           period == max(period)) %>% 
    mutate(large_site = tx_curr > quantile(tx_curr, .8))
  
  #visualize distribution of vlc across sites, flagging large sites
  df_vlc_site %>% 
    ggplot(aes(vlc, fct_reorder(psnu, tx_curr, sum), size = tx_curr,
               color = large_site)) +
    geom_vline(xintercept = .7, alpha = .7, linetype = "dashed") +
    geom_point(alpha = .4, position = position_jitter(height = .3, width =  0, seed = 42)) +
    geom_text_repel(data = df_vlc_site %>% filter(large_site & vlc < .7),
                    aes(label = orgunit), size = 2,
                    family = "Source Sans Pro", color = matterhorn) +
    scale_x_continuous(limits = c(0,1),
                       oob = squish,
                       label = label_percent()) +
    scale_color_manual(values = c(trolley_grey_light, moody_blue)) +
    labs(x = NULL, y = NULL,
         title = "What large sites do we need to prioritize with low VLC?",
         subtitle = glue("Large sites defined as having more than {quantile(df_vlc_site$tx_curr, .8) %>% round} patients and low VLC as less than 70%"),
         color = NULL, size = NULL,
         caption = glue("VLC = TX_PVLS_D / TX_CURR_lag2
                        {metadata$caption}")) +
    si_style() +
    theme(legend.position = "none")
  
  #export
  si_save(file.path(folderpath_tmp, "fy60q3_vlc_02_site-distro.png"))

# VIZ - AGE/SEX LEVELS FOR FLAGGED SITES ----------------------------------

  #list large and low performing sites
  v_flag_orgunits <- df_vlc_site %>% 
    filter(vlc < .7 & large_site) %>% 
    distinct(orgunit) %>% 
    pull()
  
  #aggregate data to site level and reshape wide to calc vlc (only keep curr pd)
  df_flag <- df_vlc %>% 
    filter(orgunit %in% v_flag_orgunits) %>% 
    group_by(psnu, orgunit, period, indicator, sex, ageasentered) %>% 
    summarise(value = sum(value, na.rm = TRUE),
              .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(psnu, orgunit, sex, ageasentered) %>% 
    mutate(vlc_denom = lag(tx_curr, n = 2, order_by = period),
           vlc = tx_pvls_d /vlc_denom) %>% 
    ungroup() %>% 
    filter(!is.na(vlc),
           period == max(period)) %>% 
    mutate(fill_color = ifelse(vlc < .7, burnt_sienna, trolley_grey))
  
  #function to plot age/sex col chart by site
  viz_iteration <- function(facility, export_path = NULL){
    
    df_flag <- filter(df_flag, orgunit == facility)
    
    df_flag_sum <- df_flag %>% 
      group_by(psnu,orgunit) %>% 
      summarise(across(c(tx_pvls_d, vlc_denom), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>% 
      mutate(vlc = tx_pvls_d /vlc_denom)
    
    df_flag %>% 
      ggplot(aes(y = ageasentered)) +
      geom_col(aes(vlc_denom), color = trolley_grey_light, alpha = .4) +
      geom_col(aes(tx_pvls_d, fill = fill_color), width = .5) +
      geom_text(data = df_flag %>% filter(vlc < .7),
                aes(tx_pvls_d, label = label_percent(1)(vlc)),
                color = matterhorn, family = "Source Sans Pro",
                hjust = -.2, size = 9/.pt) +
      facet_wrap(~sex) +
      scale_fill_identity() +
      labs(x = NULL, y = NULL,
           title = glue("{label_percent(1)(df_flag_sum$vlc)} of {df_flag_sum$orgunit}'s eligible {label_comma()(df_flag_sum$vlc_denom)} patients have a documented VL result "),
           subtitle = "Low VLC flagged (orange) where TX_PVLS_D is less than 70% of TX_CURR (light background bar)",
           caption = glue("VLC = TX_PVLS_D / TX_CURR_lag2
                        {metadata$caption}")) +
      si_style_xgrid() +
      theme(plot.title = element_markdown())
    
    if(!is.null(export_path))
      si_save(file.path(export_path, glue("fy60q3_vlc_{str_remove_all(df_flag_sum$psnu, ' ')}-{str_remove_all(df_flag_sum$orgunit, ' ')}")))
  }
  

  #create files
  walk(v_flag_orgunits,
       ~viz_iteration(.x, folderpath_tmp))
  

# UPLOAD ------------------------------------------------------------------

  list.files(folderpath_tmp, full.names = TRUE) %>% 
    walk(~drive_upload(.x,
                       path = as_id(drive_id),
                       name = basename(.x)))

    