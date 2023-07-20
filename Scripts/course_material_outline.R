# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  full week outline
# REF ID:   faf9fe20 
# LICENSE:  MIT
# DATE:     2023-07-20
# UPDATED: 


# FOLDER SETUP ------------------------------------------------------------

#setup folder structure in the project from glamr (only needs to be run once)
folder_setup()


###########################################################################
##
## GUIDING PROMPT FOR THE TRAINING
## The USAID/Minoria Mission Director is interesting in optimizing the 
## allocation of resources to ensure we find unidentified HIV+ people and get
## them onto treatment. We, as the SI team, need to explore the data and provide
## outputs that identify the historic target achievement of the testing program,
## trends, and other useful information to assist our technical team and A/CORs
## USAID/Minoria, giving them the data needed to inform both discussions with 
## partners and programmatic decisions.
##
###########################################################################


# BASIC OPERATIONS --------------------------------------------------------

  #value of variable m is 3 x 3
  m <- 3*3
  
  #the city we want to reference
  city <- "Cedar Rapids"
  
  #a list of important partners
  partners <- c("Yard Goats", "Loons", "Isotopes")
  
  #nested statement without pipes
  paste0(toupper(partners), collapse = ", ")
  
  #unnested
  p2 <- toupper(partners) 
  paste0(p2, collapse = ", ")
  
  #pipe (CTRL + SHIFT + M)
  partners |> toupper() |> paste0(collapse = ", ")
  partners %>% toupper() %>%  paste0(collapse = ", ")
  
  #dataframe are objects too
  df <- data.frame(x = c("A", "B", "C"),
                    y = c(1, 2, 3))
  
  df$z <- df$y + m

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
  
  ref_id <- "faf9fe20" #id for adorning to plots, making it easier to find on GH
  
  get_metadata("Data", "PSNU_IM") #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df_msd <- return_latest("Data", "PSNU_IM") %>% 
    read_psd()   
  

# INSPECT DATA ------------------------------------------------------------

  #viewing the data in different ways
  df_msd
  
  head(df_msd)
  
  names(df_msd)
  
  View(df_msd)
  
  str(df_msd)
  
  glimpse(df_msd)
  
  
  #getting back more specific information for columns
  unique(df_msd$prime_partner_name) #vector
  ?distinct
  distinct(df_msd, prime_partner_name) #tibble
  
  ?count
  count(df_msd, indicator)
  count(df_msd, indicator, standardizeddisaggregate)
  count(df_msd, indicator, standardizeddisaggregate, wt = targets)
  count(df_msd, indicator, standardizeddisaggregate, wt = targets, sort = TRUE)
  

# RENAME VARIABLES --------------------------------------------------------
  
  ?rename
  rename(df_msd, sourcename = source_name) %>% names()
  ?select
  select(df_msd, sourcename = source_name) %>% names()
  select(df_msd, everything(), sourcename = source_name) %>% names()
  

# FILTERING ---------------------------------------------------------------

  ?filter
  
  df_msd %>% 
    filter(indicator == "HTS_TST")
  
  df_msd %>% 
    filter(indicator == "HTS_TST") %>% 
    count(indicator)
  

# SUMMARIZING AND GROUPING ------------------------------------------------

  df_msd %>% 
    filter(indicator == "HTS_TST") %>% 
    count(indicator, wt = cumulative)
  
  ?count
  
  df_msd %>% 
    filter(indicator == "HTS_TST") %>% 
    count(indicator, fiscal_year, standardizeddisaggregate, wt = cumulative)
  
  df_msd %>% 
    filter(indicator == "HTS_TST") %>% 
    count(indicator, fiscal_year, modality, wt = cumulative)
  
  unique(df_msd$modality)
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative)
  
  ?arrange
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative) %>% 
    arrange(n)
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative) %>% 
    arrange(desc(n))
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative, sort = TRUE)
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality == "Index") %>% 
    count(fiscal_year, indicator, modality, wt = cumulative)
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod")) %>% 
    count(fiscal_year, indicator, modality, wt = cumulative)
  
  ?summarize
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod")) %>% 
    summarize(cumulative = sum(cumulative))
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod")) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE))
  
  ?group_by
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod")) %>%
    group_by(modality) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod")) %>%
    group_by(fiscal_year, modality) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop")
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod")) %>%
    group_by(fiscal_year) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop")
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop")
  
  ?across
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, qtr1, qtr2, qtr3, qtr4, cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  ?starts_with
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, starts_with("qtr"), cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  ?where
  
  df_msd %>% 
    filter(indicator == "HTS_TST",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(where(is.double), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  

# MUTATE ------------------------------------------------------------------

  
  df_index_psnu <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets))
  
  df_index_psnu
  
  ?mutate
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets)
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets) %>% 
    mutate(achievement_str = percent(achievement, 1))
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           achievement_str = percent(achievement, 1))
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu))
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           achievement = na_if(achievement, Inf))
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < (2/4) - .1)
  
  str(metadata)
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < (metadata$curr_qtr/4) - .1)
  
  ?ifelse
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = ifelse(low_achv == TRUE, "purple", "#909090"))
  
  ?case_when
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = case_when(low_achv == TRUE ~ "purple",
                                  is.infinite(achievement) ~ NA_character_,
                                  TRUE ~ "#909090"))
  
  df_index_psnu_trend <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% c("Index", "IndexMod")) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets))
  
  ?lag
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = case_when(low_achv == TRUE ~ "purple",
                                  is.infinite(achievement) ~ NA_character_,
                                  TRUE ~ "#909090"),
           targets_prior = lag(targets))
  
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = case_when(low_achv == TRUE ~ "purple",
                                  is.infinite(achievement) ~ NA_character_,
                                  TRUE ~ "#909090")) %>% 
    group_by(psnu) %>% 
    mutate(targets_prior = lag(targets, order_by = fiscal_year)) %>% 
    arrange(psnu, fiscal_year)
  
  
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = case_when(low_achv == TRUE ~ "purple",
                                  is.infinite(achievement) ~ NA_character_,
                                  TRUE ~ "#909090")) %>% 
    group_by(psnu) %>% 
    mutate(targets_prior = lag(targets, order_by = fiscal_year),
           achievement_prior = lag(achievement, order_by = fiscal_year)) %>% 
    ungroup() %>% 
    filter(fiscal_year == max(fiscal_year))
  


# MERGE & APPEND ----------------------------------------------------------

  df_subnat <- return_latest("Data", "NAT_SUBNAT") %>% 
    read_psd() 

  glimpse(df_subnat) 
  
  
  df_plhiv <- df_subnat %>% 
    filter(#fiscal_year == max(df_msd$fiscal_year),
           indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, psnu) %>% 
    summarise(plhiv = sum(targets, na.rm = TRUE),
              .groups = "drop")
  
  ?left_join
  
  df_index_psnu_trend %>% 
    left_join(df_plhiv) %>% 
    arrange(psnu, fiscal_year)
  
  df_plhiv_latest <- df_plhiv %>% 
    filter(fiscal_year == max(df_msd$fiscal_year)) %>% 
    select(-fiscal_year)
  
  df_index_psnu_trend %>% 
    left_join(df_plhiv_latest) %>% 
    arrange(psnu, fiscal_year)
  
  ?right_join
  
  df_index_psnu_trend %>% 
    filter(fiscal_year == 2060) %>% 
    right_join(df_plhiv_latest)
  
  
  ?full_join
  
  df_index_psnu_trend %>% 
    filter(fiscal_year == 2060) %>% 
    full_join(df_plhiv_latest)
  