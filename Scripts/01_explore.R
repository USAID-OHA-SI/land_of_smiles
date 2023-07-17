# PROJECT:  land_of_smiles
# AUTHOR:   T. Essam | USAID
# PURPOSE:  recreate small mulitples viz
# REF ID:   fb832bac 
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

  ref_id <- "fb832bac" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements
  
  metadata$caption <- str_replace(metadata$caption, metadata$curr_fy_lab, "FY60")


# IMPORT ------------------------------------------------------------------

  #trainign data available for download from GDrive
  # https://drive.google.com/drive/folders/1Dmo7NqtBCbexsaq6XpkN1MZlTn-s2Jkz
  df <- return_latest("Data", "PSNU") %>% 
    read_psd()   


# MUNGE ============================================================================
  
  # Create a list of key partners we want to focus analysis on
  df %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_PVLS"), 
           standardizeddisaggregate == "Total Numerator", 
           fiscal_year == 2060) %>% 
    group_by(funding_agency, prime_partner_name, fiscal_year, indicator) %>% 
    summarise(tgt = sum(targets, na.rm = T)) %>% 
    spread(indicator, tgt)
    
    distinct(funding_agency, prime_partner_name, mech_code, mech_name) %>% 
    arrange(funding_agency) %>% 
    prinf()

