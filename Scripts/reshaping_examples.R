# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  example dataframes for different data shapes
# REF ID:   2673ed75 
# LICENSE:  MIT
# DATE:     2023-08-15
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(clipr)


# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "2673ed75" #id for adorning to plots, making it easier to find on GH
  
  get_metadata("Data", "PSNU_IM") #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df_msd <- return_latest("Data", "PSNU_IM") %>% 
    read_psd()   
  

# MUNGE -------------------------------------------------------------------

  
  df_semi <- df_msd %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           modality == "Index",
           psnu %in% c("Eugene")) %>% 
    group_by(fiscal_year, psnu, indicator) %>% 
    summarize(across(c(starts_with("qtr")), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(indicator)

  df_semi %>% 
    write_clip()
  
  df_semi %>% 
    reshape_msd("wide") %>% 
    write_clip()
  
  df_semi %>% 
    reshape_msd("long", include_type = FALSE) %>% 
    write_clip()
  