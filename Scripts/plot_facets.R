# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  develop visuals to explain facets
# REF ID:   b525eee4 
# LICENSE:  MIT
# DATE:     2023-08-03
# UPDATED: 

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
  
  ref_id <- "b525eee4" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df_msd <- return_latest("Data", "PSNU_IM") %>% 
    read_psd()

# MUNGE -------------------------------------------------------------------


  df_tst_psnu <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           psnu %in% c("Albuquerque", "Eugene", "Great Lakes")) %>%
    group_by(fy = fiscal_year, psnu, indicator) %>% 
    summarize(cumulative = sum(cumulative, na.rm = T),
              .groups = "drop")
  
  unique(df_tst_psnu$psnu) %>% sort
  
  
  v1 <- df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative,
               fill = psnu)) +
    geom_col()
  
  v2 <- df_tst_psnu %>% 
    ggplot(aes(x = fy, 
               y = cumulative,
               fill = psnu)) +
    geom_col() + 
    facet_wrap(~psnu)
  
  v1 / v2
  
  si_save("Images/facet.png", width = 5, height = 5)
