# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Exercise prep
# REF ID:   6f581e32 
# LICENSE:  MIT
# DATE:     2023-07-18
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
  library(openxlsx)
  library(googledrive)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "6f581e32" #id for adorning to plots, making it easier to find on GH
  
  get_metadata("Data", "PSNU_IM") #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df <- return_latest("Data", "PSNU_IM") %>% 
    read_psd()   
  

# MUNGE -------------------------------------------------------------------

  #Guiding exercise: Are there any PSNUs where we are facign declines in
  # index testing positivity for the Lugnuts partner for 15-25 year olds? 
  
  df_ex <- df %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           modality %in% c("Index", "IndexMod"),
           prime_partner_name == "Lugnuts") %>%
    summarise(across(where(is.double), ~ sum(., na.rm = TRUE)),
              .by = c(operatingunit, snu1, psnu, prime_partner_name, 
                      indicator, ageasentered, modality, fiscal_year))
  

# EXPORT FOR EXERCISE -----------------------------------------------------

  write.xlsx(df_ex, "Dataout/exercise_untidy.xlsx", overwrite = TRUE)

  # drive_upload("Dataout/exercise_untidy.xlsx",
  #              path = as_id("1Dmo7NqtBCbexsaq6XpkN1MZlTn-s2Jkz"),
  #              type = "spreadsheet")
  
# VIZ MUNGING -------------------------------------------------------------

  df_viz <- df_ex %>% 
    filter(ageasentered %in% c("15-19", "20-24")) %>% 
    summarise(across(starts_with("qtr"), ~ sum(., na.rm = TRUE)),
              .by = c(operatingunit, snu1, psnu, prime_partner_name, 
                      indicator, modality, fiscal_year)) %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator) %>% 
    filter(HTS_TST > 0) %>% 
    mutate(positivity = HTS_TST_POS / HTS_TST)


# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(period, positivity, group = psnu)) +
    geom_line() +
    geom_point() +
    facet_wrap(~psnu) +
    scale_y_continuous(labels = percent) +
    labs(x = NULL, y = NULL, 
         caption = metadata$caption) +
    theme_minimal()
  