# PROJECT:  land_of_smiles
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  mask API pull
# REF ID:   89c8432d 
# LICENSE:  MIT
# DATE:     2023-08-17
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "89c8432d" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df <- read_csv("Dataout/datim-vlc.csv")
  
  df_milb_geo <- read_csv("../themask/Dataout/geo_map.csv")
  
  
  sitetype <- c("Clinic", "Urban Clinic", "Health Post",
                "Health Centre", "Urban Clinic", "Hospital",
                "District Hospital")
  
  terms <- c("Assist", "Balk", "Base", "Bench", "Bunt", "Changeup", "Curveball", 
             "Cutter", "Diamond", "Double", "Error", "Exit Velocity", "Fastball",
             "Field", "Fly", "Forkball", "Free Agent", "Hit", "Home Run", 
             "Knuckleball", "Loss", "Out", "Pickoff", "Pitch", "Plate", "Putout",
             "Run", "Save", "Score", "Scout", "Shift", "Sinker", "Slider", 
             "Southpaw", "Splitter", "Stolen Base", "Strikeout", "Texas Leager",
             "Triple", "Walk", "Wild Pitch", "Win", "Zone")
  
  # GEN SITE NAME -----------------------------------------------------------
  
  gen_sitename <- function(){
    name <- sample(terms, 1)
    type <- sample(sitetype, 1)
    num <-  seq(1,99) %>% 
      stringr::str_pad(3, pad = "0") %>% 
      sample(1)
    sitename <- paste(name, type, num)
    
    return(sitename)
  }
  
  set.seed(41)
  df_sitename <- tibble(orgunituid = unique(df$orgunituid),
                        orgunit_milb = replicate(length(unique(df$orgunituid)), gen_sitename())
  )
  
  # length(unique(df_sitename$orgunit_milb)) == length(unique(df$orgunituid))

# MUNGE -------------------------------------------------------------------

  df_milb_geo <- df_milb_geo %>%
    select(psnuuid, ends_with("_milb"))

  df_masked<- df_milb_geo %>% 
    right_join(df) %>% 
    left_join(df_sitename) %>%
    select(-c(psnuuid, orgunit, orgunituid)) %>%
    rename_all(~str_remove(., "_milb"))

  df_masked <- df_masked %>% 
    select(operatingunit, operatingunituid, country, snu1, psnu, psnuuid, orgunit,
           period,
           indicator, sex, ageasentered, numeratordenom, 
           value)
    
  df_masked <- df_masked %>% 
    mutate(period = period %>% 
             str_replace("21", "58") %>% 
             str_replace("22", "59") %>% 
             str_replace("23", "60")
    )
  
  write_csv(df_masked, "Dataout/datim-vlc.csv", na = "")
  