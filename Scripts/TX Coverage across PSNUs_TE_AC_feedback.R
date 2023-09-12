# We didn't go over this a lot in the course, but you may consider adding a few comments at the top


# ---
# PROJECT: GDSW Minoria Data Analysis
# PURPOSE: Visualize TX_Coverage across PSNUS to better understand unmet neet
# AUTHOR:  Tim Esssam | SI
# REF ID:  f2195992
# LICENSE: MIT
# DATE:   2023-09-01
# NOTES:  Generated for capstone exercise


# Visualize TX_Coverage across PSNUs, to better understand unmet need by PSNU and coarse age disags

# SETUP -------------------------------------------------------------------

# Library
library(readxl)
library(tidyverse)
library(gagglr)
#library(tidylog) - This can generate a lot of output, may consider using iteratively
# using the tidylog::function_name() option

# Load msd data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()
glimpse(df_msd)

# Load subnath data
df_subnat <- return_latest("Data", "NAT_SUBNAT") %>%
  read_psd()
glimpse(df_subnat)

# Filter & summarize datasets to merge by psnu
# By standardized disaggregate = Age/Sex/HIVStatus
 

# Be careful about overwriting the original msd -- just in case you need to go back 
# to the original data and do some additional derivations
# I've added a stub at the end of each data frame to preserve the msd and subnat dfs

df_msd_tx <- df_msd %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year == "2060") %>% 
  group_by(fiscal_year, indicator, psnu, trendscoarse) %>% 
  summarise(cumulative = sum(cumulative,na.rm = TRUE)) %>% 
  ungroup()


df_subnat_plhiv <- df_subnat %>% 
   filter(indicator == "PLHIV",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year == "2060") %>% 
  group_by(fiscal_year, indicator, psnu, trendscoarse) %>% 
  summarise(targets = sum(targets,na.rm = TRUE)) %>% 
  ungroup()

# EXCELLENT BEST PRACTICE!! Checking column names before merging
#compare the columns in each data set?
  names(df_msd_tx)  
  names(df_subnat_plhiv)  
  
# Joining the datasets
  tidylog::left_join(df_msd, df_subnat)
  tidylog::right_join(df_msd, df_subnat)
  
# Full join of the datasets
  tidylog::full_join(df_msd,df_subnat)

# Create new dataset for the joined dbs
  df_msd_total <- tidylog::full_join(df_msd,df_subnat) %>% 
    group_by(fiscal_year,indicator, psnu,trendscoarse) %>% 
    summarise(value=sum(cumulative,targets,na.rm = TRUE)) %>% 
  ungroup()

# Alternative method to joining the data ====
# Because each dataset has only 1 indicator (TX_CURR and PLHIV)
# You can rename the cumulative and the targets column into these names
# This will let you merge the data with a left_join
  
  # You can remove the indicator column from the dataframe and rename the
  # aggregation. 
  df_tx <- df_msd %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year == "2060") %>% 
    group_by(fiscal_year, psnu, trendscoarse) %>% 
    summarise(TX_CURR = sum(cumulative,na.rm = TRUE)) %>% 
    ungroup()
  
  
  df_plhiv <- df_subnat %>% 
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year == "2060") %>% 
    group_by(fiscal_year, psnu, trendscoarse) %>% 
    summarise(PLHIV = sum(targets,na.rm = TRUE)) %>% 
    ungroup()
  
  # With the indicator column gone, we can now left join the two data frames
  df_tx_plhiv <- 
    df_tx %>% 
    left_join(., df_plhiv, by = c("fiscal_year", "psnu", "trendscoarse")) %>% 
    mutate(tx_cov_gap = PLHIV - TX_CURR,
           tx_cov_sh = TX_CURR/PLHIV) %>% 
    group_by(trendscoarse) %>% 
    mutate(psnu_order_gap = fct_reorder(psnu, tx_cov_gap),
           psnu_order_sh = fct_reorder(psnu, tx_cov_sh)) %>% 
    ungroup()
  
  # Additional munging to be used in the vis
  # Let's do the following:
  # 1) Flag and recolor psnus where the gap sh is >1
  
  df_tx_plhiv %>% 
    mutate(psnu_gap_color = case_when(
      tx_cov_sh > 1.02 ~ scooter_med,
      TRUE ~ old_rose
    )) %>% 
    ggplot(aes(x = tx_cov_sh, y = psnu_order_sh, group = psnu)) +
    geom_vline(xintercept = 1) +
    geom_segment(aes(x = tx_cov_sh, xend = 1, yend = psnu_order_sh), 
                 linewidth = 2, color = grey20k) +
    geom_point(aes(color = psnu_gap_color), size = 5) +
    geom_text(aes(label = percent(tx_cov_sh, 1)), position = position_nudge(y = 0.5)) +
    labs(title = "TX_Coverage across PSNUs", 
         x = NULL, y = NULL,
         caption = "Source: Minoria FY60Q2c MSD") +
    facet_wrap(~trendscoarse) +
    scale_color_identity() +   
    si_style_xgrid(facet_space = 2) +
    theme(axis.text.x = element_blank(), 
          panel.spacing = unit(3, "picas")) 
  
  si_save("Images/TX_coverage_across_psnus_by_trendscoarse.png")
  
# compute treatment coverage with indicator values for PLHIV and TX_CURR
  
  ?pivot_wider
  
  df_msd_final <- df_msd_total %>% 
    pivot_wider(names_from = "indicator", 
                values_from = "value")

  view(df_msd_final)
  
  
  #Calculate TX_Coverage
  df_msd_final <- df_msd_final %>% 
    mutate(TX_Coverage = TX_CURR/PLHIV) %>% 
    mutate(psnu_order = fct_reorder(psnu, TX_Coverage))
  
  
  #plot TX_Coverage
  df_msd_final%>% 
    ggplot(aes(x = TX_Coverage, y = psnu_order, group = psnu))+
    geom_line(color = "grey60")+
    geom_point(color = "#2057a7", size = 5)+
    labs(title = "TX_Coverage across PSNUs")
  

  si_save("Images/Treatment_Coverage2")

  
   
  