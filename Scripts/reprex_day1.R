# PROJECT:  land_of_smiles
# AUTHOR:   T. Essam & A. Chafetz | USAID
# PURPOSE:  snippets from day 2
# LICENSE:  MIT
# DATE:     2023-07-24
# UPDATED:  2023-08-14

# 02-Project Setup & Data Prep --------------------------------------------
  library(tidyverse)
  library(gagglr)


# Working directory screenshot
getwd()

# Relative path
getwd()
dir()

# Folder setup
library(glamr)
glamr::folder_setup()

# list files
dir()
list.files("Data")

# list files pattern
dir()
list.files("Data", pattern = ".zip")
list.files("Data", pattern = "PSNU")

# list files full names
dir()
list.files("Data", pattern = "PSNU", full.names = TRUE)

# list files for pointing to path
list.files("Data")
subnat_path <- list.files("Data", pattern = "NAT_SUBNAT", full.names = T)
subnat_path

# Exercise
library(gophr)
msd_path <- list.files("Data", pattern = "PSNU", full.names = T)
msd_path
read_psd(msd_path)

df_msd <- read_psd(msd_path)



# 03a-Data Inspection -----------------------------------------------------

# Inspecting data
df_msd
head(df_msd)
names(df_msd)
View(df_msd)
str(df_msd)
glimpse(df_msd)

# Checking for uniqueness
unique(df_msd$mech_code)

df_msd %>% 
  distinct(mech_code)

df_msd %>% 
  count(mech_code) %>% 
  print(n = 10)


#Exercise solutions

unique(df_msd$country)

unique(df_msd$snu1) %>% length()

df_msd %>% count(indicator)

# Renaming
names(df_msd)[1:10]
df_msd_renamed <- 
  df_msd %>% 
  rename(ou = operatingunit)
names(df_msd_renamed)[1:10]


# 03b-Isolating Data ----------------------------------------------------------

# filter
df_msd %>% count(indicator)
df_msd %>% 
  filter(indicator == "TX_CURR") %>% 
  count(indicator)

df_msd %>% 
  count(indicator) %>% 
  arrange(n)

df_msd %>% select(1:10) %>% names()
df_msd %>% select(c(operatingunit, psnu))

# Exercise solutions

# Rename
df_msd %>% 
  rename(sourcename = source_name) %>% 
  names()

df_msd %>% 
  select(sourcename = source_name) %>% 
  names()

# Filter
df_msd %>% 
  filter(indicator == "HTS_TST")

# Arrange
df_msd %>% 
  count(indicator, sort = T)

df_msd %>% 
  count(indicator) %>% 
  arrange(desc(n))

# Select
df_mechs <- 
  df_msd %>% 
  select(c(mech_name, mech_code))

df_mechs <- 
  df_msd %>% 
  select(contains("mech"))


# 04-Summarizing ----------------------------------------------------------

  #review data
  glimpse(df_msd)

  #create a HTS dataframe
  df_hts <- filter(df_msd, indicator == "HTS_TST") 
  
  ?count
  
  #count # tested for HIV
  df_hts %>% 
    count(wt = cumulative)
  
  df_hts %>% 
    count(indicator, fiscal_year, standardizeddisaggregate, 
          wt = cumulative)
  
  #exploring variables
  df_hts %>% 
    distinct(modality)
  
  df_hts %>% 
    count(modality)
  
  unique(df_hts$modality)
  
  ?arrange
  
  #order modalities by cumulative value (Default = ascending)
  df_hts %>% 
    filter(standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative) %>% 
    arrange(n)
  
  #order modalities by cumulative value descending
  df_hts %>% 
    filter(standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative) %>% 
    arrange(desc(n))

  #can use sort = TRUE instead of extra line
  df_hts %>% 
    filter(standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2060) %>% 
    count(indicator, fiscal_year, modality, wt = cumulative, sort = TRUE)
  
  #multiple input parameters
  df_hts %>% 
    filter(modality == "Index") %>% 
    count(fiscal_year, indicator, modality, wt = cumulative)
  
  df_hts %>% 
    filter(modality %in% c("Index", "IndexMod")) %>% 
    count(fiscal_year, indicator, modality, wt = cumulative)
  
  ?summarize
  
  #index testing dataframe
  df_index <- df_hts %>% 
    filter(modality %in% c("Index", "IndexMod")) 
  
  #na handing
  df_index %>% 
    summarize(cumulative = sum(cumulative))
  
  df_index %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE))
  
  #applying a ground variable
  ?group_by
  
  df_index %>%
    group_by(fiscal_year, modality) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()
  
  df_index %>%
    group_by(fiscal_year) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()
  
  #various ways to do the same thing
  df_index %>% 
    count(fiscal_year, modality, wt = cumulative, 
          name = "cumulative")
  
  df_index %>%
    group_by(fiscal_year, modality) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()
  
  df_index %>% 
    group_by(fiscal_year, modality) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop")
  
  df_index %>%
    summarize(cumulative = sum(cumulative, na.rm = TRUE),
              .by = c(fiscal_year, modality))
  
  #aggregating multiple variables
  df_index %>% 
    group_by(fiscal_year) %>% 
    summarize(cumulative = sum(cumulative, na.rm = TRUE),
              targets = sum(targets, na.rm = TRUE),
              .groups = "drop")
  
  #aggregating multple variables simplified
  ?starts_with
  
  df_index %>% 
    filter(fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, starts_with("qtr"), cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  ?where
  
  df_index %>% 
    filter(fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(where(is.double), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  #using different functions in an aggregation
  df_index %>% 
    group_by(fiscal_year) %>% 
    summarize(max_value = max(cumulative, na.rm = TRUE),
              total_records = n(),
              .groups = "drop")
  
  

# 06-Mutating -------------------------------------------------------------

  df_index_psnu <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% c("Index", "IndexMod"),
           fiscal_year == 2060) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), 
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets))

  #calculate achievement
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets)
  
  #multiple muates can be done in the same function
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets) %>% 
    mutate(achievement_str = percent(achievement, 1))
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           achievement_str = percent(achievement, 1))
  
  #mutate can both add new variables and adjust existing ones
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu))
  
  #boolean logic - TRUE/FALSE
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < .4)
  
  #make it dynamic
  # str(metadata)
  # 
  # df_index_psnu %>% 
  #   mutate(achievement = cumulative / targets,
  #          psnu = toupper(psnu),
  #          low_achv = achievement < (metadata$curr_qtr/4) - .1)
  
  #logic based variables - if...then
  ?ifelse
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = ifelse(low_achv == TRUE, "purple", "#909090"))
  
  #multi conditional variables
  ?case_when
  
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           low_achv = achievement < (metadata$curr_qtr/4) - .1,
           achv_color = case_when(low_achv == TRUE ~ "purple",
                                  is.infinite(achievement) ~ NA_character_,
                                  TRUE ~ "#909090"))
  
  #mutating based on string
  df_index_psnu %>% 
    mutate(achievement = cumulative / targets,
           psnu = toupper(psnu),
           contains_q = ifelse(str_detect(psnu, "Q"), cumulative, NA))
  
  #PSNU index testing trends data frame
  df_index_psnu_trend <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           modality %in% c("Index", "IndexMod")) %>%
    group_by(fiscal_year, psnu) %>% 
    summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(desc(targets))
  
  #group based calculations
  ?lag
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           targets_prior = lag(targets),
           max_targets = max(targets)) %>% 
    arrange(psnu, fiscal_year)
  
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets,
           targets_prior = lag(targets),
           max_targets = max(targets))
  
  df_index_psnu_trend %>% 
    mutate(achievement = cumulative / targets) %>% 
    group_by(psnu) %>% 
    mutate(targets_prior = lag(targets, order_by = fiscal_year),
           max_targets = max(targets)) %>% 
    ungroup() %>% 
    arrange(psnu, fiscal_year)
  


# 07-Joining --------------------------------------------------------------

  #load the df_subnat data
  subnat_path <- list.files("Data", pattern = "SUBNAT", full.names = TRUE)
  df_subnat <- read_psd(subnat_path)
  
  # Practice joining the interational partner labels to the mechanisms
  df_msd %>% distinct(funding_agency, prime_partner_name, mech_code, mech_name)
  
  # Should this be a URL on github?
  # An excel file?
  library(googlesheets4)
  load_secrets()
  df_lp <- read_sheet(ss = "17AN5LjuPk7lMBitf4UPmEXEPdf4T-qWBTFTvjISiPYI")

  df_msd_subset <- 
    df_msd %>% 
    filter(indicator == "HTS_TST")
  
  # Should break because of incompatible types
  left_join(df_msd_subset, df_lp)
  
  
  # Try this again with proper columns
  # Should this be a URL on github?
  # An excel file?
  library(googlesheets4)
  load_secrets()
  df_lp <- read_sheet(ss = "17AN5LjuPk7lMBitf4UPmEXEPdf4T-qWBTFTvjISiPYI", sheet = 2)
  
  df_msd_subset <- 
    df_msd %>% 
    filter(indicator == "HTS_TST")
  
  # Should break because of incompatible types
  tidylog::left_join(df_msd_subset, df_lp)
  tidylog::right_join(df_msd_subset, df_lp)
  tidylog::full_join(df_msd_subset, df_lp)
  
  # How many partners do we expect to match?
  df_msd_mechs <- df_msd_subset %>% distinct(mech_code, mech_name)
  df_lp_mechs <- df_lp %>% distinct(mech_code, mech_name)
  
  # How to check overlap?
  intersect(df_lp_mechs$mech_code, df_msd_mechs$mech_code)
  setdiff(df_lp_mechs$mech_code, df_msd_mechs$mech_code)
  setequal(df_lp_mechs$mech_code, df_msd_mechs$mech_code)
  
  

# 08-Reshaping ------------------------------------------------------------

  df_semi <- df_msd %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           modality == "Index",
           psnu %in% c("Eugene")) %>% 
    group_by(fiscal_year, psnu, indicator) %>% 
    summarize(across(c(starts_with("qtr")), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    arrange(indicator)
  
  ?pivot_longer
  ?pivot_wider
  
  
  (df_long <- df_semi %>% 
    pivot_longer(starts_with("qtr"),
                 names_to = "quarter",
                 names_prefix = "qtr",
                 values_to = "value"))
  
  
  df_long %>% 
    unite(period, c(fiscal_year, qtr), sep = "Q") %>% 
    mutate(period = str_replace(period, "20", "FY")) %>% 
    pivot_wider(names_from = indicator, 
                values_from = value) %>% 
    mutate(positivity = HTS_TST_POS/HTS_TST)
  
  df_semi
  
  
  df_subnat <- read_psd("data/MER_Structured_TRAINING_Datasets_NAT_SUBNAT_FY58-60_20230616_v2_1.zip")
  
  
  df_subnat %>% 
    filter(standardizeddisaggregate %in% c("Age/Sex", "Age/Sex/HIVStatus")) %>% 
    count(fiscal_year, psnu, indicator, wt = targets)

  