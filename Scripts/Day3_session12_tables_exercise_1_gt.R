# Exercise: Day3_session12_tables_exercise_gt


# SETUP -------------------------------------------------------------------
  
  # Install gt and gtExtras if needed
  remotes::install_github("rstudio/gt")
  remotes::install_github("jthomasmock/gtExtras")

  # Library
  library(tidyverse)
  library(gagglr)
  library(scales)
  library(gt)
  library(gtExtras)

# Data - Make sure to downlaod `MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-60_20230616_v2_1.zip` file and move it to `Data` folder

  # GIS - Make sure the GIS folder has the proper shapefiles
  # Load PSNU x IM data
  df_msd <- return_latest("Data", "PSNU_IM") %>%
    read_psd()

  # Set core data frame for exercises and examples
  # Sequence of operations
  # 1) Filter HTS POS indicator and 2060 fiscal year
  # 2) summarize targets and cumulative results by fiscal year, psnu, snu1 and indicator
  # 3) Create a new variable (fy) that transform fiscal year variable as character type
    df_tst_psnu <- df_msd %>%
      filter(indicator == "HTS_TST_POS") %>%
      group_by(fiscal_year, snu1, psnu) %>% 
      summarize(across(c(cumulative, targets), 
                       \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>% 
      mutate(achievement = cumulative / targets)
  
  # Take a look at the structure of the spatial data
    glimpse(df_tst_psnu)
    names(df_tst_psnu)   

# EXERCISE ----------------------------------------------------------------

  #Instruction: Use the df_tst_psnu data frame to explore gt()
  # Create a summary table of testing results by psnu
  # Consult the fmt_number() and fmt_percent() functions to learn about formatting
  
  df_tst_psnu %>% 
    gt() %>% 
    fmt_number(columns = c("...", "..."), 
               decimals = "...") %>% 
    fmt_percent(columns = "...", 
                decimals = "...")
  
  # Experiment between groupname_col and rowname_col arguments within gt() function
  df_tst_psnu %>% 
    gt(groupname_col  = "...") %>% 
    fmt_number(columns = c("...", "..."), 
               decimals = "...") %>% 
    fmt_percent(columns = "...", 
                decimals = "...")
  
  df_tst_psnu %>% 
    gt(rowname_col  = "...") %>% 
    fmt_number(columns = c("...", "..."), 
               decimals = "...") %>% 
    fmt_percent(columns = "...", 
                decimals = "...")
  
  # Experiment with the tab_options() function and arguments
  df_tst_psnu %>% 
    gt(groupname_col  = "...") %>% 
    fmt_number(columns = c("...", "..."), 
               decimals = "...") %>% 
    fmt_percent(columns = "...", 
                decimals = "...") %>% 
    tab_options(data_row.padding = "...",
                table.font.size = "...",
                row_group.padding = "...",
                row_group.font.weight = "...")
  
  

  