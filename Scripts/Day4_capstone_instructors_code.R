# PROJECT: GDSW Training 
# PURPOSE: Munge and Analysis of Minoria MSD for cascade results
# AUTHOR:  Tim Esssam | SI
# REF ID:  f2195992
# LICENSE: MIT
# DATE:   2023-09-01
# NOTES:  Generated for capstone exercise

# LOCALS & SETUP ============================================================================

  # Libraries needed
  library(tidyverse)
  library(gagglr)
  library(scales)
  library(cascade)


# Get paths for feeding into the read_psd function
  msd_path <- return_latest("Data", "PSNU_IM")
  get_metadata(msd_path)
  
  subnat_path <- return_latest("Data", "SUBNAT")


# Load the data
  df_msd <- read_psd(msd_path)
  df_subnat <- read_psd(subnat_path)
  

# RUN  CASCADES -----------------------------------------------------------

  # List all the functions in the package so you can see options
  ls('package:cascade')
  
  # Check the cascade data frame to ensure MSD is valid
  return_cascade(df_msd, 1)

  # Create 13 cascade plots saving them in the Images folder
  batch_cascade_plot(df_msd, imgpath = "Images/")

   