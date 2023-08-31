# Exercise: Day3_session10_small_multiples_exercise_1_facets


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)
library(scales)
library(selfdestructin5)
library(cascade)

# Data - Make sure to downlaod `MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-60_20230616_v2_1.zip` file and move it to `Data` folder

# Get paths
  msd_path <- return_latest("Data", "PSNU_IM")
  get_metadata(msd_path)
  
  subnat_path <- return_latest("Data", "SUBNAT")


# Load the data
  df_msd <- read_psd(msd_path)
  
  df_subnat <- read_psd(subnat_path)
  

# RUN  CASCADES -----------------------------------------------------------
  ls('package:cascade')
  return_cascade(df_msd, 1)

  batch_cascade_plot(df_msd, imgpath = "Images/")

   