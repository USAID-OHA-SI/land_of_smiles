# Exercise: Day2_session07_joining_and_appending_exercise_1_joining_KEY


# SETUP -------------------------------------------------------------------

# Library
# install.packages("readxl")
library(readxl)
library(tidyverse)
library(gagglr)

# Load data
df_msd <- return_latest("Data", "PSNU_IM") %>%
  read_psd()


df_msd_tst <- df_msd %>% 
  filter(indicator == "HTS_TST", fiscal_year == 2060,
         standardizeddisaggregate == "Total Numerator") %>% 
  summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
            .by = c(mech_code, mech_name, indicator))


# Load in the partner data
df_lp <- read_excel("Dataout/Minoria_local_partners.xlsx") %>% #specify location of data ("Data" vs "Dataout")
  relocate(mech_name, .after = mech_code)


# EXERCISE ----------------------------------------------------------------

# Instructions
# Investigate the columns of each data set.
# What columns might you try merging on?


# How can we compare the columns in each data set?
names(df_msd_tst)  
names(df_lp)  

# What columns provide common overlap? 
intersect(names(df_msd_tst), names(df_lp)) #mech_code and mech_name 

# Try joining the two data sets together
left_join(df_msd_tst, df_lp) 
right_join(df_msd_tst, df_lp)


# Why is this not working? 
incompatible `mech_code` types (character vs numeric)

#What do you notice about the data types?


#devtools::install_github("reconhub/linelist")
#library(linelist)
#compare_data(df_msd_tst, df_lp) 

#// Comparison of variable classes /
# `mech_code` has changed from `character` to `numeric`


# LOAD LP CROSSWALK CORRECTED ---------------------------------------------

#install.packages("tidylog")
library(tidylog)

# Load in the partner data
df_lp_fixed <- read_excel("Dataout/Minoria_local_partners.xlsx", sheet = "lp_crosswalk_fixed") %>% 
  relocate(mech_name, .after = mech_code)

# We can see that both are character data types 
str(df_lp_fixed$mech_code)
str(df_msd_tst$mech_code)  

# Can we join now?
tidylog::left_join(df_lp_fixed, df_msd_tst)
tidylog::right_join(df_lp_fixed, df_msd_tst)

#joining tips 
setequal(df_lp$mech_code, df_msd_tst$mech_code) #check if equal in each dataset - FALSE
intersect(df_lp$mech_code, df_msd_tst$mech_code) #check how variables intersect - 0 similar values
setdiff(df_lp$mech_code, df_msd_tst$mech_code) #check how many variables differ - 36 different values 

setequal(df_lp_fixed$mech_code, df_msd_tst$mech_code) #check if equal in each dataset - FALSE
intersect(df_lp_fixed$mech_code, df_msd_tst$mech_code) #check how variables intersect - 18 similar values
setdiff(df_lp_fixed$mech_code, df_msd_tst$mech_code) #check how variables differ - 19 different values 
