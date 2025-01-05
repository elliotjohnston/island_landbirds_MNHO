#
# ARU Locations Script
# 
# Date created: 1/4/2025
# Date last modified: 1/4/2025
# Created by: Elliot Johnston
# 
# Purpose: create BirdNET confidence scores thresholds for each species based on prediction validations


# set up ------------------------------------------------------------------

# load packages

library(tidyverse)


# get list of validation file names

validations_files <-
  list.files(
    'data/raw/2024_validation_results',
    full.names = TRUE)


# import and format

spp_validations <-
  validations_files %>% 
 
  # iteratively import files into a list and name 
  
   map(
    ~ read_delim(.x,
                 delim = "\t")) %>% 
  set_names(
    str_remove(list.files('data/raw/2024_validation_results'), 
               '.txt')) %>% 
  map(
    ~ .x %>% 
      select("file_name" = "Begin File",
             "valid" = "Valid") %>% 
      
      # extract confidence scores. then extract shortened files names to remove duplicate validations. Duplicates happened in some cases because 100 random files were selected for confidence scores 0.1 to 1.0 and 100 additional random files were selected for confidence scores 0.85 to 1.0.
      
      mutate(confidence_score = 
               str_sub(file_name, 1, 5),
             file_name_short = 
               str_sub(file_name, str_locate(file_name, "[a-zA-Z]")[, 1], nchar(file_name))) %>% 
      
      distinct(file_name_short,
               .keep_all = TRUE) %>% 
      
      select(!file_name_short)
  )


# next step: remove files that didn't have any correct predictions
# were there any files with all correct predictions?



