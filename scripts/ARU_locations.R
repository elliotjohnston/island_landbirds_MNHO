########################################
######### ARU Locations Script #########
########################################
# 
# Date created: 10/27/2023
# Created by: Elliot Johnston
# 
# Purpose: generate random coordinates for ARU deployment 

# load libraries
library(sf)
library(tmap)
library(tidyverse)

# import island vectors. made polygons by hand in Google Earth as exported as a .kml file

islands <- st_read("data/raw/project_islands_2024.kml") %>% 
  
  # remove Z coordinate (so just XY polygons)
  
  st_zm() %>% 
  
  # transform CRS to projected UTM 19 N
  
  st_transform(crs = 32619) %>% 
  
  # set columns name to lower case and drop unnessary columns 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  select(-description)


# look at object details

islands 

# look at object on interactive map

tmap_mode("view")

tm_basemap(c('Esri.WorldImagery')) +
  islands %>% 
  tm_shape(name = 'Islands') +
  tm_polygons(alpha = 0.3)


# generate random points for ARU locations



tm_basemap(c('Esri.WorldImagery')) +
islands %>% 
  
  # set buffer (units in meters)
  
  st_buffer(dist = -25) %>%
  tm_shape(name = 'Islands') +
  tm_polygons()


# things to do/figure out:
#   1) generate random points within polygons using a reproducable seed 
#   2) minimum distance that random points can be to each other
#   3) how to deal with islands that became two polygons after buffer
#   4) is 25 meters an appropriate buffer? see Goodale MCHT report
#   5) do I decide how many ARU points should be on a given island or should I set up R to generate that?
  
  
