#
# ARU Locations Script
# 
# Date created: 10/27/2023
# Date last modified: 4/2/2024
# Created by: Elliot Johnston
# 
# Purpose: generate random coordinates for ARU deployment 


# set up ------------------------------------------------------------------



# load libraries

library(sf)
library(tmap)
library(units)
library(spatstat)
library(tidyverse)

# import site and ARU polygons. made polygons by hand in Google Earth and exported as .kml files

polygons <-
  
  # create list of files to read in
  
  list(
    sites = st_read("data/raw/2024_site_polygons.kml"),
    aru = st_read("data/raw/2024_ARU_polygons.kml")) %>% 
  
  # map data cleaning steps
  
  purrr::map(
    ~ .x %>% 
      
      # remove Z coordinate (so just XY polygons)
      
      st_zm() %>% 
      
      # transform CRS to projected UTM 19 N
      
      st_transform(crs = 32619) %>% 
      
      # set columns name to lower case and drop unnessary columns 
      
      set_names(
        names(.) %>% 
          tolower()) %>% 
      select(-description))


# look at ARU object details

polygons$aru 


# look at object on interactive map

tmap_mode("view")

tm_basemap(c('Esri.WorldImagery')) +
  polygons$aru %>% 
  tm_shape(name = 'ARU Polygons') +
  tm_polygons(alpha = 0.3)



# generate random ARU points ----------------------------------------------

# can use shoreline buffer to also ensure that points in consecutive island polygons can't be too close to each other

# set seed

set.seed(217)

aru_points <-
  polygons$aru %>% 
  
  # set buffer (units in meters)
  
  st_buffer(dist = -40) %>%
  
  # generate random points 
  
  st_sample(size = c(1, 1),
            type = "random") %>% 
  
  # join attributes from polygons to points
  
  st_sf() %>% 
  st_join(polygons$aru)


# view points in tmap

tm_basemap(c('Esri.WorldImagery')) +
  aru_points %>% 
  tm_shape(name = 'ARU Points') +
  tm_dots(col = "white")



# distance between ARUs and shoreline -------------------------------------

# calculate shortest distance between aru points and the site boundary (i.e., shoreline)

# matrix of distances between points and sites

dist_to_edge <-
  st_geometry(polygons$sites) %>% 
  
  # change from polygon to linestring
  
  st_cast(to = 'LINESTRING') %>% 
  st_distance(y = aru_points)

# set row and column names

rownames(dist_to_edge) <- polygons$sites$name
colnames(dist_to_edge) <- aru_points$name

# extract minimum distances between points and site boundaries

dist_to_edge_min <-
  dist_to_edge %>% 
  
  # remove units object class
  
  drop_units() %>% 
  
  # conver to tibble and keep row names
  
  as_tibble(rownames = NA) %>% 
  rownames_to_column("island") %>% 
  
  # pivot from wide to long format
  
  pivot_longer(cols = 'Western Head 1':'Turtle 3',
               names_to = "aru", 
               values_to = "dist_to_edge") %>% 
 
  # extract minimum distances for each aru point
  
  group_by(aru) %>% 
  summarise(dist_to_edge_m = min(dist_to_edge))

# summary stats

mean(dist_to_edge_min$dist_to_edge_m)
sd(dist_to_edge_min$dist_to_edge_m)
range(dist_to_edge_min$dist_to_edge_m)

rm(dist_to_edge)

# nearest neighbor distance between ARUs ----------------------------------

# create distance matrix

aru_points_dist <-
  aru_points %>% 
  st_distance(y = aru_points)

# set row and column names

rownames(aru_points_dist) <- aru_points$name
colnames(aru_points_dist) <- aru_points$name

# extract nearest neighbor distances

aru_nearest_neighbor <-
  aru_points_dist %>% 
  
  # remove units object class
  
  drop_units() %>% 
  
  # conver to tibble and keep row names
  
  as_tibble(rownames = NA) %>% 
  rownames_to_column("aru_1") %>% 
  
  # pivot from wide to long format
  
  pivot_longer(cols = 'Western Head 1':'Turtle 3',
               names_to = "aru_2", 
               values_to = "distance") %>% 
  
  # filter out distances between same point and islands with only one ARU
  
  filter(distance > 0,
         ! aru_1 %in% c("Little Hardwood 1", 
                    "Double Shot 1",
                    "Double Head Shot 1")) %>% 
  
  # extract nearest neighbor distance
  
  group_by(aru_1) %>% 
  summarise(distance_m = min(distance))


# summary stats

mean(aru_nearest_neighbor$distance_m)
sd(aru_nearest_neighbor$distance_m)
range(aru_nearest_neighbor$distance_m)

rm(aru_points_dist)



# export points to kml file for google earth. export a separate file (WGS 1984) to load gpx onto GPS


# things to do/figure out:
#   1) minimum distance that random points can be to each other
#   2) is 40 meters an appropriate shoreline buffer? see Goodale MCHT report
  
# need to make sure that for all ARU locations, the distance of detection radius entirely falls on land. If an ARU had a detection range partly in the ocean, we wouldn't be monitoring the same total area as a unit in the middle of the forest. -> compute distance matrix between points and the nearest point of the contiguous shoreline polygon

  
  
