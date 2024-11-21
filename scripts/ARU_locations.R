#
# ARU Locations Script
# 
# Date created: 10/27/2023
# Date last modified: 4/5/2024
# Created by: Elliot Johnston
# 
# Purpose: generate random coordinates for ARU deployment 


# set up ------------------------------------------------------------------



# load libraries

library(sf)
library(tmap)
library(units)
library(spsurvey)
library(spatstat)
library(tidyverse)

# import site polygons. made polygons by hand in Google Earth and exported as .kml files

sites <-
  
  # read in file
  
  st_read("data/raw/2024_site_polygons.kml") %>% 
  
  # remove Z coordinate (so just XY polygons)
  
  st_zm() %>% 
  
  # transform CRS to projected UTM 19 N
  
  st_transform(crs = 32619) %>% 
  
  # set columns name to lower case and drop unnecessary columns 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  select(-description) %>% 
  
  # calculate site area and sort
  
  mutate(area = st_area(.) %>% 
           units::set_units('ha')) %>% 
  arrange(desc(area)) %>% 
  
  # classify sites into small (1 ARU), medium (2 ARU), large (3 ARU) sites
  
  mutate(size_class = 
           case_when(
             st_area(.) < set_units(10, ha) ~ 'small',
             st_area(.) < set_units(80, ha) ~ 'medium',
             .default = 'large') %>% 
           as.factor(),
         name = name %>% 
           as.factor())


# look at object details

sites


# look at object on interactive map

tmap_mode("view")

tm_basemap(c('Esri.WorldImagery')) +
  sites %>% 
  tm_shape(name = 'Sites') +
  tm_polygons(alpha = 0.5)


# import stratified ARU blocks (1, 2, and 3 blocks for small, medium, and large sites, respectively)

aru_blocks <-
  st_read("data/raw/2024_aru_polygons.kml") %>% 
  st_zm() %>% 
  st_transform(crs = 32619) %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  select(-description) %>% 
  mutate(area = st_area(.) %>% 
           units::set_units('ha')) %>% 
  arrange(desc(area))



# generate random ARU points ----------------------------------------------

# buffer aru blocks from shoreline and each other. for larger blocks (> 10 ha) buffer is 100 m and for smaller islands (< 10 ha) buffer is 40 m

aru_blocks_buffered <-
  aru_blocks %>%
  st_buffer(dist =
              case_when(
                st_area(.) > set_units(10, ha) ~ -100,
                .default = -40))


# generate random points (ARU locations)

# see: https://cran.r-project.org/web/packages/spsurvey/vignettes/sampling.html 

# set seed

set.seed(9382)


aru_points <-
  aru_blocks_buffered %>% 
  
  # generate random points. specify number of points to be generated within each polygon and minimum distance from each other 
  
  grts(n_base = c('Great Wass 1' = 1, 'Great Wass 2' = 1, 'Great Wass 3' = 1,
                  'Marshall 1' = 1, 'Marshall 2' = 1, 'Marshall 3' = 1,
                  'Penobscot 1' = 1, 'Penobscot 2' = 1, 'Penobscot 3' = 1,
                  'Western Head 1' = 1, 'Western Head 2' = 1, 'Western Head 3' = 1,
                  'Flint 1' = 1, 'Flint 2' = 1,
                  'Turtle 1' = 1, 'Turtle 2' = 1,
                  'Pinkham 1' = 1, 'Pinkham 2' = 1,         
                  'Lakeman 1' = 1, 'Lakeman 2' = 1,
                  'Mark 1' = 1, 'Mark 2' = 1,
                  'Knight 1' = 1,
                  'Big White 1' = 1,
                  'Big Garden 1' = 1,
                  'Inner Sand 1' = 1,
                  'Anguilla 1' = 1,        
                  'Ned 1' = 1,
                  'Double Shot 1' = 1,
                  'Little Hardwood 1' = 1,
                  'Double Head Shot 1' = 1),
       stratum_var = "name",
       mindis = 250,
       maxtry = 100)
  

# view points and aru blocks in tmap

tm_basemap(c('Esri.WorldImagery')) +
  aru_blocks_buffered %>% 
  tm_shape(name = 'ARU Blocks (Buffered)') +
  tm_polygons(alpha = 0.5) +

  aru_points$sites_base %>% 
  tm_shape(name = 'ARU Points') +
  tm_dots(col = "white")



# distance between ARUs and shoreline -------------------------------------

# calculate shortest distance between aru points and the site boundary (i.e., shoreline)

# matrix of distances between points and sites

dist_to_edge <-
  st_geometry(sites) %>% 
  
  # change from polygon to linestring
  
  st_cast(to = 'LINESTRING') %>% 
  st_distance(y = aru_points$sites_base)

# set row and column names

rownames(dist_to_edge) <- sites$name

colnames(dist_to_edge) <-
  c("Great Wass 1", "Great Wass 2", "Great Wass 3", 
    "Marshall 1", "Marshall 2", "Marshall 3", 
    "Penobscot 1", "Penobscot 2", "Penobscot 3", 
    "Western Head 1", "Western Head 2", "Western Head 3",
    "Flint 1", "Flint 2", 
    "Turtle 1", "Turtle 2",
    "Pinkham 1", "Pinkham 2",
    "Lakeman 1", "Lakeman 2",
    "Mark 1", "Mark 2", 
    "Knight 1", 
    "Big White 1", 
    "Big Garden 1", 
    "Inner Sand 1",
    "Anguilla 1", 
    "Ned 1", 
    "Double Shot 1", 
    "Little Hardwood 1", 
    "Double Head Shot 1")

# extract minimum distances between points and site boundaries

dist_to_edge_min <-
  dist_to_edge %>% 
  
  # remove units object class
  
  drop_units() %>% 
  
  # conver to tibble and keep row names
  
  as_tibble(rownames = NA) %>% 
  rownames_to_column("island") %>% 
  
  # pivot from wide to long format
  
  pivot_longer(cols = 'Great Wass 1':'Double Head Shot 1',
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
  aru_points$sites_base %>% 
  st_distance(y = aru_points$sites_base)

# set row and column names

rownames(aru_points_dist) <-
  c("Great Wass 1", "Great Wass 2", "Great Wass 3", 
    "Marshall 1", "Marshall 2", "Marshall 3", 
    "Penobscot 1", "Penobscot 2", "Penobscot 3", 
    "Western Head 1", "Western Head 2", "Western Head 3",
    "Flint 1", "Flint 2", 
    "Turtle 1", "Turtle 2",
    "Pinkham 1", "Pinkham 2",
    "Lakeman 1", "Lakeman 2",
    "Mark 1", "Mark 2", 
    "Knight 1", 
    "Big White 1", 
    "Big Garden 1", 
    "Inner Sand 1",
    "Anguilla 1", 
    "Ned 1", 
    "Double Shot 1", 
    "Little Hardwood 1", 
    "Double Head Shot 1")

colnames(aru_points_dist) <-
  c("Great Wass 1", "Great Wass 2", "Great Wass 3", 
    "Marshall 1", "Marshall 2", "Marshall 3", 
    "Penobscot 1", "Penobscot 2", "Penobscot 3", 
    "Western Head 1", "Western Head 2", "Western Head 3",
    "Flint 1", "Flint 2", 
    "Turtle 1", "Turtle 2",
    "Pinkham 1", "Pinkham 2",
    "Lakeman 1", "Lakeman 2",
    "Mark 1", "Mark 2", 
    "Knight 1", 
    "Big White 1", 
    "Big Garden 1", 
    "Inner Sand 1",
    "Anguilla 1", 
    "Ned 1", 
    "Double Shot 1", 
    "Little Hardwood 1", 
    "Double Head Shot 1")

# extract nearest neighbor distances

aru_nearest_neighbor <-
  aru_points_dist %>% 
  
  # remove units object class
  
  drop_units() %>% 
  
  # convert to tibble and keep row names
  
  as_tibble(rownames = NA) %>% 
  rownames_to_column("aru_1") %>% 
  
  # pivot from wide to long format
  
  pivot_longer(cols = 'Great Wass 1':'Double Head Shot 1',
               names_to = "aru_2", 
               values_to = "distance") %>% 
  
  # filter out distances between same point and islands with only one ARU
  
  filter(distance > 0,
         ! aru_1 %in% c("Knight 1", 
                        "Big White 1", 
                        "Big Garden 1", 
                        "Inner Sand 1",
                        "Anguilla 1", 
                        "Ned 1", 
                        "Double Shot 1", 
                        "Little Hardwood 1", 
                        "Double Head Shot 1")) %>% 
  
  # extract nearest neighbor distance
  
  group_by(aru_1) %>% 
  summarise(distance_m = min(distance))


# summary stats

mean(aru_nearest_neighbor$distance_m)
sd(aru_nearest_neighbor$distance_m)
range(aru_nearest_neighbor$distance_m)

# all ARUs at least 250 m apart

rm(aru_points_dist)


# name aru points 

aru_points$sites_base$siteID <-
  c("Great Wass 1", "Great Wass 2", "Great Wass 3", 
    "Marshall 1", "Marshall 2", "Marshall 3", 
    "Penobscot 1", "Penobscot 2", "Penobscot 3", 
    "Western Head 1", "Western Head 2", "Western Head 3",
    "Flint 1", "Flint 2", 
    "Turtle 1", "Turtle 2",
    "Pinkham 1", "Pinkham 2",
    "Lakeman 1", "Lakeman 2",
    "Mark 1", "Mark 2", 
    "Knight 1", 
    "Big White 1", 
    "Big Garden 1", 
    "Inner Sand 1",
    "Anguilla 1", 
    "Ned 1", 
    "Double Shot 1", 
    "Little Hardwood 1", 
    "Double Head Shot 1")
  

# export aru points -------------------------------------------------------


# export points to kml file for google earth

aru_points$sites_base %>% 
  select(Name = siteID) %>% 
  st_transform(4326) %>% 
  st_write(dsn = "outputs/aru_points.kml", 
           driver = "kml")

# In Google Earth moved:
# Marshall 1 235 m SE so at least 250 m from old air strip
# Great Wass 2 400 m SE so not in what looks to be a bog/open wetland 

# when ground truthing points, will need to make sure that Western Head 1 and Great Wass 1 are at least 250 m from trails

  
  
