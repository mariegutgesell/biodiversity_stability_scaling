##Hydrobasin delineation - fish and benthic invertebrate 

##NOTE: delineation of hydrobasins and all spatial data processing is done on Senckenberg laptop. output files saved to repo, but all QGIS/GDAL/GRASS GIS/GNU parallel needed to use Hydrography90m r package is downloaded on Senckenberg PC


library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

#remotes::install_github("velofrog/readxlsb")
library(readxlsb)

##Hydrography and riverdist packages 
#remotes::install_github("glowabio/hydrographr") 
library(hydrographr)
library(riverdist)
library(sf)
library(mapview)


##Read in master file with dataset info 
master <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "overview")


##Macroinvertebrate site information - lat, long etc. 
bi_site_df <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB")

##Lets see if we just start with one country to see if can get process of identifying hydrobasins/connected sites working 
test_df <- bi_site_df %>%
  filter(Country == "Czech Republic")

##just testing out plotting of sites, are they potentially connected? 
sites_coord <- st_as_sf(test_df, coords = c("Longitude_X", "Latitude_Y"), crs = 4326)
site_map <- mapview(sites_coord, map.types = "Esri.WorldTopoMap", legend = TRUE,  alpha = 1, alpha.regions = 1, cex = 4)
site_map


##exploring hydrography code to see how it works
tile_id <- get_tile_id(data = test_df,
                       lon = "Longitude_X",  lat = "Latitude_Y")


##Fish site information - lat, long etc. 
fish_site_df <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_fish")
