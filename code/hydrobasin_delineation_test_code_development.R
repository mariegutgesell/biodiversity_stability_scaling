##Hydrobasin delineation - benthic invertebrates -- all sites



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
library(dplyr)
library(terra)
library(tools)
library(stringr)
library(leaflet)
library(leafem)
library(tidyr)


##set working directory 
getwd()
#wd <- setwd("/Users/mariegutgesell/LocalRepos/biodiversity_stability_scaling")

##Read in master file with dataset info 
master <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "overview")


##Macroinvertebrate site information - lat, long etc. 
bi_site_df <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB")

test <- bi_site_df %>%
  select(origin) %>%
  unique()

ellen_data <- bi_site_df %>%
  filter(grepl("first", origin))

##Lets see if we just start with one country to see if can get process of identifying hydrobasins/connected sites working 
test_df <- bi_site_df %>%
  filter(Country == "Austria")

##just testing out plotting of sites, are they potentially connected? 
sites_coord <- st_as_sf(test_df, coords = c("Longitude_X", "Latitude_Y"), crs = 4326)
site_map <- mapview(sites_coord, map.types = "Esri.WorldTopoMap", legend = TRUE,  alpha = 1, alpha.regions = 1, cex = 4)
site_map


##exploring hydrography code to see how it works

##1) Download relevant tiles (only need to do this once, don't do everytime running code ) ---------------
tile_id <- get_tile_id(data = test_df,
                       lon = "Longitude_X",  lat = "Latitude_Y")

reg_unit_df <- get_regional_unit_id(data = test_df,
                                    lon = "Longitude_X",  lat = "Latitude_Y")

##select only tiles that are relevant, for example all of czech republic is in 1 tile (see https://hydrography.org/hydrography90m/hydrography90m_layers#tile-map) so select that one 
##this selects all tiles in Europe
tile_id <- c("h16v02","h16v04","h18v00","h18v02","h18v04","h20v00","h20v02","h20v04")


##define the raster layers that you want to download for that tile
vars_tif <- c("sub_catchment", "segment", "accumulation","basin","direction")

vars_gpkg <- c("basin", "order_vect_segment")

options(timeout = 1000)
#setwd("/Volumes/LaCie/SGN_Repo_LargeFiles/hydrography_90m")
#getwd()
#file.exists("/Volumes/LaCie/SGN_Repo_LargeFiles/hydrography_90m")
write.csv(data.frame(x = 1), "/Volumes/LaCie/SGN_Repo_LargeFiles/hydrography_90m/test_write.csv")

##download the .tif files of desired variables
download_tiles(variable = vars_tif, tile_id = tile_id, file_format = "tif", download_dir = "/Volumes/LaCie/SGN_Repo_LargeFiles/hydrography_90m")

##download .gpkg tiles of desired variables 
download_tiles(variable = vars_gpkg, tile_id = tile_id, file_format = "gpkg", download_dir = "data")

##download the raster mask of the regional unit 
download_tiles(variable = "regional_unit", file_format = "tif", reg_unit_id = reg_unit_df, download_dir = "data")

##NOTE: can also download from the hydrography page directly as well ... 


##2) Extract basin ids and crop rasters ---------------------

##NOT SURE IF NEED TO CROP BUT KEEEPING CODE IN HERE IN CASE NEED IT AT A FUTURE POINT
##Now need to crop the layers to the extend of study area extended by 500km so that basins are are not split in half 

##https://github.com/glowabio/hydrographr/blob/0962858cabb6afa7baea92309f793edb850cc849/vignettes/case_study_germany.Rmd

##First get the basin IDs 
##see if i can extract basin id for each site
test_ids <- extract_ids(data = test_df, id = "Unique.ID", lon = "Longitude_X", lat = "Latitude_Y", basin_layer = paste0(wd, "/data/r.watershed/basin_tiles20d/basin_h18v02.tif"), subc_layer = paste0(wd, "/data/r.watershed/sub_catchment_tiles20d/sub_catchment_h18v02.tif"))
test_basin_id <- test_ids %>%
  select(basin_id) %>%
  unique()
##define and create a directory for the clipped/merged files
study_area_dir <-  paste0(wd, "/data/study_area")
if(!dir.exists(study_area_dir)) dir.create(study_area_dir)

##start with the basin files
##get the full paths of the basin GeoPackage tiles
(basin_dir <- list.files(wd, pattern = "basin_h[v0-8]+.gpkg$", full.names = TRUE, recursive = TRUE))

##filter the basin IDs from the Geopackages of the basin tiles, then save the filtered tiles 
#Filter the basin IDs from the GeoPackages of the basin tiles, then save the filtered tiles
for(itile in basin_dir) {
  filtered_tile<-read_geopackage(itile, import_as = "sf", subc_id = test_basin_id, name = "ID")
  write_sf(filtered_tile, paste(study_area_dir, paste0(str_remove(basename(itile), ".gpkg"), "_tmp.gpkg"), sep="/"))
}

#Merge the filtered GeoPackage tiles
merge_tiles(tile_dir = study_area_dir, tile_names = list.files(study_area_dir, full.names = FALSE, pattern = "basin_.+_tmp.gpkg$"), out_dir = study_area_dir, file_name = "test_basins.gpkg", name = "ID", read = FALSE)


##--Now for the other files
#Get the full paths of all raster tiles
(raster_dir<-list.files(paste0(getwd(), "/data/r.watershed"), pattern = ".tif", full.names = TRUE, recursive = TRUE))
#double check the names to make sure no other file types have crept in, which can happen if you open them in qGIS
#because it autosaves other files to the same directory

#Crop all tiles to the extent of the drainage basins, saved as temporary files because they will be deleted later
for(itile in raster_dir) {
  crop_to_extent(raster_layer = itile, vector_layer = paste0(study_area_dir, "/test_basins.gpkg"), out_dir = study_area_dir, file_name =  paste0(str_remove(basename(itile), ".tif"),"_tmp.tif"))
}  

#Merge the cropped raster layers of the different tiles
merge_tiles(tile_dir = study_area_dir, tile_names = list.files(study_area_dir, full.names = FALSE, pattern = "basin_.+_tmp.tif$"), out_dir = study_area_dir, file_name = "be_merged_basins.tif")
merge_tiles(tile_dir = study_area_dir, tile_names = list.files(study_area_dir, full.names = FALSE, pattern = "segment_.+_tmp.tif$"), out_dir = study_area_dir, file_name = "be_merged_segments.tif")
#merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "accumulation_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_accumulation.tif")
merge_tiles(tile_dir = study_area_dir, tile_names = list.files(study_area_dir, full.names = FALSE, pattern = "sub_.+_tmp.tif$"), out_dir = study_area_dir, file_name = "be_merged_subcatchments.tif")
#merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "direction_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_direction.tif")

##okay so this worked, is only for the basin i think

###---Now the "order_vect_segment" files, which can be a bit slow because they're the big ones
#these files also hold a lot of important river data, such as th Strahler orders

#The order_vect_segment uses the subcatchment IDs, so first load the merged sub-catchment raster layer
(sub_rast<-paste0(study_area_dir, "/be_merged_subcatchments.tif"))
sub<-rast(x=sub_rast)

#Get all sub-catchment IDs of the drainage basin
(sub_ids<-terra::unique(sub))

# Get the full paths of the stream order segment GeoPackage tiles
(order_dir<-list.files(getwd(), pattern = "order_vect_segment+_h[v0-8]+.gpkg$", full.names = TRUE, recursive = TRUE))

#Filter the sub-catchment IDs from the GeoPackage of the order_vector_segment tiles (sub-catchment ID = stream ID)
for(itile in order_dir) {
  filtered_tile <- read_geopackage(itile,
                                   import_as = "sf",
                                   subc_id = sub_ids$be_merged_subcatchments,
                                   name = "stream")
  
  write_sf(filtered_tile, paste(study_area_dir,
                                paste0(str_remove(basename(itile), ".gpkg"),
                                       "_tmp.gpkg"), sep="/"))
}

#Merge filtered GeoPackage tiles
#This process can take a few minutes
merge_tiles(tile_dir = study_area_dir,
            tile_names = list.files(study_area_dir, full.names = FALSE,
                                    pattern = "order_.+_tmp.gpkg$"),
            out_dir = study_area_dir,
            file_name = "be_merged_order_vect_segments.gpkg",
            name = "stream",
            read = FALSE)

#Import the ordered segments geopackage for later use
order_vect_seg<-read_geopackage(paste0(study_area_dir, "/be_merged_order_vect_segments.gpkg"), import_as = "sf", name = "stream")

#Delete all temporary files
(tmp_files<-list.files(study_area_dir, pattern = "_tmp.", full.names = TRUE, recursive = TRUE))
file.remove(tmp_files)
#you should now be left with just 7 files, which is each input layer cropped to the basins the sites are in
#can load into qGIS to double check, lets try the segment layer


##hm it does not look like that cropping worked when looking in QGIS... but it is alot smaller in file size.. so maybe? lets see 


##3) Load layers (start here once have them downloaded so don't need to repeat those steps/downloads which take a long time) ------------
##what does uncropped version look like? 
study_area_dir <-  paste0(wd, "/data/study_area")
#if(!dir.exists(study_area_dir)) dir.create(study_area_dir)
basin_layer <- rast(paste0(study_area_dir, "/be_merged_basins.tif"))
basin_ids <- sort(unique(values(basin_layer, na.rm = TRUE)))
class(basin_layer)
plot(basin_layer)

#mapview(basin_layer)

sub_c_layer <- rast(paste0(study_area_dir, "/be_merged_subcatchments.tif"))
plot(sub_c_layer)

stream_network <- rast(paste0(study_area_dir, "/be_merged_segments.tif"))
plot(stream_network)
##get basin IDs
basin_ids <- terra::unique(basin_layer)


##see if i can extract basin id for each site
test_ids <- extract_ids(data = test_df, id = "Unique.ID", lon = "Longitude_X", lat = "Latitude_Y", basin_layer = paste0(study_area_dir, "/be_merged_basins.tif"), subc_layer = paste0(study_area_dir, "/be_merged_subcatchments.tif"))

#no fucking way i think this worked
unique_basin_test <- test_ids %>%
  select(basin_id) %>%
  unique()

unique_subcatchment_test <- test_ids %>%
  select(subcatchment_id) %>%
  unique()

##see if basin ids match what ellen has 
ellen_df <- read.csv("data/Welti_etal_2024/TREAM_siteLevel.csv") %>%
  select(site_id:River, MacrobasinID, MicrobasinID)

basin_ids <- basin_ids %>%
  rename(MacrobasinID = "basin_h18v02")

basin_id_overlap <- left_join( basin_ids, ellen_df, by = "MacrobasinID")

test <- basin_id_overlap %>%
  filter(!is.na(site_id))

##so most of the basin IDs are not the same ... should they be? this feels concerning to me ... why are they not the same? most basin IDs in ellen df can not be found in the hydrography basin id (only 59 of 48905 basins in this one tile)
##the subcatchment IDs also seem different ... 

##3) Get distance between sites ---------------
##Define full path lengths to the relevant layers
basin_rast <- paste0(study_area_dir, "/be_merged_basins.tif")
subc_rast <- paste0(study_area_dir, "/be_merged_subcatchments.tif")
stream_vect <- paste0(study_area_dir, "/be_merged_order_vect_segments.gpkg")

stream_rast <- paste0(study_area_dir, "/be_merged_segments.tif")


##First, automatically extract the basin and sub-catchment IDs and then snap the data points to the stream segment 
#test_df <- test_df %>%
#  filter(Site_ID_original != "1") 
test_df$Latitude_Y <- as.numeric(test_df$Latitude_Y)
test_df$Longitude_X <- as.numeric(test_df$Longitude_X)

##if having issues with GRASS use following code:
Sys.setenv(PATH = paste("/Applications/GRASS-8.3.app/Contents/Resources/bin",
                        Sys.getenv("PATH"), sep = ":"))
system2("grass", "--version")



##trying with extracting ids first
test_ids <- extract_ids(data = test_df, id = "Unique.ID", lon = "Longitude_X", lat = "Latitude_Y", basin_layer =basin_rast, subc_layer = subc_rast)
str(test_ids)
test_ids$shortID <- str_extract(test_ids$Unique.ID, "MZB_.*")
test_ids$id_num <- as.numeric(seq_len(nrow(test_ids)))
str(test_ids)



snapped_coordinates <- snap_to_subc_segment(data = test_ids, lon = "Longitude_X", lat = "Latitude_Y", id = "id_num",
                                            basin_id = "basin_id", subc_id = "subcatchment_id", basin_layer = basin_rast, subc_layer = subc_rast, stream_layer = stream_vect, n_cores = 1)

##THIS WORRRRKKEEEDDDD HELL YEA

snapped_coordinates_net <- snap_to_network(data = test_ids, lon = "Longitude_X", lat = "Latitude_Y", id = "id_num", 
                                           stream_layer = stream_rast, method = "distance")
##this is did not work ... i wonder if trying it with the flow as well may help, can come back and try that. but i think really only need to snap one way, - but need to investigate the sites 


##so ID has to have less than 10 characters..


##troubleshooting to see if i can get this to work:
library(tidyterra)
library(stars)

pts <- st_as_sf(test_df, coords = c("Longitude_X", "Latitude_Y"), crs = 4326)
pts_sf_proj <- st_transform(pts, st_crs(crs(stream_r)))
##checking coordinate systems 
crs(pts)
basin_r <- rast(basin_rast)
crs(basin_r)
subc_r <- rast(subc_rast)
crs(subc_r)
stream_r <- rast(stream_rast)
crs(stream_r)
#streams <- st_read(stream_vect, quiet = TRUE)
#st_crs(streams)


##seeing if i can plot the site points and the stream network
pts <- st_as_sf(test_df, coords = c("Longitude_X", "Latitude_Y"), crs = 4326)
stream_r <- rast(stream_rast)
pts_sf_proj <- st_transform(pts, st_crs(crs(stream_r)))

##crop stream network raster -- by creating a bounding box

# 3. Create a bounding box around the points + add a small buffer (e.g., 10 km)
bbox_pts <- st_bbox(pts_sf_proj) |> st_as_sfc()
bbox_buffered <- st_buffer(bbox_pts, dist = 10000)  # 10,000 m = 10 km buffer; adjust as needed

# 4. Crop and mask the raster to that area
stream_crop <- crop(stream_r, vect(bbox_buffered))




plot(stream_crop)
points(vect(pts_sf_proj), pch = 50, col = "red")



mapview(stream_crop, layer.name = "Stream Network") +
  mapview(pts_sf_proj, color = "red", cex = 5, layer.name = "Sites")

#cropping and then saving tif 
study_area_dir <-  paste0(wd, "/data/study_area")
if(!dir.exists(study_area_dir)) dir.create(study_area_dir)
##save cropped tif
stream_crop_path <- file.path(study_area_dir, "stream_crop_study_area.tif")

# Save raster as GeoTIFF
terra::writeRaster(stream_crop, stream_crop_path, overwrite = TRUE)


stream_crop <- paste0(wd, "/data/study_area/stream_crop_study_area.tif")
sr <- rast(stream_crop)
crs(sr)
ext(sr)

sr <- terra::rast(stream_crop)
normalizePath(stream_crop)
terra::sources(sr)
##lets try snapping to network again .. 
##need to have numeric ID that is less than 10 numbers
test_ids$id_num <- as.numeric(seq_len(nrow(test_ids)))
str(test_ids)
test_ids$id_num <- as.numer

snapped_coordinates <- snap_to_network(data = test_ids, lon = "Longitude_X", lat = "Latitude_Y", id = "id_num", 
                                       stream_layer = stream_crop, method = "distance")



##snap_to_subc_segment using grass v.net function, which is what Welti et al. used, so do this 
##this is not working atm 

head(snapped_coordinates)




##get the network distance between all pairs of points
distance_table <- get_distance(data = snapped_coordinates, lon = "lon_snap", lat = "lat_snap", id = "id_num", 
                               stream_layer = stream_vect, distance = "euclidean")

##okay network distance is still not working
##but the euclidean distance is.. 

###4) Plot out points and basins 
##how can i just get a map of the basins .. 
test_site_coord <- st_as_sf(test_df, coords = c("Longitude_X", "Latitude_Y"), crs = 4326)
test_site_coord <- st_transform(test_site_coord, crs(basin_layer))

site_basins <- terra::extract(basin_layer, vect(test_site_coord))
names(site_basins)[2] <- "basin_id"
test_site_coord$basin_id <- site_basins$basin_id


##
keep_ids <- sort(na.omit(unique(test_site_coord$basin_id)))
keep_ids

##mask the raster to only the 3 basins interested in
r_keep <- classify(basin_layer, rcl = cbind(keep_ids, keep_ids), others = NA)

##make quick static plot:
# Pick 3 distinct colors
cols3 <- c("#1f77b4", "#2ca02c", "#d62728")  # swap to your palette if you like

# Order colors to match keep_ids order
plot(r_keep, col = cols3[seq_along(keep_ids)], axes = FALSE, legend = FALSE, box = FALSE)
points(st_coordinates(test_site_coord), pch = 19, cex = 0.8)

# dissolve merges pixels per ID into a single multipart polygon per basin
b_poly <- as.polygons(r_keep, dissolve = TRUE, values = TRUE, trunc = TRUE)
b_poly <- st_as_sf(b_poly)  # to sf for easy plotting

# Plot with sf + base
plot(st_geometry(b_poly), col = cols3[match(b_poly$lyr.1, keep_ids)], border = "grey20", lwd = 0.7)
points(st_geometry(test_site_coord), pch = 21, bg = "black", cex = 0.7)
legend("topright", fill = cols3, legend = paste("Basin", keep_ids), bty = "n")

##make interactive map
mapview(b_poly,zcol = "basin_h18v02", col.regions = cols3, layer.name = "Basins") +
  mapview(test_site_coord, color = "black", cex = 3, layer.name = "Sites")

##Other information to extract:
##Area of basins
##distance between sites
##

##questions to sort out:
##why are basin ID in mine different from what macro and microbasin ID is in Welti data? 

##Fish site information - lat, long etc. 
fish_site_df <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_fish")
