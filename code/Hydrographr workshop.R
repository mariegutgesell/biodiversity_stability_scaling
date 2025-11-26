#First save this code somewhere on your C drive in a folder with no spaces (e.g., C:\Desktop)
#The Python code hydrographr runs has problems accessing file addresses with spaces in the name
#also make sure any files/folders you create have no spaces in their names

###---Load libraries (I honestly cant remember if all of them get used)
library(hydrographr)
library(data.table)
library(dplyr)
library(terra)
library(sf)
library(tools)
library(stringr)
library(ranger)
library(leaflet)
library(leafem)

###---Download the necessary hydrographr tiles from:
#https://hydrography.org/hydrography90m/hydrography90m_layers/

tile_id<-c("h16v02","h16v04","h18v00","h18v02","h18v04","h20v00","h20v02","h20v04")
#these tiles will typically cover all catchments in Europe
tile_id<-c("h18v02","h18v04")
#but we only need these for today

vars_tif <- c("sub_catchment", "segment", "accumulation","basin","direction")
vars_gpkg <- c("basin", "order_vect_segment")
#these are the five input layers you'll need for the processes we'll run

#set your working directory, then make a destination folder to store the data
save.dir<-paste0(getwd(),"/data") #should have no spaces in the names

#download the tifs and any geopackages for the layers you've chosen
download_tiles(variable = vars_tif, tile_id = tile_id, file_format = "tif", download_dir = save.dir)
download_tiles(variable = vars_gpkg, tile_id = tile_id, file_format = "gpkg", download_dir = save.dir)


########################################################################################################
#
#                                         STEP 1: CLIPPING/MERGING
#
########################################################################################################

#the next set of steps aims to reduce the areas of the above files to only the basins where your points are located
#this makes the file sizes much smaller and therefore easier/faster to work with
#they do this by getting the basin IDs where the points are, then clipping the different files to these basins,
#then merging the different clipped files together

#we'll use some example site locations in Belgium
#first we need to get the basin IDs, for which I use the point sampling plugin in qGIS, so we'll load the provided site locations there ("BE )
be.basin.points<-read.csv(file.choose(), header=TRUE)
be.basins<-unique(c(be.basin.points[,6]))
#note the basin IDs can be in multiple columns if the points cover multiple basin tiles
#you'll need to combine them if so, for example: unique(c(be.basin.points[,c(6,7)]))
#this example combines the basin IDs from both columns 6 and 7

#Define a directory to store the clipped/merged files
be_dir<- paste0(getwd(), "/basins")
#create the directory if it does not exist
if(!dir.exists(be_dir)) dir.create(be_dir)

##--Start with the basin files
#Get the full paths of the basin GeoPackage tiles
(basin_dir<-list.files(save.dir, pattern = "basin_h[v0-8]+.gpkg$", full.names = TRUE, recursive = TRUE))

#Filter the basin IDs from the GeoPackages of the basin tiles, then save the filtered tiles
for(itile in basin_dir) {
  filtered_tile<-read_geopackage(itile, import_as = "sf", subc_id = be.basins, name = "ID")
  write_sf(filtered_tile, paste(be_dir, paste0(str_remove(basename(itile), ".gpkg"), "_tmp.gpkg"), sep="/"))
}

#Merge the filtered GeoPackage tiles
merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "basin_.+_tmp.gpkg$"), out_dir = be_dir, file_name = "be_basins.gpkg", name = "ID", read = FALSE)

##--Now for the other files
#Get the full paths of all raster tiles
(raster_dir<-list.files(paste0(getwd(), "/data/r.watershed"), pattern = ".tif", full.names = TRUE, recursive = TRUE))
#double check the names to make sure no other file types have crept in, which can happen if you open them in qGIS
#because it autosaves other files to the same directory

#Crop all tiles to the extent of the drainage basins, saved as temporary files because they will be deleted later
for(itile in raster_dir) {
  crop_to_extent(raster_layer = itile, vector_layer = paste0(be_dir, "/be_basins.gpkg"), out_dir = be_dir, file_name =  paste0(str_remove(basename(itile), ".tif"),"_tmp.tif"))
}  

#Merge the cropped raster layers of the different tiles
merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "basin_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_basins.tif")
merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "segment_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_segments.tif")
merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "accumulation_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_accumulation.tif")
merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "sub_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_subcatchments.tif")
merge_tiles(tile_dir = be_dir, tile_names = list.files(be_dir, full.names = FALSE, pattern = "direction_.+_tmp.tif$"), out_dir = be_dir, file_name = "be_merged_direction.tif")

###---Now the "order_vect_segment" files, which can be a bit slow because they're the big ones
#these files also hold a lot of important river data, such as th Strahler orders

#The order_vect_segment uses the subcatchment IDs, so first load the merged sub-catchment raster layer
(sub_rast<-paste0(be_dir, "/be_merged_subcatchments.tif"))
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
  
  write_sf(filtered_tile, paste(be_dir,
                                paste0(str_remove(basename(itile), ".gpkg"),
                                       "_tmp.gpkg"), sep="/"))
}

#Merge filtered GeoPackage tiles
#This process can take a few minutes
merge_tiles(tile_dir = be_dir,
            tile_names = list.files(be_dir, full.names = FALSE,
                                    pattern = "order_.+_tmp.gpkg$"),
            out_dir = be_dir,
            file_name = "be_merged_order_vect_segments.gpkg",
            name = "stream",
            read = FALSE)

#Import the ordered segments geopackage for later use
order_vect_seg<-read_geopackage(paste0(be_dir, "/be_merged_order_vect_segments.gpkg"), import_as = "sf", name = "stream")

#Delete all temporary files
(tmp_files<-list.files(be_dir, pattern = "_tmp.", full.names = TRUE, recursive = TRUE))
file.remove(tmp_files)
#you should now be left with just 7 files, which is each input layer cropped to the basins the sites are in
#can load into qGIS to double check, lets try the segment layer


########################################################################################################
#
#                                         STEP 2: SNAPPING
#
########################################################################################################

#sites must be located on the river network to define the upstream area, this code snaps each site to the closest raster square

#Define the path to the segment layer
(stream_rast<-paste0(be_dir, "/be_merged_segments.tif"))
#Define the path to the flow accumulation layer
(flow_rast<-paste0(be_dir, "/be_merged_accumulation.tif"))

#Load the coordinates for each site
point_locations_tmp<-be.basin.points[,c(1,4,5)]
#set the column names
colnames(point_locations_tmp)<-c("site_id","longitude","latitude")

#Snap the points, I use the default settings but you can mess with the values if it becomes important
point_locations_snapped_tmp<-snap_to_network(data = point_locations_tmp,
                                               lon = "longitude",
                                               lat = "latitude",
                                               id = "site_id",
                                               stream_layer = stream_rast,
                                               accu_layer = flow_rast,
                                               method = "accumulation",
                                               distance = 500,
                                               accumulation = 0.5,
                                               quiet = FALSE)

#first check for NAs in the subcatchment IDs, this means it could not snap the site to the network
point_locations_snapped_tmp$site_id[which(point_locations_snapped_tmp$subc_id_snap_accu %in% NA)]
#two points could not be snapped using the values set for the function above
#I check these and fix them manually in qGIS, then re-load the new coordinates

#get the updated coordinates
coords<-read.csv(file.choose(), header=TRUE)
point_locations_tmp<-coords[,c(1,6,7)] #remember to adjust the columns if needed
colnames(point_locations_tmp)<-c("site_id","longitude","latitude")

#Re-snap the points
point_locations_snapped_tmp<-snap_to_network(data = point_locations_tmp,
                                               lon = "longitude",
                                               lat = "latitude",
                                               id = "site_id",
                                               stream_layer = stream_rast,
                                               accu_layer = flow_rast,
                                               method = "accumulation",
                                               distance = 500,
                                               accumulation = 0.5,
                                               quiet = FALSE)

#check for NAs again
point_locations_snapped_tmp$site_id[which(point_locations_snapped_tmp$subc_id_snap_accu %in% NA)]
#all fixed!


########################################################################################################
#
#                                         STEP 3: UPSTREAM AREAS
#
########################################################################################################

#Define the path for the direction raster layer
(direction_rast<-paste0(be_dir, "/be_merged_direction.tif"))
#Define the path where the upstream areas will be saved for each site
(upcatch_dir<-paste0(be_dir, "/upstream_catchment"))
#Create output folder if it doesn't exist
if(!dir.exists(upcatch_dir)) dir.create(upcatch_dir)

#Get the upstream catchment, note it is using the snapped point locations, not their original coordinates
#we'll only do the first 10 sites as examples
rows<-c(1:10)
get_upstream_catchment(as.data.table(point_locations_snapped_tmp[rows,]),
                       lon = "lon_snap_accu",
                       lat = "lat_snap_accu",
                       id = "site_id",
                       direction_layer = direction_rast,
                       out_dir = upcatch_dir,
                       n_cores = 2)


########################################################################################################
#
#                                         STEP 4: QA/QC
#
########################################################################################################

#now you have the upstream areas, but there will be errors caused by problems with the snapping or due to problems
#with the river network
#ideally, you would manually check every site, but the dataset is too large to do that
#the most obvious errors can typically be identified using the file size, and I tend to focus on those <1kb
#manually load all upstream areas below or around that size into qGIS (we'll do all 10 for now)

#Keep track of which sites need to be removed because the network is too broken
rem<-c(1,2,7,8)

#Keep track of which sites have updated coordinates
new<-c(3)

#Re-snap the updated coordinates
coords<-read.csv(file.choose(), header=TRUE)
point_locations_tmp<-coords[,c(1,6,7)]
colnames(point_locations_tmp)<-c("site_id","longitude","latitude")

point_locations_snapped_tmp<-snap_to_network(data = point_locations_tmp,
                                             lon = "longitude",
                                             lat = "latitude",
                                             id = "site_id",
                                             stream_layer = stream_rast,
                                             accu_layer = flow_rast,
                                             method = "accumulation",
                                             distance = 500,
                                             accumulation = 0.5,
                                             quiet = FALSE)

#Get the upstream areas for the updated sites
rows<-new
get_upstream_catchment(as.data.table(point_locations_snapped_tmp[rows,]),
                       lon = "lon_snap_accu",
                       lat = "lat_snap_accu",
                       id = "site_id",
                       direction_layer = direction_rast,
                       out_dir = upcatch_dir,
                       n_cores = 2)

#Remove the upstream areas for the sites that cannot be used
for (i in rem) {
  file.remove(paste0(upcatch_dir,"/upstream_basin_", coords$site[i],".tif"))
}


########################################################################################################
#
#                                         EXAMPLE RIVER INFORMATION
#
########################################################################################################

###---Upstream area size
#can now load the upstream areas and use various raster related functions to extract information

#for example, you can get the size of the upstream areas
#first restrict to the sites with upstream area files
coords2<-coords[c(3,4,5,6,9,10),]
coords2$up.area<-NA
for (i in 1:length(coords2$site)) {
  up.rast<-rast(paste0(upcatch_dir, "/upstream_basin_", coords2$site[i],".tif")) #functions loads the upstream area as a raster
  coords2$up.area[i]<-length(cells(up.rast)) #save the number of raster cells in the upstream area
}

#convert the number of cells to km2 based on cell size in the dataset
90*90 #Hydrographr uses a 90x90m cell size
coords2$up.km2<-(coords2$up.area*8100)*0.000001 #multiply the total cell size by 0.000001 to convert m2 to km2

###---Strahler order
#now that the points are snapped to the Hydrography90m network, you can also get data that is available from this network
#requires the ID of the subcatchment each site is located in
coords2$subc_id<-NA
coords2$order<-NA
for (i in 1:length(coords2$site)) {
  coords2$subc_id[i]<-point_locations_snapped_tmp$subc_id_snap_accu[which(point_locations_snapped_tmp$site_id==coords2$site[i])]
  coords2$order[i]<-order_vect_seg[which(order_vect_seg$stream==coords2$subc_id[i]),]$strahler
}
#there is a variety of additional information available
order_vect_seg[which(order_vect_seg$stream==coords2$subc_id[i]),]


########################################################################################################
#
#                                                LAND COVER
#
########################################################################################################

library(exactextractr)

#Annual land cover data from: https://cds.climate.copernicus.eu/datasets/satellite-land-cover?tab=overview
eu.lu<-"C:/Users/jsinclair/Documents/Research/12 - Large GIS/Europe land use"

coords2$urban<-NA
coords2$crop<-NA

for (i in 1:length(coords2$site)) {
  up.rast<-rast(paste0(upcatch_dir, "/upstream_basin_", coords2$site[i],".tif"))
  up.rast<-as.polygons(up.rast) #convert to polygon, which also converts to SpatVector
  up.rast<-st_as_sf(up.rast) #convert to sf format
  
  lu<-rast(paste0(eu.lu, "/EU_lccs_", 2020, ".tif"))
  
  dat<-exact_extract(x=lu, y=up.rast)
  dat2<-as.data.frame(dat[[1]])
  
  #Total area
  tot<-sum(dat2[,2])
  
  #Urban area
  num<-which(dat2[,1]>=190 & dat2[,1]<200) #urban
  if (length(num)==0) {
    coords2$urban[i]<-0
  } else {
    coords2$urban[i]<-sum(dat2[num,2])/tot
  }
  
  #Crop area
  num<-which(dat2[,1]>=10 & dat2[,1]<40) #crop
  if (length(num)==0) {
    coords2$crop[i]<-0
  } else {
    coords2$crop[i]<-sum(dat2[num,2])/tot
  }
}