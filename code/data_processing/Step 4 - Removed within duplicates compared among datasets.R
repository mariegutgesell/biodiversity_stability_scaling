#checks whether sites listed for removal also present in other datasets using a 100m threshold

###---Load libraries
library(sf)
library(geosphere)
library(Hmisc)
library(raster)

#######################################################################################################
#
#                                       READ IN DATA
#
#######################################################################################################


##Read in data that has been assigned datafile names, database IDs, unique IDs, country codes, 
sites_mg <- read.csv("data/processed/step2_site_ids/Step2_MZB_sites_lotic.csv",   colClasses = c(Provider.Number = "character"))  %>%
  mutate(Latitude_Y = as.numeric(Latitude_Y),  Longitude_X = as.numeric(Longitude_X)) %>% ##ensure lat and long are numeric
  filter(fulfills.requirement == "yes") ##select sites that fulfill requirements - this is based on dataset criteria listed in data call 

##Read in list of sites to be removed from step 3 
rem <- read.csv("data/processed/step3_overlap_within/Step3_within_datasets_sites_to_be_removed.csv")


#######################################################################################################
#
#                                           FUNCTIONS
#
#######################################################################################################
##Function to find sites in different datasets from the same country, that are within 100m of sites to be removed
find_overlap_across <- function(df, df_rem, min_dist = 0, max_dist = 100) {
  # convert both to sf once
  df_sf <- st_as_sf(
    df,
    coords = c("Longitude_X", "Latitude_Y"),
    crs    = 4326,
    remove = FALSE
  ) %>%
    st_transform(3034)
  
  rem_sf <- st_as_sf(
    df_rem,
    coords = c("longitude", "latitude"),
    crs    = 4326,
    remove = FALSE
  ) %>%
    st_transform(3034)
  
  out_list <- vector("list", nrow(df_rem))
  group_id <- 1L
  
  for (i in seq_len(nrow(df_rem))) {
    focal <- rem_sf[i, ]
    
    # filter candidate sites in full df:
    # same country, different dataset.id
    df_test <- df_sf %>%
      filter(
        Country    == focal$country,
        Dataset.ID != focal$dataset.id
      )
    
    if (nrow(df_test) == 0) next
    
    # distances from focal (1 row) to all df_test sites
    dists <- st_distance(focal, df_test)
    dists_num <- drop_units(dists[1, ])  # numeric vector
    
    # neighbors within distance band
    neigh_idx <- which(dists_num >= min_dist & dists_num <= max_dist)
    if (length(neigh_idx) == 0) next
    
    neigh <- df_test[neigh_idx, ]
    neigh_dists <- dists_num[neigh_idx]
    
    # focal row
    focal_tbl <- tibble(
      group      = group_id,
      unique.id  = focal$unique.id,
      org.site   = focal$org.site,
      river      = focal$river,
      latitude   = focal$latitude,
      longitude  = focal$longitude,
      st.yr      = focal$st.yr,
      ed.yr      = focal$ed.yr,
      yr.ln      = focal$yr.ln,
      yr.num     = focal$yr.num,
      origin     = focal$origin,
      country    = focal$country,
      prov       = focal$prov,
      dataset.id = focal$dataset.id,
      distance_m = 0
    )
    
    # neighbor rows
    neigh_tbl <- tibble(
      group      = group_id,
      unique.id  = neigh$Unique.ID,
      org.site   = neigh$Site_ID_original,
      river      = neigh$Site_name,
      latitude   = neigh$Latitude_Y,
      longitude  = neigh$Longitude_X,
      st.yr      = neigh$Starting_year,
      ed.yr      = neigh$Ending_year,
      yr.ln      = neigh$Year_count,
      yr.num     = neigh$Sampling_years,
      origin     = neigh$origin,
      country    = neigh$Country,
      prov       = neigh$Data_owner,
      dataset.id = neigh$Dataset.ID,
      distance_m = neigh_dists
    )
    
    out_list[[group_id]] <- bind_rows(focal_tbl, neigh_tbl)
    group_id <- group_id + 1L
  }
  
  bind_rows(out_list)
}

##Function to plot tied sites to manually check tie breakers 
plot_group <- function(overlap_df, group_id, within_distance_type = NULL,map_type = "OpenTopoMap") {
  df <- overlap_df %>%
    filter(
      group == group_id
    )
  
  if (!is.null(within_distance_type)) {
    df <- df %>%
      filter(within_distance_type == !!within_distance_type)
  }
  
  df_sf <- df %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  mapview(
    df_sf,
    map.types      = map_type,
    legend         = TRUE,
    zcol           = "unique.id",
    alpha          = 1,
    alpha.regions  = 1,
    cex            = 4
  )
}



#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES BETWEEN DATASETS
#
#######################################################################################################

##Identify clusters within 100m and filter out redundant clusters ------------------
##remove sites already 

##Get clusters within 100m 
across_100m_dist <- find_overlap_across(sites_mg, rem, min_dist = 0, max_dist = 100)
 

#######################################################################################################
#
#                 PLOT GROUPS TO SORT OVERLAP
#
#######################################################################################################
plot_group(across_100m_dist, 1)
plot_group(across_100m_dist, 2) 


#######################################################################################################
#
#                                      SAVE THE DATASET
#
#######################################################################################################

write.csv(rem.100m.2, "Lotic macroinvert - removed less than 100m.csv", row.names=FALSE, fileEncoding = "latin1")
#next, load the site list into qGIS and manually examine by group