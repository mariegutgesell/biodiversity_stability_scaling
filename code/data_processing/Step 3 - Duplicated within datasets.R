#this code identifies sites that lie within a certain threshold distance from each other within each dataset
#the purpose being to find sites that are close together in the same river section and identify which to keep and which to remove

###---Load libraries
library(sf)
library(geosphere)
library(Hmisc)
library(readxlsb)
library(units)
library(purrr)
library(tidyverse)
library(mapview)
##Read in data that has been assigned datafile names, database IDs, unique IDs, country codes, 
sites_mg <- read.csv("data/data_processing/Step2_MZB_sites_lotic.csv",   colClasses = c(Provider.Number = "character"))  

##NOTE: as of Nov 24, still have the issue of the duplicates from the two files (see issue #2 in step 2 code, otherwise all other data matches james)


#######################################################################################################
#
#                                   REMOVE ALREADY CHECKED DATASETS
#
#######################################################################################################

#we already have a list of datasets that have been checked, so these can be removed from the site list to
#avoid repeating effort - these can be removed from the site list of sites that need to be checked, not ones that should all be removed 

#file is called "Datasets already checked.csv"
rem<-read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Datasets already checked.csv")

sites3_test <- sites_mg %>%
  filter(!Dataset.ID %in% rem$checked) %>%
  filter(fulfills.requirement == "yes")
##so these would be the datasets that still need to be checked? 

##look at dfs made:
lo_sites_less_100m <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Lotic macroinvert - within less than 100m.csv")
lo_sites_within_100m_1km <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Lotic macroinvert - within 101-1000m.csv")

sites_to_be_removed_james <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Step 3 - duplicated within removed.csv")



#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES WITHIN DATASETS
#
#######################################################################################################

#overlap is checked within 100m and 1km, but lists must be examined manually
#within 100m is manually checked first because these are almost always overlapping so the checking is faster
#within 1km = >100m but <=1km, these are typically slower to get through
##make sure lat/long are numeric 
sites_mg <- sites_mg %>%
  mutate(Latitude_Y = as.numeric(Latitude_Y),
         Longitude_X = as.numeric(Longitude_X)) %>%
  filter(fulfills.requirement == "yes") ##select sites that fulfill requirements 


##function to identify clusters 
find_clusters <- function(df, min_dist = 100, max_dist = 1000) {
  if (nrow(df) < 2) return(tibble())
  
  # Make df into sf 
  df_sf <- st_as_sf(
    df,
    coords = c("Longitude_X", "Latitude_Y"),  # adjust if your column names differ
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3034)
  
  ##create distance matrix (in m)
  dmat <- st_distance(df_sf)
  dmat_num <- drop_units(dmat)
  
  #create vector list of length df (i.e., number of sites) to generate cluster id (group)
  clusters <- vector("list", length = nrow(df))
  cluster_id <- 1L
  
  ##for each site (k) identify which other sites fall within specific distance band (min_dist and max_dist) - but exclude the site itself 
  for (k in seq_len(nrow(df))) {
    ##extract the distances from site k to all sites
     dists_k <- dmat_num[, k]
    
    # identify neighbors > min & <= max (excluding self)
    neigh_idx <- which(dists_k > min_dist & dists_k <= max_dist)
    
    #if not neighbors, store null and skip to next site 
    if (length(neigh_idx) == 0) {
      clusters[[k]] <- NULL
      next
    }
    
    #
    # cluster = focal + neighbors
    idx <- c(k, neigh_idx)
    
    clusters[[k]] <- tibble(
      # one row per site in cluster
      group       = cluster_id,                 # like 'group' in original code
      unique.id   = df$Unique.ID[idx],
      org.site    = df$Site_ID_original[idx],
      river       = df$River.lake[idx],
      latitude    = df$Latitude_Y[idx],
      longitude   = df$Longitude_X[idx],
      st.yr       = df$Starting_year[idx],
      ed.yr       = df$Ending_year[idx],
      yr.ln       = df$Year_count[idx],
      yr.num      = df$Sampling_years[idx],
      origin      = df$origin[idx],
      country     = df$Country[1],
      prov        = df$Data_owner[1],
      dataset.id  = df$Dataset.ID[1],
      # distance from focal: first row = focal (0), rest from matrix
      distance_m  = c(0, dists_k[neigh_idx])
    )
    
    cluster_id <- cluster_id + 1L
  }
  
  bind_rows(clusters)
}

##function to filter redundant clusters 
#Sorts clusters from largest to smallest
#Keeps the large clusters
#For each cluster, it checks all the smaller clusters:
#  If a smaller cluster’s members are entirely contained within a larger one, the smaller cluster is marked as redundant

#Returns only the unique, non-subset clusters.
filter_redundant_clusters <- function(df_members) {
  # df_members: rows = groups, cols = group, members (list of unique.ids) 
  n <- nrow(df_members)
  keep <- rep(TRUE, n)
  
  # 1. Compute cluster sizes ----
  sizes <- vapply(df_members$members, length, integer(1))   #for each cluster, count how many members it contains
  ord <- order(-sizes) ##order clusters from largest to smallest - 
  
  # 2. Compare clusters --- 
  #loop through clusters in size order
  for (ii in seq_along(ord)) {
    i <- ord[ii] ##row index of the ith largest cluster
    if (!keep[i]) next ##if this cluster has already been marked redundant, skip
    
    set_i <- df_members$members[[i]] ##members of the current cluster
    
    #compare to all other clusters 
    for (jj in seq_along(ord)) {
      j <- ord[jj]
    
      ##skip if: -j is the same cluster as i, or j has already been marked for removal
      if (j == i || !keep[j]) next
      
      ##members of the candidate cluster
      set_j <- df_members$members[[j]]
      # if group j is a subset of group i, drop j
      
      # 3. Subset test --- 
      # if all members of cluster j are also in cluster i, then j is redundant and should be removed
      if (all(set_j %in% set_i)) {
        keep[j] <- FALSE
      }
    }
  }
  ##4. Return only the non-redundant clusters 
  df_members[keep, , drop = FALSE]
}

##Get clusters within 100m 
within_100m_clusters <- sites_mg %>%
  group_split(Dataset.ID) %>%
  map_dfr(find_clusters, min_dist = 0, max_dist = 100) %>%
  ungroup()

##Create df of groups to keep (non-redundant groups)
kept_groups_100m <- within_100m_clusters %>%
  group_by(dataset.id, group) %>%
  #Create a data frame where each row represents a cluster/group - contains list column where each element is a vector of unique site IDs 
  summarise(
    members = list(sort(unique(unique.id))),  # sorted vector of site IDs
    .groups = "drop"
  ) %>%
  ##apply function to filter out redundant clusters (i.e., duplicates)
  group_split(dataset.id) %>%
  map_dfr(filter_redundant_clusters)

##filter out the kept groups from full cluster list
within_100m_clusters_dedup <- within_100m_clusters %>%
  inner_join(kept_groups_100m, by = c("dataset.id", "group")) %>%
  mutate(within_distance_type = "within_100m")

##Get clusters within 1km 
within_1km_clusters <- sites_mg %>%
  group_split(Dataset.ID) %>%
  map_dfr(find_clusters, min_dist = 101, max_dist = 1000) %>%
  ungroup()

kept_groups_1km <- within_1km_clusters %>%
  group_by(dataset.id, group) %>%
  summarise(
    members = list(sort(unique(unique.id))),  # sorted vector of site IDs
    .groups = "drop"
  ) %>%
  group_split(dataset.id) %>%
  map_dfr(filter_redundant_clusters)

within_1km_clusters_dedup <- within_1km_clusters %>%
  inner_join(kept_groups_1km, by = c("dataset.id", "group")) %>%
  mutate(within_distance_type = "within_100m_to_1000m") 



cluster_count_1km <- within_1km_clusters_dedup %>% count(unique.id)
num_clusters_1km <- within_1km_clusters_dedup %>%
  group_by(dataset.id, group) %>%
  count()
##729 clusters/groups - 591 have 2 sites, 101 have 3 sites, 30 have 4 sites, 3 have 5 sites, 3 have 6 sites, and 1 has 10 sites 
num_in_cluster <- num_clusters_1km %>%
  group_by(n) %>%
  count()

##Each site only appears in one cluster/group for the within 100m
cluster_count_100m <- within_100m_clusters_dedup %>% count(unique.id)
num_clusters_100m <- within_100m_clusters_dedup %>%
  group_by(dataset.id, group) %>%
  count()
##79 clusters/groups - most have 2 sites per cluster, 8 have 3 sites per cluster, and 1 has 4 sites in cluster
##Then, need to go through each cluster  




##Save csv of site pairs 
#write.csv(within_100m, "data/data_processing/Step3_lotic_MZB_within_less_100m.csv")
#write.csv(within_1000m, "data/data_processing/Step3_lotic_MZB_within_100m_1000m.csv")


#######################################################################################################
#
#                          SORT SITES TO KEEP AND REMOVE OR CHECK MANUALLY
#
#######################################################################################################

#from workflow:
#1)  	Overlapping sites within <=100m are found
#Fix: Check group-by-group within just the group file. If the river names are different within the group, then keep all sites. If the river names are the same, pick one to keep (usually the one with the longest time series) and record the other sites for removal.

#2)  	Overlapping sites within >100–1000m are found.
#Fix: Check group-by-group within the group file. If the river names are different within the group, then keep all sites. If the river names are the same, check the sites manually in qGIS. If they occur in different river sections (e.g., different tributaries), then keep all sites. If they do not, pick one to keep (usually the one with the longer time series) and record the others for removal.


##My logic for keeping/removing sites from a group within a dataset - applied to each river within a group 
##1) if all river names are different - keep all 
##2) If river names are the same (or blank), keep longest time series
##3) If river names are the same (or blank) and time series length is the same, keep the one with the most sampling years
##4) If river names are the same (or blank) and time series length is the same and number of sampling years is the same, check manually
##5) If tied on all conditions above, or site is part of multiple groups and gets different score  - check manually, 

##Set up workflow so that for each site pair - if different river names, keep both, if no river name or if river name is the same, pick the one in the pair that has the longer time series 
##add a column that says keep/remove 
##create map so can look at each site pair in R - rather than needing to go to QGIS, this way can check and do all sorting etc. in R and is fully reproducible 

##Create sorting function
sorting_function <- function(df, tol = 1e-8) {
  df %>% 
    group_by(river, .add = TRUE) %>% ##group each cluster by river, so that within each cluster, if all unique rivers will keep all, but if there are duplicate rivers in cluster will treat as replicates ..
    mutate(
      n_in_river = n(), ##how many sites share this river in this cluster?
     
    #for rivers with more than 1 site: compute time-series summaries within that river 
      max_yr_ln  = max(yr.ln, na.rm = TRUE),
      n_max_ln   = sum(yr.ln == max_yr_ln),
      
     # max_yr_num = max(yr.num, na.rm = TRUE)
      # among sites with max yr.ln, what is the max # years?
      max_yr_num_tied = max(
        if_else(yr.ln == max_yr_ln, yr.num, NA_real_),
        na.rm = TRUE
      ),
      
      # how many sites are tied on BOTH metrics and have the same river name? 
      n_tied = sum(yr.ln == max_yr_ln & yr.num == max_yr_num_tied),
      
      ) %>%
    mutate(
      site_sorting = case_when(
        # 1) If river only appears once in the group - keep
        n_in_river == 1 ~ "keep",
        
        # 2) same river, any site with strictly shorter time series than the max → remove
        n_in_river > 1 & yr.ln < max_yr_ln ~ "remove",
        
        # 3) same river, unique longest time series (no tie) → keep
        n_in_river >1 & yr.ln == max_yr_ln & n_max_ln == 1 ~ "keep",
        
        #4) same river, tied for longest time series, but less number of sampling years - remove
        n_in_river > 1 & yr.ln == max_yr_ln & yr.num < max_yr_num_tied ~ "remove",
    
        
        #5) where they are tied on all 3, need to check manually and make decision 
       n_in_river >1 & n_tied >1 ~ "tied - check manually", 
        
        ##6) same river, tied for longest time series, but most number of sampling years - keep - order is important here, don't want to keep all the ones that are tied, so this comes after the tie indication
        n_in_river >1 & yr.ln == max_yr_ln & yr.num == max_yr_num_tied ~ "keep",
        
        
        # anything weird left over
      #  TRUE ~ "tied - check manually"
      )
    )
}


##Sort 100m data
within_100m_sorted <- within_100m_clusters_dedup %>%
  group_split(dataset.id, group) %>%
  map_dfr(sorting_function) %>%
  ungroup()

##Sort 1km data
within_1km_sorted <- within_1km_clusters_dedup %>%
  group_split(dataset.id, group) %>%
  map_dfr(sorting_function) %>%
  ungroup()


##Combine the sorted lists for both 100m and 1km 
sites_sorted <- rbind(within_100m_sorted, within_1km_sorted)


#######################################################################################################
#
#                          MANUALLY CHECK TIED SITES
#
#######################################################################################################

##sites tied 
tied <- sites_sorted %>%
  filter(n_tied >= 2) %>%
  select(site_sorting, group:n_tied)

num_groups_tied <- tied %>%
  select(dataset.id, within_distance_type, group) %>%
  distinct()
##77 groups tied 

##General tie breaker rules: if on same reach, keep site id that james kept (if in his list) or the one with lower site number (random but consistent - if not on his list), if 3 sites on same reach, keep one in middle, if on different tributaries - keep 
##Checking, dataset IDs in alphabetical order
## 	CHE_040_MZB_LO, group 1 ------
test_coords <- tied %>%
  filter(dataset.id == "CHE_040_MZB_LO") %>%
  filter(group == 1) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##different tributaries (i think) - keep both - same as james 

## 	CHE_076_MZB_LO, group 3 ------
test_coords <- tied %>%
  filter(dataset.id == "CHE_076_MZB_LO") %>%
  filter(group == 3) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##3 rivers names, just keep 1 from each, selecting lowest site code - some same as james, but keeping  at least and only one site from each unique river name

## 	DNK_011_MZB_LO, group 5 ------
test_coords <- tied %>%
  filter(dataset.id == "DNK_011_MZB_LO") %>%
  filter(group == 5) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##different tributaries, keep both - same as james

## 	ENG_041_MZB_LO, group 3 ------
test_coords <- tied %>%
  filter(dataset.id == "ENG_041_MZB_LO") %>%
  filter(group == 3) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same channel, only keep 1 - lower site # (not in james list)

## 	ENG_062_MZB_LO, group 15 ------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 15) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same river name, remove 1 - same as james (james removes 1211)

## 	ENG_062_MZB_LO, group 50 ------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 50) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 61 ------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 61) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 328 ------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 328) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 330 ------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 330) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 334------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 334) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##look like are on same channel, only keep 1 (not in james list)

## 	ENG_062_MZB_LO, group 340------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 340) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##look like are on same channel, only keep 1 - same as james 


## 	ENG_062_MZB_LO, group 452------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 452) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##2 of the same sites as in group 334, already sorted. third group has much shorter time series, so is removed 

## 	ENG_062_MZB_LO, group 503------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 503) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, only keep 1 site - same as james 

## 	ENG_062_MZB_LO, group 544------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 544) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, only keep 1 site - same as james 

## 	ENG_062_MZB_LO, group 610------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 610) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, only keep 1 site - same as james 

## 	ENG_062_MZB_LO, group 672------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 672) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, only keep 1 site - same site as james 

## 	ENG_062_MZB_LO, group 683------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 683) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##all 3 on same reach, keep the two are the furthest extremes, which are more than 1 km apart - this is to match james 

## 	ENG_062_MZB_LO, group 690------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 690) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, keep 1 - same as james 

## 	ENG_062_MZB_LO, group 694------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 694) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, keep 1 (not in james list)

## 	ENG_062_MZB_LO, group 695------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 695) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on two different rivers, within each river, two sites on same reach, just keep one from each - same ones as james 

## 	ENG_062_MZB_LO, group 713------
test_coords <- tied %>%
  filter(dataset.id == "ENG_062_MZB_LO") %>%
  filter(group == 713) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##looks like all three are on the same reach (a little hard to tell) - two edge ones - this is to match james

## 	ESP_034_MZB_LO, group 19------
test_coords <- tied %>%
  filter(dataset.id == "ESP_034_MZB_LO") %>%
  filter(group == 19) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##all 4 sites are on same reach, 2 of 4 are already removed due to short time series, keep only one of the two tied sites - keep one that doesn't lead to conflicted sorting (so is different than james) 

## 	ESP_060_MZB_LO, group 4------
test_coords <- tied %>%
  filter(dataset.id == "ESP_060_MZB_LO") %>%
  filter(group == 4) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach, keep 1 (not in james list)

## 	FIN_004_MZB_LO, group 9------
test_coords <- tied %>%
  filter(dataset.id == "FIN_004_MZB_LO") %>%
  filter(group == 9) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##3 sites on same reach, keep same one as james 

## 	FRA_015_MZB_LO, group 1------
test_coords <- tied %>%
  filter(dataset.id == "FRA_015_MZB_LO") %>%
  filter(group == 1) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach and river, only keep 1 - same one as james

## 	GER_017_MZB_LO, group 1 ------
test_coords <- tied %>%
  filter(dataset.id == "GER_017_MZB_LO") %>%
  filter(group == 1) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach and river, only keep 1 (not in james list)


## 	GER_018_MZB_LO, group 3------
test_coords <- tied %>%
  filter(dataset.id == "GER_018_MZB_LO") %>%
  filter(group == 3) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach and river, only keep 1 (not in james list)

## 	GER_018_MZB_LO, group 5------
test_coords <- tied %>%
  filter(dataset.id == "GER_018_MZB_LO") %>%
  filter(group == 5) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach and river, only keep 1 (not in james list)

## 	GER_047_MZB_LO, group 2------
test_coords <- tied %>%
  filter(dataset.id == "GER_047_MZB_LO") %>%
  filter(group == 2) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites are identical, just keep 1 (not in james list)

## 	GER_047_MZB_LO, group 3------
test_coords <- tied %>%
  filter(dataset.id == "GER_047_MZB_LO") %>%
  filter(group == 3) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites are identical, just keep 1 (not in james list)

## 	GER_070_MZB_LO, group 27-----
test_coords <- tied %>%
  filter(dataset.id == "GER_070_MZB_LO") %>%
  filter(group == 27) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach, just keep 1 (not on james list)

## 	GER_070_MZB_LO, group 34 **-----
test_coords <- tied %>%
  filter(dataset.id == "GER_070_MZB_LO") %>%
  filter(group == 34) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##all 3 on same reach, keep middle one - note overlap with group 35 (b/c two farthest sites are more than 1 km, but all are on the same reach, - keep the two farthest apart, gives some match then to one site james removed
 
## 	GER_070_MZB_LO, group 35 **-----
test_coords <- tied %>%
  filter(dataset.id == "GER_070_MZB_LO") %>%
  filter(group == 35) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##all 4 on same reach, keep middle one - also there is overlap here with group 34, keeping the two sites that are farthest apart (141 and 143)- 

## 	GER_070_MZB_LO, group 56-----
test_coords <- tied %>%
  filter(dataset.id == "GER_070_MZB_LO") %>%
  filter(group == 56) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, only keep 1 (not in james list)

## 	GER_071_MZB_LO, group 2-----
test_coords <- tied %>%
  filter(dataset.id == "GER_071_MZB_LO") %>%
  filter(group == 2) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##4 sites on the same reach, keep two furthest outside - this is to match james

## 	GER_071_MZB_LO, group 5-----
test_coords <- tied %>%
  filter(dataset.id == "GER_071_MZB_LO") %>%
  filter(group == 5) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##2 sites on same reach, keep only 1 - same one as james

## 	GER_071_MZB_LO, group 9-----
test_coords <- tied %>%
  filter(dataset.id == "GER_071_MZB_LO") %>%
  filter(group == 9) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##3 sites on same reach, keep only 1 - one is already removed due to fewer number of years, 
##also overlap with sites in group 10, so going to keep the two farthest part sites (9 and 10) to get max number of sites, even though 10 has 1 less year of samplng


## 	GER_072_MZB_LO, group 1-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 1) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##both on same reach, keep only 1 


## 	GER_072_MZB_LO, group 3-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 3) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##both on same reach, keep only 1 

## 	GER_072_MZB_LO, group 5-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 5) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##both on same reach, keep only 1 


## 	GER_072_MZB_LO, group 1-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 1) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##not entirely clear if on same reach, one looks like its not on any stream, and so could be on main reach or side trib, but likely on same, so keep 1 

## 	GER_072_MZB_LO, group 3-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 3) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1 

## 	GER_072_MZB_LO, group 5-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 5) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1 

## 	GER_072_MZB_LO, group 7-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 7) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1


## 	GER_072_MZB_LO, group 9-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 9) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1 

## 	GER_072_MZB_LO, group 11-----
test_coords <- tied %>%
  filter(dataset.id == "GER_072_MZB_LO") %>%
  filter(group == 11) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1 


## 	GER_073_MZB_LO, group 1-----
test_coords <- tied %>%
  filter(dataset.id == "GER_073_MZB_LO") %>%
  filter(group == 1) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1 - removing same one as james


## 	GER_073_MZB_LO, group 5-----
test_coords <- tied %>%
  filter(dataset.id == "GER_073_MZB_LO") %>%
  filter(group == 5) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same branch, keep 1 - removing same one as james


## 	HUN_022_MZB_LO, group 1-----
test_coords <- tied %>%
  filter(dataset.id == "HUN_022_MZB_LO") %>%
  filter(group == 1) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same point - removing same one as james

## 	IRL_023_MZB_LO, group 1-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_023_MZB_LO") %>%
  filter(group == 1) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##points not exactly on stream, but if snapped to closest river then would be on same reach - remove 1, same one as james 

## 	IRL_023_MZB_LO, group 3-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_023_MZB_LO") %>%
  filter(group == 3) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - removing same one as james 


## 	IRL_051_MZB_LO, group 9-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 9) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same point, remove 1 - removing same one as james 

## 	IRL_051_MZB_LO, group 3-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 3) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - 


## 	IRL_051_MZB_LO, group 41-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 41) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 43-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 43) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 61-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 61) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##all 3 on same reach, 2 are identical remove 2  - 1 same as james, and 1 more, keep lowest site number


## 	IRL_051_MZB_LO, group 71-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 71) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##hard to tell where stream is, but remove 1 - same as james 

## 	IRL_051_MZB_LO, group 128-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 128) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##hard to tell where stream is, but remove 1 - same as james 

## 	IRL_051_MZB_LO, group 132-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 132) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 157-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 157) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 190-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 190) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 204-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 204) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 241-----
test_coords <- tied %>%
  filter(dataset.id == "IRL_051_MZB_LO") %>%
  filter(group == 241) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on same reach, remove 1 - same as james 

## 	NOR_054_MZB_LO, group 2-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 2) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##3 sites, keep 1 - same as james 

## 	NOR_054_MZB_LO, group 6-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 6) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##3 sites, keep 1 - same as james 

## 	NOR_054_MZB_LO, group 9-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 9) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##on different tribs into the lake, keep both 

## 	NOR_054_MZB_LO, group 11-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 11) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same site, remove 1  - same as james  

## 	NOR_054_MZB_LO, group 13-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 13) %>%
  filter(within_distance_type == "within_100m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same reach, remove 1  - same as james 

## 	NOR_054_MZB_LO, group 3-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 3) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same sites that are also part of within 100m group 6, already dealt with 

## 	NOR_054_MZB_LO, group 5-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 5) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##same sites that are also part of within 100m group 6, and 100-1000m group 5, already dealt with 

## 	NOR_054_MZB_LO, group 11-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 11) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##different streams, keep both 

## 	NOR_054_MZB_LO, group 13-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 13) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##site 55 and 56 are on different tribs, site 57 on same trib as 55, so remove that one (same as james)


## 	NOR_054_MZB_LO, group 17-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 17) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites on different tribs into the lake, keep both 

## 	NOR_054_MZB_LO, group 18-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 18) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites on different tribs into different lakes, keep both 

## 	NOR_054_MZB_LO, group 21-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 21) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites on main stem and tributary, keep both 

## 	NOR_054_MZB_LO, group 25-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 25) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites both on same reach, remove 1 - same as james 


## 	NOR_054_MZB_LO, group 27-----
test_coords <- tied %>%
  filter(dataset.id == "NOR_054_MZB_LO") %>%
  filter(group == 27) %>%
  filter(within_distance_type == "within_100m_to_1000m") %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##sites both on same reach, remove 1 - same as james 




###Create final sorted df ----- 
##Select only sites previously already sorted - i.e., not tied
sites_sorted_notie <- sites_sorted %>%
  filter(site_sorting %in% c("keep", "remove")) %>%
  mutate(tiebreaker_state = site_sorting)

##make df of only tied 
sites_sorted_tie <- sites_sorted %>%
  filter(site_sorting %in% c("tied - check manually"))
##Make column with decision tree made of tie breaker states for each site ------------
sites_sorted_tie <- sites_sorted_tie %>%
  mutate(tiebreaker_state = case_when(
    grepl("CHE_040_MZB_LO_3", unique.id) ~ "keep",
    grepl("CHE_040_MZB_LO_10", unique.id) ~ "keep",
    grepl("CHE_076_MZB_LO_2", unique.id) ~ "keep",
    grepl("CHE_076_MZB_LO_3", unique.id) ~ "remove",
    grepl("CHE_076_MZB_LO_4", unique.id) ~ "keep",
    grepl("CHE_076_MZB_LO_5", unique.id) ~ "keep",
    grepl("CHE_076_MZB_LO_6", unique.id) ~ "remove",
    grepl("CHE_076_MZB_LO_7", unique.id) ~ "remove",
    grepl("CHE_076_MZB_LO_8", unique.id) ~ "remove",
    grepl("CHE_076_MZB_LO_9", unique.id) ~ "remove",
    grepl("DNK_011_MZB_LO_144", unique.id) ~ "keep",
    grepl("DNK_011_MZB_LO_145", unique.id) ~ "keep",
    grepl("ENG_041_MZB_LO_3496", unique.id) ~ "keep",
    grepl("ENG_041_MZB_LO_3513", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1211", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1382", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_285", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_286", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_325", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_456", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_1547", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_1926", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1554", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_1922", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1556", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3246", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1580", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_2103", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_2515", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_2516", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_2731", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3053", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3021", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3416", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3217", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3225", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3269", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3270", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3300", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3309", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3310", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3324", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3343", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3325", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3326", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3327", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3328", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3358", unique.id) ~ "keep",
    grepl("ENG_062_MZB_LO_3359", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_3360", unique.id) ~ "keep",
    grepl("ESP_034_MZB_LO_11", unique.id) ~ "keep",
    grepl("ESP_034_MZB_LO_38", unique.id) ~ "remove",
    grepl("ESP_060_MZB_LO_20", unique.id) ~ "keep",
    grepl("ESP_060_MZB_LO_21", unique.id) ~ "remove",
    grepl("FIN_004_MZB_LO_169", unique.id) ~ "remove",
    grepl("FIN_004_MZB_LO_170", unique.id) ~ "remove",
    grepl("FIN_004_MZB_LO_171", unique.id) ~ "keep",
    grepl("FRA_015_MZB_LO_1", unique.id) ~ "remove",
    grepl("FRA_015_MZB_LO_2", unique.id) ~ "keep",
    grepl("GER_017_MZB_LO_3", unique.id) ~ "keep",
    grepl("GER_017_MZB_LO_4", unique.id) ~ "remove",
    grepl("GER_018_MZB_LO_21", unique.id) ~ "keep",
    grepl("GER_018_MZB_LO_129", unique.id) ~ "remove",
    grepl("GER_018_MZB_LO_25", unique.id) ~ "keep",
    grepl("GER_018_MZB_LO_139", unique.id) ~ "remove",
    grepl("GER_047_MZB_LO_2", unique.id) ~ "keep",
    grepl("GER_047_MZB_LO_22", unique.id) ~ "remove",
    grepl("GER_047_MZB_LO_3", unique.id) ~ "keep",
    grepl("GER_047_MZB_LO_23", unique.id) ~ "remove",
    grepl("GER_070_MZB_LO_117", unique.id) ~ "keep",
    grepl("GER_070_MZB_LO_118", unique.id) ~ "remove",
    grepl("GER_070_MZB_LO_139", unique.id) ~ "remove",
    grepl("GER_070_MZB_LO_141", unique.id) ~ "keep",
    grepl("GER_070_MZB_LO_142", unique.id) ~ "remove",
    grepl("GER_070_MZB_LO_140", unique.id) ~ "remove",
    grepl("GER_070_MZB_LO_143", unique.id) ~ "keep",
    grepl("GER_070_MZB_LO_392", unique.id) ~ "keep",
    grepl("GER_070_MZB_LO_410", unique.id) ~ "remove",
    grepl("GER_071_MZB_LO_3", unique.id) ~ "keep",
    grepl("GER_071_MZB_LO_4", unique.id) ~ "remove",
    grepl("GER_071_MZB_LO_5", unique.id) ~ "remove",
    grepl("GER_071_MZB_LO_6", unique.id) ~ "keep",
    grepl("GER_071_MZB_LO_7", unique.id) ~ "remove",
    grepl("GER_071_MZB_LO_8", unique.id) ~ "keep",
    grepl("GER_071_MZB_LO_11", unique.id) ~ "remove",
    grepl("GER_071_MZB_LO_12", unique.id) ~ "remove", 
    grepl("GER_072_MZB_LO_14", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_15", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_16", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_17", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_20", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_21", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_1", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_2", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_3", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_4", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_5", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_6", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_9", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_10", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_12", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_13", unique.id) ~ "keep",
    grepl("GER_072_MZB_LO_18", unique.id) ~ "remove",
    grepl("GER_072_MZB_LO_19", unique.id) ~ "keep",
    grepl("GER_073_MZB_LO_55", unique.id) ~ "keep",
    grepl("GER_073_MZB_LO_56", unique.id) ~ "remove",
    grepl("GER_073_MZB_LO_59", unique.id) ~ "keep",
    grepl("GER_073_MZB_LO_60", unique.id) ~ "remove",
    grepl("HUN_022_MZB_LO_22", unique.id) ~ "keep",
    grepl("HUN_022_MZB_LO_54", unique.id) ~ "remove",
    grepl("IRL_023_MZB_LO_3", unique.id) ~ "keep",
    grepl("IRL_023_MZB_LO_4", unique.id) ~ "remove",
    grepl("IRL_023_MZB_LO_11", unique.id) ~ "keep",
    grepl("IRL_023_MZB_LO_12", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_1284", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_1285", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_71", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_72", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_238", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_239", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_241", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_242", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_369", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_370", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_371", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_477", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_478", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_853", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_854", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_873", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_874", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_1032", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_1033", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_1349", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_1350", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_1422", unique.id) ~ "keep",
    grepl("IRL_051_MZB_LO_1423", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_1634", unique.id) ~ "remove",
    grepl("IRL_051_MZB_LO_1635", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_42", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_43", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_44", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_46", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_47", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_48", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_49", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_53", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_58", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_59", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_80", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_81", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_54", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_55", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_56", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_57", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_63", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_67", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_65", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_68", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_71", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_72", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_75", unique.id) ~ "remove",
    grepl("NOR_054_MZB_LO_76", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_82", unique.id) ~ "keep",
    grepl("NOR_054_MZB_LO_83", unique.id) ~ "remove",
    
    
  ))

##join back into single df 
sites_sorted_tiebreaker <- rbind(sites_sorted_notie, sites_sorted_tie)

#######################################################################################################
#
#                          MANUALLY CHECK CONFLICTING SITE SORTING
#
#######################################################################################################

####Look into conflicting sites ------------------

##Check if any unique.ids have conflicting sorting - i.e., in some pairs are listed as keep and others as remove
conflicted_ids <- sites_sorted_tiebreaker %>%
  group_by(unique.id) %>%
  summarise(
    n_states = n_distinct(tiebreaker_state),
    states   = paste(sort(unique(tiebreaker_state)), collapse = ", ")
  ) %>%
  filter(n_states > 1) 
##15 with conflicting states - these also need to be checked manually  ... 

##Conflicted site: ENG_062_MZB_LO_1113 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_1113")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##only kept because came up in <100m and was a trib and mainstem so different names so kept, but is too close to others on main stem, so remove


##Conflicted site: ENG_062_MZB_LO_1211 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_1211")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove  (fixed in tie coding above)


##Conflicted site: ENG_062_MZB_LO_1231 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_1231")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove - kept b/c unique stream name in group - shows up in two groups, in one is removed b/c fewer sampling years. remove

##Conflicted site: ENG_062_MZB_LO_1382 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_1382")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove - too close to other sites - shows up in 2 groups (same as 1211) (fixed in tie coding above)

##Conflicted site: ENG_062_MZB_LO_2104 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_2104")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove, 2 other sites close on main stem, and want to keep the one <100m on other trib

##Conflicted site: ENG_062_MZB_LO_2995 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_2995")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove, 2 other sites close on main stem, and want to keep the one <100m on other trib

##Conflicted site: ENG_062_MZB_LO_3206 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_3206")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove, shows up in two groups, another close site that has longer time series 

##Conflicted site: ENG_062_MZB_LO_3246 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ENG_062_MZB_LO_3246")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove, shows up in two groups, only 2 of these sites can be kept - keeping longest time series 


##Conflicted site: ESP_034_MZB_LO_11 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ESP_034_MZB_LO_11")))) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##keep, remove other tied site to avoid conflict - all was equal had only selected other to be same as james, but clearer reasoning here (fixed in tie coding above)

##Conflicted site: ESP_034_MZB_LO_33 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "ESP_034_MZB_LO_33")))) %>% 
  filter(group %in% c(16, 42, 73)) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##is in 3 groups, kept in one group where it had the longest time series, but in other two groups does not, so remove, but in the group where it was kept (16), keep the site that had second longest time seires (site 366)

##Conflicted site: GER_071_MZB_LO_11 / 10 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "GER_071_MZB_LO_11")))) %>% 
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##going to keep the two sites furthest apart (9 and 10), fix coding of 10 below because originally marked to remove b/c 1 less sampling year. but this way get to keep two sites rather than just 1, and 9 and 10 are more than 1 km apart 

##Conflicted site: IRL_051_MZB_LO_643 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "IRL_051_MZB_LO_643")))) %>% 
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##remove, too close to another site on same stem 

##Conflicted site: NOR_054_MZB_LO_45 / 47 / 48 ------
test_coords <- sites_sorted_tiebreaker %>%
  filter(map_lgl(members, ~ any(str_detect(.x, "NOR_054_MZB_LO_45")))) %>% 
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "OpenTopoMap",legend = TRUE, zcol = "unique.id",  alpha = 1, alpha.regions = 1, cex = 4)
##says it is on a different river than 16, but its not - so keep 16 and remove 45 
##remove 47 and 48, only come as keep because different river than 16, but too close to 46, so remove 


###Resolve conflicts - make column with decision to fix conflicts, and assign final states  ------------
sites_sorted_final <- sites_sorted_tiebreaker %>%
  mutate(final_state = case_when(
    startsWith(unique.id, "ENG_062_MZB_LO_1113") ~ "remove",
    startsWith(unique.id, "ENG_062_MZB_LO_1231") ~ "remove",
    startsWith(unique.id, "ENG_062_MZB_LO_2104") ~ "remove",
    startsWith(unique.id, "ENG_062_MZB_LO_2995") ~ "remove",
    startsWith(unique.id, "ENG_062_MZB_LO_3206") ~ "remove",
    startsWith(unique.id, "ENG_062_MZB_LO_3246") ~ "remove",
    startsWith(unique.id, "ESP_034_MZB_LO_33") ~ "remove",
    startsWith(unique.id, "ESP_034_MZB_LO_366") ~ "keep",
    startsWith(unique.id, "GER_071_MZB_LO_10") ~ "keep",
    startsWith(unique.id, "IRL_051_MZB_LO_643") ~ "remove",
    startsWith(unique.id, "NOR_054_MZB_LO_45") ~ "remove",
    startsWith(unique.id, "NOR_054_MZB_LO_47") ~ "remove",
    startsWith(unique.id, "NOR_054_MZB_LO_48") ~ "remove",
    TRUE ~ NA_character_
  ),
  final_state = coalesce(final_state, tiebreaker_state) ##fill in any NAs with the state written in the tiebreaker
  ) 

##double check there are no more conflicting sites 
conflicted_ids <- sites_sorted_final %>%
  group_by(unique.id) %>%
  summarise(
    n_states = n_distinct(final_state),
    states   = paste(sort(unique(final_state)), collapse = ", ")
  ) %>%
  filter(n_states > 1) 
## hell ya no conflicts 


##Make final df of sites to remove ---------
sites_to_be_removed_marie <- sites_sorted_final %>%
  filter(final_state == "remove")
##I remove 371 sites 
sites_to_be_removed_marie$members <- as.character(sites_to_be_removed_marie$members)

##save csv
write.csv(sites_to_be_removed_marie, "data/data_processing/Step3_within_datasets_sites_to_be_removed_.csv")



##Look into overlapping sites between james and Marie  -----------
##when james has one marked to remove that i dont, i have usually selected the one with longer time series and/or more sampling years. not sure why he has selected the shorter one 
##some sites missing from james that really should be removed (e.g., GER 018, GER 047, GER 070)


unique.id_marie <- sites_to_be_removed_marie %>%
  select(unique.id) %>%
  distinct() 
##I remove 322 sites 

unique.id_james <- sites_to_be_removed_james %>%
  select(unique.id) %>%
  distinct() 
##james removes 297 sites 


###Look into figure out where there is and is not overlap 
##create a master list of all unique ids that show up in both removal dfs
all_ids <- union(sites_to_be_removed_marie$unique.id, sites_to_be_removed_james$unique.id) %>% 
  tibble(unique.id = .) %>%
  left_join(sites_mg %>% select(Dataset.ID, Unique.ID) %>% rename(unique.id = "Unique.ID"), by = "unique.id")

compare_df <- all_ids %>%
  mutate(
    marie_remove = unique.id %in% sites_to_be_removed_marie$unique.id,
    james_remove = unique.id %in% sites_to_be_removed_james$unique.id
  ) %>%
  mutate(
    category = case_when(
      marie_remove & james_remove ~ "both_remove",
      marie_remove & !james_remove ~ "marie_only",
      !marie_remove & james_remove ~ "james_only",
      TRUE ~ "neither"   # shouldn’t happen since union was taken
    )
  )

##calculate # of differences
table(compare_df$category)

119-94
322-297

james_only_id <- compare_df %>% filter(category == "james_only") 

james_remove_sites <- sites_sorted %>%
  filter(map_lgl(members, ~ any(.x %in% james_only_id$unique.id))) %>%
  mutate(james_removed = case_when(
    unique.id %in% james_only_id$unique.id ~ "james_removed"
  )) %>%
  select(site_sorting, james_removed, group:n_tied)

##30 of these 92 do come up in my site sorted
##of these 30, often marked removed by james even if river name is different, or sometimes the shorter time series was selected (when same river name)
##so 62 do not? 

##so a bunch are sites that don't even come up in my distance grouping... look into those later 


##lets just look at the ones that are tied/same distance 
test_coords <- tied %>%
  filter(unique.id %in% c("ENG_062_MZB_LO_1211", "ENG_062_MZB_LO_1382")) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "Esri.WorldTopoMap",legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)

str(test_coords)

head(tied)




head(tied)
  

##Conflicting Site 1 and 2: ENG_062_MZB_LO_1108 and ENG_062_MZB_LO_1113
test_coords <- sites_sorted %>%
  filter(str_detect(pair_id, "ENG_062_MZB_LO_1108|ENG_062_MZB_LO_1113")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)
##5 sites here, removing these two ensures all sites are at least 1km apart and/or on different tribs/rivers
##Decision - remove ENG_062_MZB_LO_1108 and ENG_062_MZB_LO_1113


##Conflicting Site 3: ENG_062_MZB_LO_1145
test_coords <- sites_sorted %>%
  filter(str_detect(pair_id, "ENG_062_MZB_LO_1145")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)
#remove, as within 1000m on same river, and keeps other site on other trib 


##Conflicting Site 4: ENG_062_MZB_LO_1153
test_coords <- sites_sorted %>%
  filter(str_detect(pair_id, "ENG_062_MZB_LO_1153")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)
#remove, as within 1000m on same river 


##Conflicting Site 5: ENG_062_MZB_LO_1176
test_coords <- sites_sorted %>%
  filter(str_detect(pair_id, "ENG_062_MZB_LO_1176")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)
#keep, 
###come back to this one ... as when all tied getting two keeps - one sec need to figure that out above

##Make column with decision tree made of final states for each site 
conflicted_ids_1000m_2 <- conflicted_ids_1000m %>%
  mutate(final_state = case_when(
    grepl("ENG_062_MZB_LO_1108", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1113", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1145", unique.id) ~ "remove",
    grepl("ENG_062_MZB_LO_1153", unique.id) ~ "remove",
  ))




##Make df of sites to be removed 

sites_to_be_removed_marie <- sites_sorted %>%
  filter(site_sorting == "remove")
##okay i think the only issue here is then when river name for one is blank, and the other is not ... because don't really know if they are the same river or not 

##test - how does this compare to the list of sites indicated that need to be removed? 
sites_to_be_removed_james <- as.data.frame(read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Step 3 - duplicated within removed.csv"))
str(sites_to_be_removed_james)

###Look into figure out where there is and is not overlap 
##create a master list of all unique ids that show up in both removal dfs
all_ids <- union(sites_to_be_removed_marie$unique.id, sites_to_be_removed_james$unique.id) %>% 
  tibble(unique.id = .) %>%
  left_join(sites_mg %>% select(Dataset.ID, Unique.ID) %>% rename(unique.id = "Unique.ID"), by = "unique.id")

compare_df <- all_ids %>%
  mutate(
    marie_remove = unique.id %in% sites_to_be_removed_marie$unique.id,
    james_remove = unique.id %in% sites_to_be_removed_james$unique.id
  ) %>%
  mutate(
    category = case_when(
      marie_remove & james_remove ~ "both_remove",
      marie_remove & !james_remove ~ "marie_only",
      !marie_remove & james_remove ~ "james_only",
      TRUE ~ "neither"   # shouldn’t happen since union was taken
    )
  )

##calculate # of differences
table(compare_df$category)





marie_only_df <- compare_df %>%
  filter(category == "marie_only") %>%
  left_join(sites_sorted, by = "unique.id") ##part of issue here is what to do about the ones that are sometimes keep and sometimes remove - i think those you would have to go through manually
marie_dataset_ids <- marie_only_df %>%
  select(Dataset.ID) %>%
  unique()

test_coords <- sites_sorted %>%
  filter(pair_id == "ENG_062_MZB_LO_1089__ENG_062_MZB_LO_992") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


mapview(test_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)


james_only_df <- compare_df %>%
  filter(category == "james_only") #%>%
  #left_join(sites_sorted, by = "unique.id")
##so there are a whole bunch that did not come up in my list of 100-1000m 

test <- james_only_df %>%
  filter(!unique.id %in% sites_sorted$unique.id)
##so 68 sites that don't come up in my site list - why? - so these are ones that are not even in the site list? wtf ... 

test2 <- sites_mg %>%
  filter(Unique.ID %in% test$unique.id)

##Check if any unique.ids have conflicting sorting - i.e., in some pairs are listed as keep and others as remove
conflicted_ids_100m <- within_100m_sorted %>%
  group_by(unique.id) %>%
  summarise(
    n_states = n_distinct(site_sorting),
    states   = paste(sort(unique(site_sorting)), collapse = ", ")
  ) %>%
  filter(n_states > 1)
##no conflicting ones at 100m
conflicted_ids_1000m <- within_1000m_sorted %>%
  group_by(unique.id) %>%
  summarise(
    n_states = n_distinct(site_sorting),
    states   = paste(sort(unique(site_sorting)), collapse = ", ")
  ) %>%
  filter(n_states > 1)
##48 sites with conflicting states here..how do we figure which one to do? 

##are any of these confilected sites in james to remove?
test <- sites_to_be_removed_marie %>%
  filter(unique.id %in% conflicted_ids_1000m$unique.id)


##look into the conflicts:
conflicted_sites <- sites_sorted %>%
  filter(unique.id %in% conflicted_ids_1000m$unique.id)


##still some NAs
test <- within_1000m_sorted %>%
  filter(is.na(site_sorting))


sites_to_remove_100m <- within_100m_sorted %>%
  filter(site_sorting == "remove")

sites_to_remove_1000m <- within_1000m_sorted %>%
  filter(site_sorting== "remove")

sites_to_be_removed_marie <- rbind(sites_to_remove_1000m, sites_to_remove_100m)
##okay i think the only issue here is then when river name for one is blank, and the other is not ... because don't really know if they are the same river or not 

##test - how does this compare to the list of sites indicated that need to be removed? 
sites_to_be_removed_james <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Step 3 - duplicated within removed.csv")


##checking site overlaps
sites_to_be_removed_marie <- sites_to_be_removed_marie %>% mutate(unique.id = trimws(unique.id))
sites_to_be_removed_james <- sites_to_be_removed_james %>% mutate(unique.id = trimws(unique.id))

##sites indicated by james to remove that i did not list
test <- anti_join(sites_to_be_removed_james, sites_to_be_removed_marie, by = "unique.id")

##sites i listed to remove but james did not 
test1 <- anti_join(sites_to_be_removed_marie, sites_to_be_removed_james, by = "unique.id")

345-238

107+87
sum(sites_to_be_removed_marie$unique.id %in% sites_to_be_removed_james$unique.id)

##sites we both said to remove:
both_remove <- inner_join(sites_to_be_removed_marie, sites_to_be_removed_james, by = "unique.id")
##okay so somewhere have duplicate unique ids.. this is odd

marie_dupes <- sites_to_be_removed_marie %>%
  group_by(unique.id) %>%
  filter(n() > 1) %>%
  ungroup()
##duplicates here i think because come up in different pairs, and in all come up as remove - however, this does not solve the issue of when have both keep and remove ...

james_dupes <- sites_to_be_removed_james %>%
  group_by(unique.id) %>%
  filter(n() > 1) %>%
  ungroup()
##so why do we have duplicates here? 


##Plotting out on map to look at sites 
within_100m_sorted_coords <- st_as_sf(within_100m_sorted, coords = c("longitude", "latitude"), crs = 4326)
site_map_100m <- mapview(within_100m_sorted_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)
site_map_100m



##so maybe i am crazy, but wouldn't we want to do all pairwise comparisons? not just grouped by dataset? 

test_coords <- sites_sorted %>%
  filter(pair_id == "ENG_062_MZB_LO_1089__ENG_062_MZB_LO_992") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mapview(test_coords, map.types = "Esri.WorldTopoMap",zcol = "pair_id", legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)

#######################################################################################################
#
#                                      SAVE THE DATASETS
#
#######################################################################################################

write.csv(within.100m.2, "Lotic macroinvert - within less than 100m.csv", row.names=FALSE, fileEncoding = "latin1")
write.csv(within.1km.2, "Lotic macroinvert - within 101-1000m.csv", row.names=FALSE, fileEncoding = "latin1")

#next, load the site list into qGIS and manually examine by group, specifically focusing on groups with the same river names
#note do not overwrite the Excel files with the CSVs, only add from the CSVs to what is already in the Excel sheets