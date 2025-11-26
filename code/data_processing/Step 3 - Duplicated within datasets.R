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

sites_to_be_removed <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Step 3 - duplicated within removed.csv")



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


find_close_pairs <- function(df, min_dist = 0, max_dist = 100) {
  # df = one dataset (one Dataset.ID)
  if (nrow(df) < 2) return(tibble())  # nothing to compare
  
  # Make sf object in projected CRS (meters)
  df_sf <- st_as_sf(
    df,
    coords = c("Longitude_X", "Latitude_Y"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3034)  
  
  # Distance matrix (units in m)
  dmat <- st_distance(df_sf)
  dmat_num <- drop_units(dmat)  # numeric matrix
  
  # Get upper triangle pairs with 0 < d <= cutoff (so don't have duplicated pairs)
  idx <- which(
    dmat_num > min_dist & dmat_num <= max_dist & upper.tri(dmat_num),
    arr.ind = TRUE
  )
  
  if (nrow(idx) == 0) return(tibble())
  
  i <- idx[, "row"]
  j <- idx[, "col"]
  
  # Build long-form pairwise df
  tibble(
    country     = df$Country[i],
    prov        = df$Data_owner[i],
    origin_i    = df$origin[i],
    origin_j    = df$origin[j],
    dataset.id  = df$Dataset.ID[i],
    org.site_i  = df$Site_ID_original[i],
    org.site_j  = df$Site_ID_original[j],
    unique.id_i = df$Unique.ID[i],
    unique.id_j = df$Unique.ID[j],
    river_i     = df$River.lake[i],
    river_j     = df$River.lake[j],
    latitude_i  = df$Latitude_Y[i],
    latitude_j  = df$Latitude_Y[j],
    longitude_i = df$Longitude_X[i],
    longitude_j = df$Longitude_X[j],
    st.yr_i     = df$Starting_year[i],
    st.yr_j     = df$Starting_year[j],
    ed.yr_i     = df$Ending_year[i],
    ed.yr_j     = df$Ending_year[j],
    yr.ln_i     = df$Year_count[i],
    yr.ln_j     = df$Year_count[j],
    yr.num_i    = df$Sampling_years[i],
    yr.num_j    = df$Sampling_years[j],
    distance_m  = dmat_num[idx] 
  )
}

##Get list of site pairs within 100m, within each dataset 
within_100m <- sites_mg %>%
  group_split(Dataset.ID) %>%
  map_dfr(find_close_pairs, min_dist= 0, max_dist = 100) %>%
  ungroup() %>%
  mutate(within_distance_type = "within_100m") %>%
  mutate(pair_id = if_else(
    unique.id_i < unique.id_j,
    paste(unique.id_i, unique.id_j, sep = "__"),
    paste(unique.id_j, unique.id_i, sep = "__") ##create a unique id for the pair, so know which sites are the pair
  )) %>%
  pivot_longer( cols = matches("_(i|j)$"),               # all columns ending in _i or _j
                names_to = c(".value", "site_role"),     # .value = base name, site_role = i/j
                names_pattern = "(.+)_([ij])$") ##transform table so in long form - so have one row for each unique site id 


##Get list of site pairs within 100m-1000m, within each dataset 
within_1000m <- sites3_test %>%
  group_split(Dataset.ID) %>%
  map_dfr(find_close_pairs, min_dist = 101, max_dist = 1000) %>%
  ungroup() %>%
   mutate(within_distance_type = "within_100m_to_1000m") %>%
  mutate(pair_id = if_else(
    unique.id_i < unique.id_j,
    paste(unique.id_i, unique.id_j, sep = "__"),
    paste(unique.id_j, unique.id_i, sep = "__") ##create a unique id for the pair, so know which sites are the pair
  )) %>%
  pivot_longer( cols = matches("_(i|j)$"),               # all columns ending in _i or _j
                names_to = c(".value", "site_role"),     # .value = base name, site_role = i/j
                names_pattern = "(.+)_([ij])$") ##transform table so in long form - so have one row for each unique site id 

##i am getting more/different sites than james


marie_1000m_unique_ids <- within_1000m %>%
  select(unique.id) %>%
  distinct()

james_1000m_unique_ids <- within.1km.2 %>%
  select(unique.id) %>%
  distinct()

james_marie_overlap_1000m <- inner_join(marie_1000m_unique_ids, james_1000m_unique_ids, by = "unique.id")


##Save csv of site pairs 
write.csv(within_100m, "data/data_processing/Step3_lotic_MZB_within_less_100m.csv")
write.csv(within_1000m, "data/data_processing/Step3_lotic_MZB_within_100m_1000m.csv")



##Trying approach using clusters rather than pairwise comparisons
df <- sites_mg %>%
  filter(Dataset.ID == "CZE_003_MZB_LO")
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
#from workflow:
#1)  	Overlapping sites within <=100m are found
#Fix: Check group-by-group within just the group file. If the river names are different within the group, then keep all sites. If the river names are the same, pick one to keep (usually the one with the longest time series) and record the other sites for removal.

#2)  	Overlapping sites within >100–1000m are found.
#Fix: Check group-by-group within the group file. If the river names are different within the group, then keep all sites. If the river names are the same, check the sites manually in qGIS. If they occur in different river sections (e.g., different tributaries), then keep all sites. If they do not, pick one to keep (usually the one with the longer time series) and record the others for removal.


##My logic for keeping/removing sites from a group within a dataset
##1) if all river names are different - keep all 
##2) If river names are the same (or blank), keep longest time series
##3) If river names are the same (or blank) and time series length is the same, keep the one with the most sampling years
##4) If river names are the same (or blank) and time series length is the same and number of sampling years is the same, keep the one in the group that is closest to the centroid of the cluster of groups
##5) If tied on all conditions above, or site is part of multiple groups and gets different score  - check manually, 

##Set up workflow so that for each site pair - if different river names, keep both, if no river name or if river name is the same, pick the one in the pair that has the longer time series 
##add a column that says keep/remove 
##create map so can look at each site pair in R - rather than needing to go to QGIS, this way can check and do all sorting etc. in R and is fully reproducible 

##Create sorting function
sorting_function <- function(df, tol = 1e-8) {
  df %>% 
    mutate(
      # do all sites share the same river? (ignore NAs)
      same_river = n_distinct(river) <= 1,
      max_yr_ln  = max(yr.ln, na.rm = TRUE),
      n_max_ln   = sum(yr.ln == max_yr_ln),
      
     # max_yr_num = max(yr.num, na.rm = TRUE)
      # among sites with max yr.ln, what is the max # years?
      max_yr_num_tied = max(
        if_else(yr.ln == max_yr_ln, yr.num, NA_real_),
        na.rm = TRUE
      ),
      
      # how many sites are tied on BOTH metrics and have the same river name? 
      n_tied = sum(yr.ln == max_yr_ln & yr.num == max_yr_num_tied & same_river == TRUE),
      
      # cluster centroid
      cluster_centroid_lat = mean(latitude, na.rm = TRUE),
      cluster_centroid_lon = mean(longitude, na.rm = TRUE),
      dist_to_centroid = sqrt(
        (latitude  - cluster_centroid_lat)^2 +
          (longitude - cluster_centroid_lon)^2
      ),
      
      # min distance among tied-top sites only
      min_dist_tied = min(
        if_else(yr.ln == max_yr_ln & yr.num == max_yr_num_tied,
                dist_to_centroid,
                Inf),
        na.rm = TRUE),
        
      #how many sites are essentially at the same min distance? 
      n_min_dist = sum(yr.ln == max_yr_ln & yr.num == max_yr_num_tied  & abs(dist_to_centroid - min_dist_tied) < tol)
      ) %>%
    mutate(
      site_sorting = case_when(
        # 1) different rivers in cluster → keep all
        !same_river ~ "keep",
        
        # 2) same river, any site with strictly shorter time series than the max → remove
        same_river & yr.ln < max_yr_ln ~ "remove",
        
        # 3) same river, unique longest time series (no tie) → keep
        same_river & yr.ln == max_yr_ln & n_max_ln == 1 ~ "keep",
        
        #4) same river, tied for longest time series, but less number of sampling years - remove
        same_river & yr.ln == max_yr_ln & yr.num < max_yr_num_tied ~ "remove",
    
        
        #5) where they are tied on all 3, need to check manually and make decision 
        n_tied >=2 ~ "tied - check manually", 
        
        ##6) same river, tied for longest time series, but most number of sampling years - keep - order is important here, don't want to keep all the ones that are tied, so this comes after the tie indication
        same_river & yr.ln == max_yr_ln & yr.num == max_yr_num_tied ~ "keep",
        
        
        # anything weird left over
      #  TRUE ~ "tied - check manually"
      )
    )
}




##Starting with 100m set - 168 observations, 79 clusters/groups (most have 2 sites per cluster, 8 have 3 sites per cluster, and 1 has 4 sites in cluster)


within_100m_sorted <- within_100m_clusters_dedup %>%
  group_split(dataset.id, group) %>%
  map_dfr(sorting_function, tol = 1e-3) %>%
  ungroup()
  
  
tied_100m <- within_100m_sorted %>%
  filter(n_tied >= 2) %>%
  select(site_sorting, group:n_min_dist)

num_groups_tied_100m <- tied_100m %>%
  select(dataset.id, group) %>%
  distinct()
##14 groups

within_100m_removed <- within_100m_sorted %>%
  filter(site_sorting == "remove")

within_1km_sorted <- within_1km_clusters_dedup %>%
  group_split(dataset.id, group) %>%
  map_dfr(sorting_function) %>%
  ungroup()

tied_1km <- within_1km_sorted %>%
  filter(n_tied >= 2) %>%
  select(site_sorting, group:min_dist_tied)

num_groups_tied_1km <- tied_1km %>%
  select(dataset.id, group) %>%
  distinct()
##55 groups tied 

within_1km_removed <- within_1km_sorted %>%
  filter(site_sorting == "remove")

##Combine the sorted lists for both 100m and 1km 
sites_sorted <- rbind(within_100m_sorted, within_1km_sorted)

sites_removed <- sites_sorted %>%
  filter(site_sorting != "keep")




##lets just look at the ones that are tied/same distance 
test_coords <- tied %>%
  filter(unique.id %in% c("ENG_062_MZB_LO_1211", "ENG_062_MZB_LO_1382")) %>%
  select(!members) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapview(test_coords, map.types = "Esri.WorldTopoMap",legend = TRUE,   alpha = 1, alpha.regions = 1, cex = 4)

str(test_coords)

head(tied)




head(tied)
  
  
  
  group_by(dataset.id, group) %>%
  mutate(
    # do all sites share the same river? (ignore NAs)
    same_river = n_distinct(na.omit(river)) <= 1,
    
    max_yr_ln  = max(yr.ln, na.rm = TRUE),
    n_max_ln   = sum(yr.ln == max_yr_ln),
    
    # among sites with max yr.ln, what is the max # years?
    max_yr_num_tied = max(
      if_else(yr.ln == max_yr_ln, yr.num, NA_real_),
      na.rm = TRUE
    ),
    
    # how many sites are tied on BOTH metrics?
    n_tied_both = sum(yr.ln == max_yr_ln & yr.num == max_yr_num_tied),
    
    # cluster centroid
    cluster_centroid_lat = mean(latitude, na.rm = TRUE),
    cluster_centroid_lon = mean(longitude, na.rm = TRUE),
    dist_to_centroid = sqrt(
      (latitude  - cluster_centroid_lat)^2 +
        (longitude - cluster_centroid_lon)^2
    ),
    
    # min distance among tied-top sites only
    min_dist_tied = min(
      if_else(yr.ln == max_yr_ln & yr.num == max_yr_num_tied,
              dist_to_centroid,
              Inf),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    site_sorting = case_when(
      # 1) different rivers in cluster → keep all
      !same_river ~ "keep",
      
      # 2) same river, strictly shorter time series → remove
      same_river & yr.ln < max_yr_ln ~ "remove",
      
      # 3) same river, unique longest time series (no tie) → keep
      same_river & yr.ln == max_yr_ln & n_max_ln == 1 ~ "keep",
      
      # 4) same river, tied on length & years: use centroid to break tie
      same_river & yr.ln == max_yr_ln & yr.num == max_yr_num_tied &
        dist_to_centroid == min_dist_tied ~ "keep",
      
      same_river & yr.ln == max_yr_ln & yr.num == max_yr_num_tied &
        dist_to_centroid > min_dist_tied ~ "remove",
      
      # anything weird left over
      TRUE ~ "tied - check manually"
    )
  ) %>%
  ungroup() 

##join both together
sites_sorted <- rbind(within_1000m_sorted, within_100m_sorted)

conflicted_sites <- sites_sorted %>%
  group_by(unique.id) %>%
  summarise(
    n_states = n_distinct(site_sorting),
    states   = paste(sort(unique(site_sorting)), collapse = ", ")
  ) %>%
  filter(n_states > 1)


##sites to check manually
manual_check_sites <- sites_sorted %>%
  filter(site_sorting == "tied - check manually")
##38 



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