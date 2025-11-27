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
sites_mg <- read.csv("data/processed/step2_site_ids/Step2_MZB_sites_lotic.csv",   colClasses = c(Provider.Number = "character"))  %>%
  mutate(Latitude_Y = as.numeric(Latitude_Y),  Longitude_X = as.numeric(Longitude_X)) %>% ##ensure lat and long are numeric
  filter(fulfills.requirement == "yes") ##select sites that fulfill requirements - this is based on dataset criteria listed in data call 

##NOTE: as of Nov 24, still have the issue of the duplicates from the two files (see issue #2 in step 2 code, otherwise all other data matches james)

#######################################################################################################
#
#                                           FUNCTIONS
#
#######################################################################################################
##Function to identify clusters (function logic: assigns group number to cluster of sites within defined distance)
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

##Function to filter redundant clusters (function logic: sorts clusters from largest to smallest, starts with the larger clusters, for each cluster checks all smaller clusters. If a smaller cluster’s members are entirely contained within a larger one, the smaller cluster is marked as redundant)
#Returns only the unique, non-subset clusters. (note: sites can still occur in multiple clusters if not full subsets of eachother)
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

##Function to sort sites into keep/remove/tie-breaker 
##Logic for keeping/removing sites from a group within a dataset - applied to each river within a group 
##1) if all river names are different - keep all 
##2) When river names are the same (or blank), keep longest time series
##3) When river names are the same (or blank) and time series length is the same, keep the one with the most sampling years
##4) When river names are the same (or blank) and time series length is the same and number of sampling years is the same, tied - check manually
sorting_function <- function(df) {
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

##Function to plot tied sites to manually check tie breakers 
plot_tie_group <- function(tied_df, dataset_id, group_id, within_distance_type = NULL,map_type = "OpenTopoMap") {
  df <- tied_df %>%
    filter(
      dataset.id == dataset_id,
      group == group_id
    )
  
  if (!is.null(within_distance_type)) {
    df <- df %>%
      filter(within_distance_type == !!within_distance_type)
  }
  
  df_sf <- df %>%
    select(!members) %>%
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

#Function to plot all sites in any conflict group containing a given unique.id
plot_conflict_map <- function(df, target_unique_id, groups = NULL, map_type = "OpenTopoMap") {
  
  df_conflict <- df %>%
    filter(map_lgl(members, ~ any(str_detect(.x, fixed(target_unique_id))) )) %>%
    select(!members) %>%
    # if groups provided, filter to those; otherwise leave as-is
    { 
      if (!is.null(groups)) {
        filter(., group %in% groups)
      } else {
        .
      }
    } %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  mapview(df_conflict, map.types = map_type, legend  = TRUE, zcol = "unique.id", alpha= 1, alpha.regions = 1, cex  = 4
  )
}

#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES WITHIN DATASETS
#
#######################################################################################################

##Identify clusters within 100m and filter out redundant clusters ------------------
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

####Identify clusters within 100m - 1km and filter out redundant clusters ------------------
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


##Count number of clusters and number of sites within each cluster 
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



##Save csv of site clusters
#write.csv(within_100m_clusters_dedup, "data/data_processing/Step3_lotic_MZB_within_less_100m.csv")
#write.csv(within_1km_clusters_dedup, "data/data_processing/Step3_lotic_MZB_within_100m_1000m.csv")


#######################################################################################################
#
#                          SORT SITES TO KEEP AND REMOVE OR CHECK MANUALLY
#
#######################################################################################################


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
#                          RESOLVE TIED SITES
#
#######################################################################################################

##1) Read in tiebreaker lookup table 
tiebreaker_lookup <- read.csv("data/processed/lookups/Step3_lookup_tiebreaker_state.csv")

tiebreaker_lookup %>%
  count(unique.id) %>%
  filter(n > 1)
##check to ensure only one replicate of unique id in lookup table 
stopifnot(
  nrow(
    tiebreaker_lookup %>%
      count(unique.id) %>%
      filter(n > 1)
  ) == 0
)

##2) check for any new ties that currently do not exist in the lookup table 
# all currently tied sites in this run
ties_current <- sites_sorted %>%
  filter(site_sorting == "tied - check manually") %>%
  distinct(unique.id, dataset.id, group, within_distance_type)

# which of these tied sites are *not* in your lookup table?
new_ties <- ties_current %>%
  anti_join(tiebreaker_lookup, by = "unique.id")

##if there are new ties, this creates a csv listing which sites need a manual decision (i.e., new ties)
if (nrow(new_ties) > 0) {
  readr::write_csv(
    new_ties,
    "data/processed/lookups/Step3_new_ties_needing_tiebreaker.csv"
  )
  
  stop(
    glue::glue(
      "There are {nrow(new_ties)} tied sites without tiebreaker decisions.\n",
      "See data/processed/lookups/Step3_new_ties_needing_tiebreaker.csv and update Step3_lookup_tiebreaker_state.csv."
    )
  )
}
##For any new ties --> go to below to resolve tiebreaker section, inspect sites, make decision and add manually to lookup csv, then come back and re-read in tiebreaker lookup table 

##3) Sort tie breakers 
sites_sorted_tiebreaker <- sites_sorted %>%
  filter(site_sorting == "tied - check manually") %>%
  left_join(tiebreaker_lookup, by = "unique.id") 

##Check if any rows still have a missing resolution 
still_missing <- sites_sorted_tiebreaker %>%
  filter(site_sorting == "tied - check manually",
         is.na(tiebreaker_state))

stopifnot(nrow(still_missing) == 0)


##filter out not tied sites
sites_sorted_notie <- sites_sorted %>%
  filter(site_sorting %in% c("keep", "remove")) %>%
  mutate(tiebreaker_state = site_sorting)

##then join back together -- splitting this here to ensure that tiebreaker doesnt override any preivous states so if there is a conflict can manually sort
sites_sorted_tiebreaker <- rbind(sites_sorted_tiebreaker, sites_sorted_notie)

##double check there are no NAs
test <- sites_sorted_tiebreaker %>%
  filter(is.na(tiebreaker_state))

#######################################################################################################
#
#                          RESOLVE CONFLICTED
#
#######################################################################################################

##1) Read in conflict resolution look up table
##read in conflict resolution lookup table 
conflict_overrides_lookup <- read.csv("data/processed/lookups/Step3_lookup_conflict_state_overrides.csv")

##2) ##Check if any unique.ids have conflicting sorting - i.e., in some pairs are listed as keep and others as remove
conflicted_ids_current <- sites_sorted_tiebreaker %>%
  group_by(unique.id) %>%
  summarise(
    n_states = n_distinct(tiebreaker_state),
    states   = paste(sort(unique(tiebreaker_state)), collapse = ", ")
  ) %>%
  filter(n_states > 1) 

# which of these conflicted sites are *not* in your lookup table?
new_conflicts <- conflicted_ids_current %>%
  anti_join(conflict_overrides_lookup, by = "unique.id")

##if there are new ties, this creates a csv listing which sites need a manual decision (i.e., new ties)
if (nrow(new_conflicts) > 0) {
  readr::write_csv(
    new_conflicts,
    "data/processed/lookups/Step3_new_conflicts_needing_resolution.csv"
  )
  
  stop(
    glue::glue(
      "There are {nrow(new_conflicts)} sites with conflicting states without resolution decisions.\n",
      "See data/processed/lookups/Step3_new_conflicts_needing_resolution.csv and update Step3_lookup_conflict_state_overrides.csv."
    )
  )
}

##3) Join the conflict resolutions to the df to create final sorted df 

sites_sorted_final <- sites_sorted_tiebreaker %>%
  left_join(conflict_overrides_lookup, by = "unique.id") %>%
  mutate(final_state = coalesce(final_state, tiebreaker_state)) ##fill in any NAs with the state written in the tiebreaker

#######################################################################################################
#
#                         CREATE DF OF SITES TO REMOVE
#
#######################################################################################################

sites_to_remove_marie <- sites_sorted_final %>%
  filter(final_state == "remove")


num_sites_removed_marie <- sites_to_remove_marie %>%
  select(unique.id) %>%
  distinct()

#######################################################################################################
#
#                          MANUALLY CHECK TIED SITES 
#           **REMEMBER TO UPDATE TIEBREAKER LOOKUP TABLE AFTER MANUALLY CHECKING**
#
#######################################################################################################
##General tie breaker rules: if on same reach, keep site id that james kept (if in his list) or the one with lower site number (random but consistent - if not on his list), if 3 sites on same reach, keep one in middle, if on different tributaries - keep 
##Checking, dataset IDs in alphabetical order
##1) Read in df of sites of new ties that need to be manually checked 
tied <- read.csv("data/processed/lookups/Step3_new_ties_needing_tiebreaker.csv")

num_groups_tied <- tied %>%
  select(dataset.id, within_distance_type, group) %>%
  distinct()


##2) FOR ANY NEW TIES, CHECK MANUALLY WITH PLOTTING, MAKE NOTE ABOUT DECISION AND THEN UPDATE LOOKUP TABLE 
##LOOKUP TABLE: data/processed/lookups/Step3_lookup_tiebreaker_state.csv

##NOTE: If map is grey when first run function, just need to zoom out ####


## 	CHE_040_MZB_LO, group 1 ------
plot_tie_group(tied, "CHE_040_MZB_LO", 1)
##different tributaries (i think) - keep both - same as james 

## 	CHE_076_MZB_LO, group 3 ------
plot_tie_group(tied, "CHE_076_MZB_LO", 3)
##3 rivers names, just keep 1 from each, selecting lowest site code - some same as james, but keeping  at least and only one site from each unique river name

## 	DNK_011_MZB_LO, group 5 ------
plot_tie_group(tied, "DNK_011_MZB_LO", 5)
##different tributaries, keep both - same as james

## 	ENG_041_MZB_LO, group 3 ------
plot_tie_group(tied, "ENG_041_MZB_LO", 3)
##on same channel, only keep 1 - lower site # (not in james list)

## 	ENG_062_MZB_LO, group 15 ------
plot_tie_group(tied, "ENG_062_MZB_LO", 15)
##same river name, remove 1 - same as james (james removes 1211)

## 	ENG_062_MZB_LO, group 50 ------
plot_tie_group(tied, "ENG_062_MZB_LO", 50)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 61 ------
plot_tie_group(tied, "ENG_062_MZB_LO", 61)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 328 ------
plot_tie_group(tied, "ENG_062_MZB_LO", 328)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 330 ------
plot_tie_group(tied, "ENG_062_MZB_LO", 330)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 334------
plot_tie_group(tied, "ENG_062_MZB_LO", 334)
##look like are on same channel, only keep 1 (not in james list)

## 	ENG_062_MZB_LO, group 340------
plot_tie_group(tied, "ENG_062_MZB_LO", 340)
##look like are on same channel, only keep 1 - same as james 

## 	ENG_062_MZB_LO, group 452------
plot_tie_group(tied, "ENG_062_MZB_LO", 452)
##2 of the same sites as in group 334, already sorted. third group has much shorter time series, so is removed 

## 	ENG_062_MZB_LO, group 503------
plot_tie_group(tied, "ENG_062_MZB_LO", 503)
##on same reach, only keep 1 site - same as james 

## 	ENG_062_MZB_LO, group 544------
plot_tie_group(tied, "ENG_062_MZB_LO", 544)
##on same reach, only keep 1 site - same as james 

## 	ENG_062_MZB_LO, group 610------
plot_tie_group(tied, "ENG_062_MZB_LO", 610)
##on same reach, only keep 1 site - same as james 

## 	ENG_062_MZB_LO, group 672------
plot_tie_group(tied, "ENG_062_MZB_LO", 672)
##on same reach, only keep 1 site - same site as james 

## 	ENG_062_MZB_LO, group 683------
plot_tie_group(tied, "ENG_062_MZB_LO", 683)
##all 3 on same reach, keep the two are the furthest extremes, which are more than 1 km apart - this is to match james 

## 	ENG_062_MZB_LO, group 690------
plot_tie_group(tied, "ENG_062_MZB_LO", 690)
##on same reach, keep 1 - same as james 

## 	ENG_062_MZB_LO, group 694------
plot_tie_group(tied, "ENG_062_MZB_LO", 694)
##on same reach, keep 1 (not in james list)

## 	ENG_062_MZB_LO, group 695------
plot_tie_group(tied, "ENG_062_MZB_LO", 695)
##on two different rivers, within each river, two sites on same reach, just keep one from each - same ones as james 

## 	ENG_062_MZB_LO, group 713------
plot_tie_group(tied, "ENG_062_MZB_LO", 713)
##looks like all three are on the same reach (a little hard to tell) - two edge ones - this is to match james

## 	ESP_034_MZB_LO, group 19------
plot_tie_group(tied, "ESP_034_MZB_LO", 19)
##all 4 sites are on same reach, 2 of 4 are already removed due to short time series, keep only one of the two tied sites - keep one that doesn't lead to conflicted sorting (so is different than james) 

## 	ESP_060_MZB_LO, group 4------
plot_tie_group(tied, "ESP_060_MZB_LO", 4)
##same reach, keep 1 (not in james list)

## 	FIN_004_MZB_LO, group 9------
plot_tie_group(tied, "FIN_004_MZB_LO", 9)
##3 sites on same reach, keep same one as james 

## 	FRA_015_MZB_LO, group 1------
plot_tie_group(tied, "FRA_015_MZB_LO", 1)
##same reach and river, only keep 1 - same one as james

## 	GER_017_MZB_LO, group 1 ------
plot_tie_group(tied, "GER_017_MZB_LO", 1)
##same reach and river, only keep 1 (not in james list)

## 	GER_018_MZB_LO, group 3------
plot_tie_group(tied, "GER_018_MZB_LO", 3)
##same reach and river, only keep 1 (not in james list)

## 	GER_018_MZB_LO, group 5------
plot_tie_group(tied, "GER_018_MZB_LO", 5)
##same reach and river, only keep 1 (not in james list)

## 	GER_047_MZB_LO, group 2------
plot_tie_group(tied, "GER_047_MZB_LO", 2)
##sites are identical, just keep 1 (not in james list)

## 	GER_047_MZB_LO, group 3------
plot_tie_group(tied, "GER_047_MZB_LO", 3)
##sites are identical, just keep 1 (not in james list)

## 	GER_070_MZB_LO, group 27-----
plot_tie_group(tied, "GER_070_MZB_LO", 27)
##same reach, just keep 1 (not on james list)

## 	GER_070_MZB_LO, group 34-----
plot_tie_group(tied, "GER_070_MZB_LO", 34)
##all 3 on same reach,  - note overlap with group 35 (b/c two farthest sites are more than 1 km, but all are on the same reach, - keep the two farthest apart, gives some match then to one site james removed

## 	GER_070_MZB_LO, group 35-----
plot_tie_group(tied, "GER_070_MZB_LO", 35)
##all 4 on same reach, - also there is overlap here with group 34, keeping the two sites that are farthest apart (141 and 143)- 

## 	GER_070_MZB_LO, group 56-----
plot_tie_group(tied, "GER_070_MZB_LO", 56)
##on same reach, only keep 1 (not in james list)

## 	GER_071_MZB_LO, group 2-----
plot_tie_group(tied, "GER_071_MZB_LO", 2)
##4 sites on the same reach, keep two furthest outside - this is to match james

## 	GER_071_MZB_LO, group 5-----
plot_tie_group(tied, "GER_071_MZB_LO", 5)
##2 sites on same reach, keep only 1 - same one as james

## 	GER_071_MZB_LO, group 9-----
plot_tie_group(tied, "GER_071_MZB_LO", 9)
##3 sites on same reach, keep only 1 - one is already removed due to fewer number of years, 
##also overlap with sites in group 10, so going to keep the two farthest part sites (9 and 10) to get max number of sites, even though 10 has 1 less year of samplng

## 	GER_072_MZB_LO, group 1 (within 100m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 1, within_distance_type = "within_100m")
##both on same reach, keep only 1 

## 	GER_072_MZB_LO, group 3 (within 100m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 3, within_distance_type = "within_100m")
##both on same reach, keep only 1 

## 	GER_072_MZB_LO, group 5-----
plot_tie_group(tied, "GER_072_MZB_LO", 5, within_distance_type = "within_100m")
##both on same reach, keep only 1 

## 	GER_072_MZB_LO, group 1 (100m-1000m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 1, within_distance_type = "within_100m_to_1000m")
##not entirely clear if on same reach, one looks like its not on any stream, and so could be on main reach or side trib, but likely on same, so keep 1 

## 	GER_072_MZB_LO, group 3 (100m-1000m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 3, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1 

## 	GER_072_MZB_LO, group 5 (100m-1000m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 5, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1 

## 	GER_072_MZB_LO, group 7 (100m-1000m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 7, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1

## 	GER_072_MZB_LO, group 9 (100m-1000m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 9, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1 

## 	GER_072_MZB_LO, group 11 (100m-1000m)-----
plot_tie_group(tied, "GER_072_MZB_LO", 11, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1 

## 	GER_073_MZB_LO, group 1-----
plot_tie_group(tied, "GER_073_MZB_LO", 1, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1 - removing same one as james

## 	GER_073_MZB_LO, group 5-----
plot_tie_group(tied, "GER_073_MZB_LO", 5, within_distance_type = "within_100m_to_1000m")
##on same branch, keep 1 - removing same one as james

## 	HUN_022_MZB_LO, group 1-----
plot_tie_group(tied, "HUN_022_MZB_LO", 1)
##same point - removing same one as james

## 	IRL_023_MZB_LO, group 1-----
plot_tie_group(tied, "IRL_023_MZB_LO", 1)
##points not exactly on stream, but if snapped to closest river then would be on same reach - remove 1, same one as james 

## 	IRL_023_MZB_LO, group 3-----
plot_tie_group(tied, "IRL_023_MZB_LO", 3)
##on same reach, remove 1 - removing same one as james 

## 	IRL_051_MZB_LO, group 9-----
plot_tie_group(tied, "IRL_051_MZB_LO", 9)
##same point, remove 1 - removing same one as james 

## 	IRL_051_MZB_LO, group 3-----
plot_tie_group(tied, "IRL_051_MZB_LO", 3)
##on same reach, remove 1 - 

## 	IRL_051_MZB_LO, group 41-----
plot_tie_group(tied, "IRL_051_MZB_LO", 41)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 43-----
plot_tie_group(tied, "IRL_051_MZB_LO", 43)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 61-----
plot_tie_group(tied, "IRL_051_MZB_LO", 61)
##all 3 on same reach, 2 are identical remove 2  - 1 same as james, and 1 more, keep lowest site number

## 	IRL_051_MZB_LO, group 71-----
plot_tie_group(tied, "IRL_051_MZB_LO", 71)
##hard to tell where stream is, but remove 1 - same as james 

## 	IRL_051_MZB_LO, group 128-----
plot_tie_group(tied, "IRL_051_MZB_LO", 128)
##hard to tell where stream is, but remove 1 - same as james 

## 	IRL_051_MZB_LO, group 132-----
plot_tie_group(tied, "IRL_051_MZB_LO", 132)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 157-----
plot_tie_group(tied, "IRL_051_MZB_LO", 157)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 190-----
plot_tie_group(tied, "IRL_051_MZB_LO", 190)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 204-----
plot_tie_group(tied, "IRL_051_MZB_LO", 204)
##on same reach, remove 1 - same as james 

## 	IRL_051_MZB_LO, group 241-----
plot_tie_group(tied, "IRL_051_MZB_LO", 241)
##on same reach, remove 1 - same as james 

## 	NOR_054_MZB_LO, group 2 (100m)-----
plot_tie_group(tied, "NOR_054_MZB_LO", 2, within_distance_type = "within_100m")
##3 sites, keep 1 - same as james 

## 	NOR_054_MZB_LO, group 6 (100m)-----
plot_tie_group(tied, "NOR_054_MZB_LO", 6, within_distance_type = "within_100m")
##3 sites, keep 1 - same as james 

## 	NOR_054_MZB_LO, group 9-----
plot_tie_group(tied, "NOR_054_MZB_LO", 9, within_distance_type = "within_100m")
##on different tribs into the lake, keep both 

## 	NOR_054_MZB_LO, group 11-----
plot_tie_group(tied, "NOR_054_MZB_LO", 11, within_distance_type = "within_100m")
##same site, remove 1  - same as james  

## 	NOR_054_MZB_LO, group 13-----
plot_tie_group(tied, "NOR_054_MZB_LO", 13, within_distance_type = "within_100m")
##same reach, remove 1  - same as james 

## 	NOR_054_MZB_LO, group 3-----
plot_tie_group(tied, "NOR_054_MZB_LO", 3, within_distance_type = "within_100m_to_1000m")
##same sites that are also part of within 100m group 6, already dealt with 

## 	NOR_054_MZB_LO, group 5-----
plot_tie_group(tied, "NOR_054_MZB_LO", 5, within_distance_type = "within_100m_to_1000m")
##same sites that are also part of within 100m group 6, and 100-1000m group 5, already dealt with 

## 	NOR_054_MZB_LO, group 11-----
plot_tie_group(tied, "NOR_054_MZB_LO", 11, within_distance_type = "within_100m_to_1000m")
##different streams, keep both 

## 	NOR_054_MZB_LO, group 13-----
plot_tie_group(tied, "NOR_054_MZB_LO", 13, within_distance_type = "within_100m_to_1000m")
##site 55 and 56 are on different tribs, site 57 on same trib as 55, so remove that one (same as james)

## 	NOR_054_MZB_LO, group 17-----
plot_tie_group(tied, "NOR_054_MZB_LO", 17, within_distance_type = "within_100m_to_1000m")
##sites on different tribs into the lake, keep both 

## 	NOR_054_MZB_LO, group 18-----
plot_tie_group(tied, "NOR_054_MZB_LO", 18, within_distance_type = "within_100m_to_1000m")
##sites on different tribs into different lakes, keep both 

## 	NOR_054_MZB_LO, group 21-----
plot_tie_group(tied, "NOR_054_MZB_LO", 21, within_distance_type = "within_100m_to_1000m")
##sites on main stem and tributary, keep both 

## 	NOR_054_MZB_LO, group 25-----
plot_tie_group(tied, "NOR_054_MZB_LO", 25, within_distance_type = "within_100m_to_1000m")
##sites both on same reach, remove 1 - same as james 

## 	NOR_054_MZB_LO, group 27-----
plot_tie_group(tied, "NOR_054_MZB_LO", 27, within_distance_type = "within_100m_to_1000m")
##sites both on same reach, remove 1 - same as james 


#######################################################################################################
#
#                          MANUALLY CHECK CONFLICTING SITE SORTING
#    **REMEMBER TO UPDATE CONFLICT STATE LOOKUP TABLE ONCE NEW CONFLICTS MANUALLY CHECKED**
#
#######################################################################################################

##1) Read in csv of any new conflicts that need to be resolved 
conflicted_ids <- read.csv("data/processed/lookups/Step3_new_conflicts_needing_resolution.csv")

##2) FOR ANY NEW CONFLICTS, CHECK MANUALLY WITH PLOTTING, MAKE NOTE ABOUT DECISION AND THEN UPDATE LOOKUP TABLE 
##LOOKUP TABLE: data/processed/lookups/Step3_lookup_conflict_state_overrides.csv

##Conflicted site: ENG_062_MZB_LO_1113 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_1113")
##only kept because came up in <100m and was a trib and mainstem so different names so kept, but is too close to others on main stem, so remove

##Conflicted site: ENG_062_MZB_LO_1211 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_1211")
##remove  (fixed in tie coding above)

##Conflicted site: ENG_062_MZB_LO_1231 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_1231")
##remove - kept b/c unique stream name in group - shows up in two groups, in one is removed b/c fewer sampling years. remove

##Conflicted site: ENG_062_MZB_LO_1382 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_1382")
##remove - too close to other sites - shows up in 2 groups (same as 1211) (fixed in tie coding above)

##Conflicted site: ENG_062_MZB_LO_2104 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_2104")
##remove, 2 other sites close on main stem, and want to keep the one <100m on other trib

##Conflicted site: ENG_062_MZB_LO_2995 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_2995")
##remove, 2 other sites close on main stem, and want to keep the one <100m on other trib

##Conflicted site: ENG_062_MZB_LO_3206 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_3206")
##remove, shows up in two groups, another close site that has longer time series 

##Conflicted site: ENG_062_MZB_LO_3246 ------
plot_conflict_map(sites_sorted_tiebreaker, "ENG_062_MZB_LO_3246")
##remove, shows up in two groups, only 2 of these sites can be kept - keeping longest time series (resolved in tie breaker)

##Conflicted site: ESP_034_MZB_LO_11 ------
plot_conflict_map(sites_sorted_tiebreaker, "ESP_034_MZB_LO_11")
##keep, remove other tied site to avoid conflict - all was equal had only selected other to be same as james, but clearer reasoning here (fixed in tie coding above)

##Conflicted site: ESP_034_MZB_LO_33 ------
plot_conflict_map(sites_sorted_tiebreaker, "ESP_034_MZB_LO_33", groups = c(16, 42, 73))
##is in 3 groups, kept in one group where it had the longest time series, but in other two groups does not, so remove, but in the group where it was kept (16), keep the site that had second longest time seires (site 366)

##Conflicted site: GER_071_MZB_LO_11 / 10 ------
plot_conflict_map(sites_sorted_tiebreaker, "GER_071_MZB_LO_11")
##going to keep the two sites furthest apart (9 and 10), fix coding of 10 below because originally marked to remove b/c 1 less sampling year. but this way get to keep two sites rather than just 1, and 9 and 10 are more than 1 km apart 

##Conflicted site: IRL_051_MZB_LO_643 ------
plot_conflict_map(sites_sorted_tiebreaker, "IRL_051_MZB_LO_643")
##remove, too close to another site on same stem 

##Conflicted site: NOR_054_MZB_LO_45 / 47 / 48 ------
plot_conflict_map(sites_sorted_tiebreaker, "NOR_054_MZB_LO_45")
##says it is on a different river than 16, but its not - so keep 16 and remove 45 
##remove 47 and 48, only come as keep because different river than 16, but too close to 46, so remove 


##Look into overlapping sites between james and Marie  -----------
##when james has one marked to remove that i dont, i have usually selected the one with longer time series and/or more sampling years. not sure why he has selected the shorter one 
##some sites missing from james that really should be removed (e.g., GER 018, GER 047, GER 070)

##look at dfs made:
lo_sites_less_100m <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Lotic macroinvert - within less than 100m.csv")
lo_sites_within_100m_1km <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Lotic macroinvert - within 101-1000m.csv")

sites_to_be_removed_james <- read.csv("EU MZB LO Dataset processing/Step 3 - Duplicated within datasets/Step 3 - duplicated within removed.csv")


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


##FOR FUTURE WHEN WANT TO WRAP THIS INTO ONE FUNCTION - AND ENSURE APPROPRIATE STOPS
#run_step3 <- function(
#    sites_file,
#    tiebreaker_lookup_file,
#    conflict_lookup_file,
#    out_clusters_100m   = NULL,
#    out_clusters_1km    = NULL,
#    out_sites_to_remove = NULL
#) {
  ## 1) read + prep sites (your sites_mg code)
  ## 2) find clusters (100m + 1km)
  ## 3) sort sites with sorting_function()
  ## 4) check for new ties → write CSV + stop if any
  ## 5) apply tiebreaker_lookup
  ## 6) check for new conflicts → write CSV + stop if any
  ## 7) apply conflict overrides
  ## 8) return list(sites_sorted_final = ..., sites_to_remove = ...) 
  ##    and optionally write CSVs if out_* args are not NULL
#}

##also, then move the manual checking sites to a separate script that you only go to when hit a stop so that this can run in just one uninterrupted script 