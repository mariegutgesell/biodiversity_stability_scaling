###---Load libraries
library(sf)
library(geosphere)
library(Hmisc)
library(raster)

###---Load the site list from the master table
sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
sites<-subset(sites, sites$Country=="Spain" & sites$ecosystem=="lotic") #filter to desired ecosystem and country

###---Load the site list for the new dataset
dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

###---Create two new datasets with just the relevant country, coordinate, river, and year information
colnames(sites)
sites2<-sites[,c("Country","Unique.ID","River.lake","Latitude_Y","Longitude_X","Starting_year","Ending_year","Sampling_years","Year_count","Dataset.ID","Data_owner")]
dat2<-data.frame("Country"="Spain", "Unique.ID"=dat$site, "River.lake"=dat$river, "Latitude_Y"=dat$wgs_lat, "Longitude_X"=dat$wgs_long, "Starting_year"=dat$st.yr, "Ending_year"=dat$ed.yr, "Sampling_years"=dat$num.yr, "Year_count"=dat$diff.yr)

#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES AT 100M
#
#######################################################################################################

among.100m<-data.frame("Group"=NA,"Dataset.ID"=NA,"Data_owner"=NA,"Unique.ID"=NA,"River.lake"=NA,"Latitude_Y"=NA,"Longitude_X"=NA,"Starting_year"=NA,"Ending_year"=NA,"Sampling_years"=NA,"Year_count"=NA,"Distance"=NA)

##--among 100m
wgs84<-st_as_sf(dat2, coords = c(5,4), crs = 4326)
wgs84.1<-st_as_sf(sites2, coords = c(5,4), crs = 4326)
etrs89<-st_transform(wgs84, 3034)
etrs89.1<-st_transform(wgs84.1, 3034)
coords<-as.data.frame(st_coordinates(etrs89))
coords.1<-as.data.frame(st_coordinates(etrs89.1))

count<-2
group<-1
for (j in 1:length(dat2$Unique.ID)) {
  dists<-as.vector(pointDistance(p1=coords[j,], p2=coords.1, lonlat=FALSE))
  nums<-which(dists>0 & dists<=100)
  
  if (length(nums)>0) {
    nums2<-c(count:(count+(length(nums))))
    
    among.100m<-rbind(among.100m, among.100m[rep(1, times=(length(nums2))),])
    
    among.100m$Group[nums2]<-group
    among.100m$Dataset.ID[nums2[1]]<-NA
    among.100m$Dataset.ID[nums2[2:length(nums2)]]<-sites2$Dataset.ID[nums]
    among.100m$Data_owner[nums2[1]]<-NA
    among.100m$Data_owner[nums2[2:length(nums2)]]<-sites2$Data_owner[nums]
    among.100m$Unique.ID[nums2[1]]<-dat2$Unique.ID[j]
    among.100m$Unique.ID[nums2[2:length(nums2)]]<-sites2$Unique.ID[nums]
    among.100m$River.lake[nums2[1]]<-dat2$River.lake[j]
    among.100m$River.lake[nums2[2:length(nums2)]]<-sites2$River.lake[nums]
    among.100m$Latitude_Y[nums2[1]]<-dat2$Latitude_Y[j]
    among.100m$Latitude_Y[nums2[2:length(nums2)]]<-sites2$Latitude_Y[nums]
    among.100m$Longitude_X[nums2[1]]<-dat2$Longitude_X[j]
    among.100m$Longitude_X[nums2[2:length(nums2)]]<-sites2$Longitude_X[nums]
    among.100m$Starting_year[nums2[1]]<-dat2$Starting_year[j]
    among.100m$Starting_year[nums2[2:length(nums2)]]<-sites2$Starting_year[nums]
    among.100m$Ending_year[nums2[1]]<-dat2$Ending_year[j]
    among.100m$Ending_year[nums2[2:length(nums2)]]<-sites2$Ending_year[nums]
    among.100m$Sampling_years[nums2[1]]<-dat2$Sampling_years[j]
    among.100m$Sampling_years[nums2[2:length(nums2)]]<-sites2$Sampling_years[nums]
    among.100m$Year_count[nums2[1]]<-dat2$Year_count[j]
    among.100m$Year_count[nums2[2:length(nums2)]]<-sites2$Year_count[nums]
    among.100m$Distance[nums2[1]]<-0
    among.100m$Distance[nums2[2:length(nums2)]]<-round(dists[nums],4)
    
    group<-group+1
    count<-count+length(nums2)
  }
}
among.100m<-among.100m[-1,]

###---Remove mirrored groups
among.100m.2<-among.100m[1,]

sub<-among.100m
groups<-0
for (j in 1:length(unique(sub$Group))) {
  sub1<-subset(sub, sub$Group==unique(sub$Group)[j])
  if (all(groups %nin% unique(sub$Group)[j])) {
    among.100m.2<-rbind(among.100m.2, sub1)
    groups<-append(groups, unique(sub$Group)[j])
  }
  sub2<-subset(sub, sub$Group!=unique(sub$Group)[j])
  for (k in 1:length(unique(sub2$Group))) {
    sub3<-subset(sub2, sub2$Group==unique(sub2$Group)[k])
    if (all(sub3$Unique.ID %in% sub1$Unique.ID)) {
      groups<-append(groups, unique(sub2$Group)[k])
    }
  }
}
among.100m.2<-among.100m.2[-1,]

write.csv(among.100m.2, "Individual - Among less than 100m.csv", row.names=FALSE, fileEncoding = "latin1")

#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES AT 1KM
#
#######################################################################################################

among.1km<-data.frame("Group"=NA,"Dataset.ID"=NA,"Data_owner"=NA,"Unique.ID"=NA,"River.lake"=NA,"Latitude_Y"=NA,"Longitude_X"=NA,"Starting_year"=NA,"Ending_year"=NA,"Sampling_years"=NA,"Year_count"=NA,"Distance"=NA)

count<-2
group<-1
for (j in 1:length(dat2$Unique.ID)) {
  dists<-as.vector(pointDistance(p1=coords[j,], p2=coords.1, lonlat=FALSE))
  nums<-which(dists>100 & dists<=1000)
  
  if (length(nums)>0) {
    nums2<-c(count:(count+(length(nums))))
    
    among.1km<-rbind(among.1km, among.1km[rep(1, times=(length(nums2))),])
    
    among.1km$Group[nums2]<-group
    among.1km$Dataset.ID[nums2[1]]<-NA
    among.1km$Dataset.ID[nums2[2:length(nums2)]]<-sites2$Dataset.ID[nums]
    among.1km$Data_owner[nums2[1]]<-NA
    among.1km$Data_owner[nums2[2:length(nums2)]]<-sites2$Data_owner[nums]
    among.1km$Unique.ID[nums2[1]]<-dat2$Unique.ID[j]
    among.1km$Unique.ID[nums2[2:length(nums2)]]<-sites2$Unique.ID[nums]
    among.1km$River.lake[nums2[1]]<-dat2$River.lake[j]
    among.1km$River.lake[nums2[2:length(nums2)]]<-sites2$River.lake[nums]
    among.1km$Latitude_Y[nums2[1]]<-dat2$Latitude_Y[j]
    among.1km$Latitude_Y[nums2[2:length(nums2)]]<-sites2$Latitude_Y[nums]
    among.1km$Longitude_X[nums2[1]]<-dat2$Longitude_X[j]
    among.1km$Longitude_X[nums2[2:length(nums2)]]<-sites2$Longitude_X[nums]
    among.1km$Starting_year[nums2[1]]<-dat2$Starting_year[j]
    among.1km$Starting_year[nums2[2:length(nums2)]]<-sites2$Starting_year[nums]
    among.1km$Ending_year[nums2[1]]<-dat2$Ending_year[j]
    among.1km$Ending_year[nums2[2:length(nums2)]]<-sites2$Ending_year[nums]
    among.1km$Sampling_years[nums2[1]]<-dat2$Sampling_years[j]
    among.1km$Sampling_years[nums2[2:length(nums2)]]<-sites2$Sampling_years[nums]
    among.1km$Year_count[nums2[1]]<-dat2$Year_count[j]
    among.1km$Year_count[nums2[2:length(nums2)]]<-sites2$Year_count[nums]
    among.1km$Distance[nums2[1]]<-0
    among.1km$Distance[nums2[2:length(nums2)]]<-round(dists[nums],4)
    
    group<-group+1
    count<-count+length(nums2)
  }
}
among.1km<-among.1km[-1,]

###---Remove mirrored groups
among.1km.2<-among.1km[1,]

sub<-among.1km
groups<-0
for (j in 1:length(unique(sub$Group))) {
  sub1<-subset(sub, sub$Group==unique(sub$Group)[j])
  if (all(groups %nin% unique(sub$Group)[j])) {
    among.1km.2<-rbind(among.1km.2, sub1)
    groups<-append(groups, unique(sub$Group)[j])
  }
  sub2<-subset(sub, sub$Group!=unique(sub$Group)[j])
  for (k in 1:length(unique(sub2$Group))) {
    sub3<-subset(sub2, sub2$Group==unique(sub2$Group)[k])
    if (all(sub3$Unique.ID %in% sub1$Unique.ID)) {
      groups<-append(groups, unique(sub2$Group)[k])
    }
  }
}
among.1km.2<-among.1km.2[-1,]

write.csv(among.1km.2, "Individual - Among 100m-1km.csv", row.names=FALSE, fileEncoding = "latin1")

###---Remove sites
dat3<-subset(dat2, dat2$Unique.ID %nin% among.100m.2$Unique.ID) #remove all among 100m
dat3<-subset(dat3, dat3$Unique.ID %nin% c(5099,221439,221428,221425,221358,221357,221380,221375,221374,221372,221395)) #remove these among 1km

write.csv(dat3, "Selected sites after overlap check.csv", row.names=FALSE, fileEncoding = "latin1")
