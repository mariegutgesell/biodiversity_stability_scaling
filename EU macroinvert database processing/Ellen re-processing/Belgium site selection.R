library(Hmisc)

dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
wrong<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

dat2$be_id<-paste("BEVL_VMM_", dat2$monitoring.site..prefix..BEVL_VMM_., sep="")

dat2<-subset(dat2, dat2$be_id %in% wrong$be_id)
dat2$ellen_id<-NA
dat2$year<-NA
dat2$month<-NA
dat2$day<-NA

sites<-unique(dat2$be_id)
for (i in 1:length(sites)) {
  nums<-which(dat2$be_id==sites[i])
  num<-which(wrong$be_id==sites[i])[1]
  dat2$ellen_id[nums]<-wrong$ellen_id[num]
}

dates<-unique(dat2$Sampling.date)
for (i in 1:length(dates)) {
  nums<-which(dat2$Sampling.date==dates[i])
  name<-strsplit(dat2$Sampling.date[nums[1]], split="/")
  dat2$year[nums]<-as.numeric(name[[1]][3])
  dat2$month[nums]<-as.numeric(name[[1]][2])
  dat2$day[nums]<-as.numeric(name[[1]][1])
}

dat2[which(dat2$ellen_id==121000003 & dat2$year==1999),]

write.csv(dat2, "BE taxalist for sites w incorrect dates.csv", row.names=FALSE, fileEncoding = "latin1")

dat2<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

###---Only 2019 or before
dat2<-subset(dat2, dat2$year<=2019)

###---Need to divide into separate datasets by sampling method?
unique(dat2$sampling.method)
#yes

dat.hand<-subset(dat2, dat2$sampling.method=="Handnet")
dat.art<-subset(dat2, dat2$sampling.method=="Artificial substrate")

dates.hand<-data.frame(sample_id=unique(dat.hand$Sample.ID), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates.hand$sample_id)) {
  num<-which(dat.hand$Sample.ID==dates.hand$sample_id[i])[1]
  dates.hand$site[i]<-dat.hand$Site_ID_Ellen[num]
  dates.hand$year[i]<-dat.hand$year[num]
  dates.hand$month[i]<-dat.hand$month[num]
  dates.hand$day[i]<-dat.hand$day[num]
}

dates.art<-data.frame(sample_id=unique(dat.art$Sample.ID), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates.art$sample_id)) {
  num<-which(dat.art$Sample.ID==dates.art$sample_id[i])[1]
  dates.art$site[i]<-dat.art$Site_ID_Ellen[num]
  dates.art$year[i]<-dat.art$year[num]
  dates.art$month[i]<-dat.art$month[num]
  dates.art$day[i]<-dat.art$day[num]
}

#Windows
#1=1-3, 2=2-4, 3=3-5, 4=4-6, 5=5-7, 6=6-8, 7=7-9, 8=8-10, 9=9-11, 10=10-12
options(warn=2) #1 is default

table(wrong$month)

#Handnet
sites<-unique(dates.hand$site)
site.hand<-numeric()
sort(unique(dates.hand$month))
table(dates.hand$month)

count<-0.1
for (i in 1:length(sites)) {
  sub<-subset(dates.hand, dates.hand$site==sites[i])
  window<-c(1,3)
  win.num<-numeric()
  for (j in 1:10) {
    sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2])
    win.num[j]<-length(unique(sub2$year))
    window<-window+1
  }
  if (any(win.num>7)) {
    wins<-which(win.num>7)
    if (length(wins)>1) {
      if (length(which(wins==7))>0) {
        wins<-7
      } else if (length(which(wins==8))>0) {
        wins<-8
      } else if (length(which(wins==6))>0) {
        wins<-6
      } else if (length(which(wins==5))>0) {
        wins<-5
      } else {
        wins<-which(win.num==max(win.num))[1]
      }
    }
    site.hand[i]<-wins
  } else {
    site.hand[i]<-0
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}
site.hand
table(site.hand)

#Artificial substrates
sites<-unique(dates.art$site)
site.art<-numeric()
sort(unique(dates.art$month))
table(dates.art$month)

count<-0.1
for (i in 1:length(sites)) {
  sub<-subset(dates.art, dates.art$site==sites[i])
  window<-c(1,3)
  win.num<-numeric()
  for (j in 1:10) {
    sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2])
    win.num[j]<-length(unique(sub2$year))
    window<-window+1
  }
  if (any(win.num>7)) {
    wins<-which(win.num>7)
    if (length(wins)>1) {
      if (length(which(wins==7))>0) {
        wins<-7
      } else if (length(which(wins==8))>0) {
        wins<-6
      } else if (length(which(wins==6))>0) {
        wins<-5
      } else if (length(which(wins==5))>0) {
        wins<-8
      } else {
        wins<-which(win.num==max(win.num))[1]
      }
    }
    site.art[i]<-wins
  } else {
    site.art[i]<-0
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}
site.art
table(site.art)


#Handnet
sites<-unique(dates.hand$site)
dat.hand2<-dat.hand[1,]

for (i in 1:length(sites)) {
  if (site.hand[i]>0) {
    if (site.hand[i]==1) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(1,2,3))
    }
    if (site.hand[i]==2) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(2,3,4))
    }
    if (site.hand[i]==3) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(3,4,5))
    }
    if (site.hand[i]==4) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(4,5,6))
    }
    if (site.hand[i]==5) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(5,6,7))
    }
    if (site.hand[i]==6) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(6,7,8))
    }
    if (site.hand[i]==7) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(7,8,9))
    }
    if (site.hand[i]==8) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(8,9,10))
    }
    if (site.hand[i]==9) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(9,10,11))
    }
    if (site.hand[i]==10) {
      sub<-subset(dat.hand, dat.hand$Site_ID_Ellen==sites[i] & dat.hand$month %in% c(10,11,12))
    }
    dat.hand2<-rbind(dat.hand2, sub)
  }
}
dat.hand2<-dat.hand2[-1,]

#Artificial substrates
sites<-unique(dates.art$site)
dat.art2<-dat.art[1,]

for (i in 1:length(sites)) {
  if (site.art[i]>0) {
    if (site.art[i]==1) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(1,2,3))
    }
    if (site.art[i]==2) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(2,3,4))
    }
    if (site.art[i]==3) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(3,4,5))
    }
    if (site.art[i]==4) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(4,5,6))
    }
    if (site.art[i]==5) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(5,6,7))
    }
    if (site.art[i]==6) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(6,7,8))
    }
    if (site.art[i]==7) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(7,8,9))
    }
    if (site.art[i]==8) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(8,9,10))
    }
    if (site.art[i]==9) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(9,10,11))
    }
    if (site.art[i]==10) {
      sub<-subset(dat.art, dat.art$Site_ID_Ellen==sites[i] & dat.art$month %in% c(10,11,12))
    }
    dat.art2<-rbind(dat.art2, sub)
  }
}
dat.art2<-dat.art2[-1,]

###---Get rid of samples collected twice in the same year and season
#Handnet
dat.hand3<-dat.hand2

dates<-data.frame(sample_id=unique(dat.hand3$Sample.ID), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates$sample_id)) {
  num<-which(dat.hand3$Sample.ID==dates$sample_id[i])[1]
  dates$site[i]<-dat.hand3$Site_ID_Ellen[num]
  dates$year[i]<-dat.hand3$year[num]
  dates$month[i]<-dat.hand3$month[num]
  dates$day[i]<-dat.hand3$day[num]
}

sites<-unique(dates$site)
count<-0.1
for (i in 1:length(sites)) {
  sub<-subset(dates, dates$site==sites[i])
  years<-unique(sub$year)
  for (j in 1:length(years)) {
    sub2<-subset(sub, sub$year==years[j])
    if (length(sub2$sample_id)>1) {
      rem<-c(sub2$sample_id[-1])
      dat.hand3<-subset(dat.hand3, dat.hand3$Sample.ID %nin% rem)
    }
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}

#Artificial substrates
dat.art3<-dat.art2

dates<-data.frame(sample_id=unique(dat.art3$Sample.ID), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates$sample_id)) {
  num<-which(dat.art3$Sample.ID==dates$sample_id[i])[1]
  dates$site[i]<-dat.art3$Site_ID_Ellen[num]
  dates$year[i]<-dat.art3$year[num]
  dates$month[i]<-dat.art3$month[num]
  dates$day[i]<-dat.art3$day[num]
}

sites<-unique(dates$site)
count<-0.1
for (i in 1:length(sites)) {
  sub<-subset(dates, dates$site==sites[i])
  years<-unique(sub$year)
  for (j in 1:length(years)) {
    sub2<-subset(sub, sub$year==years[j])
    if (length(sub2$sample_id)>1) {
      rem<-c(sub2$sample_id[-1])
      dat.art3<-subset(dat.art3, dat.art3$Sample.ID %nin% rem)
    }
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}

###---Keep handnet sites when possible
dat.hand4<-dat.hand3
dat.art4<-subset(dat.art3, dat.art3$Site_ID_Ellen %nin% dat.hand4$Site_ID_Ellen)
dat3<-rbind(dat.hand4, dat.art4)

###---Reorder manually again
write.csv(dat3, "BE taxalist selected.csv", row.names=FALSE, fileEncoding = "latin1")


###---Updated Belgian methods in the sites list
sites<-unique(dat3$Site_ID_Ellen)
unique(wrong$ellen_id[which(wrong$ellen_id %nin% sites)])
#121000019, 121000108, 121000251, 121000252

sites2<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
sites2$be_method<-NA

for (i in 1:length(sites)) {
  num<-which(sites2$site_id==sites[i])
  nums<-which(dat3$Site_ID_Ellen==sites[i])[1]
  sites2$be_method[num]<-dat3$sampling.method[nums]
}

length(which(sites2$be_method %nin% NA))

colnames(sites2)
write.csv(sites2[,c(1:11,42:47)], "Ellen - sites.csv", row.names=FALSE, fileEncoding = "latin1")
