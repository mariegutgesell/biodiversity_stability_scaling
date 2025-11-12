library(Hmisc)

master.over<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #master table overview sheet
master.sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #master table samples sheet (it's the sites sheet really)
cty.code<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #master table country code sheet

taxa<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #taxa
eqr<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #EQRs

###---Add owner numbers
options(warn=2)

taxa$Provider.Number<-paste("0", master.over$Provider.Number[which(master.over$Data_owner=="Ioannis Karaouzas")], sep="")
eqr$Provider.Number<-paste("0", master.over$Provider.Number[which(master.over$Data_owner=="Ioannis Karaouzas")], sep="")
taxa$Country<-master.over$Country[which(master.over$Data_owner=="Ioannis Karaouzas")]
eqr$Country<-master.over$Country[which(master.over$Data_owner=="Ioannis Karaouzas")]


###---Add site IDs
taxa$Dataset.ID<-NA
taxa$Unique.ID<-NA
i<-1
for (i in 1:length(taxa$Site.ID)) {
  cty.num<-which(cty.code$country==taxa$Country[i])
  num<-which(master.sites$Country==taxa$Country[i] & master.sites$Site_ID_original==taxa$Site.ID[i])
  if (master.sites$ecosystem[num]=="lotic") {
    taxa$Dataset.ID[i]<-paste(cty.code$letters[cty.num], taxa$Provider.Number[i], "MZB_LO", sep="_")
    taxa$Unique.ID[i]<-paste(cty.code$letters[cty.num], taxa$Provider.Number[i], "MZB_LO", master.sites$SGN_Site_ID[num], sep="_")
  }
  if (master.sites$ecosystem[num]=="lentic") {
    taxa$Dataset.ID[i]<-paste(cty.code$letters[cty.num], taxa$Provider.Number[i], "MZB_LE", sep="_")
    taxa$Unique.ID[i]<-paste(cty.code$letters[cty.num], taxa$Provider.Number[i], "MZB_LE", master.sites$SGN_Site_ID[num], sep="_")
  }
}

###---Add dates
taxa$year<-NA
taxa$month<-NA
taxa$day<-NA
taxa$sample_id<-NA
dates<-unique(taxa$Sampling.date)
for (i in 1:length(dates)) {
  nums<-which(taxa$Sampling.date==dates[i])
  name<-strsplit(taxa$Sampling.date[nums[1]], split="\\.")
  taxa$year[nums]<-as.numeric(name[[1]][3])
  taxa$month[nums]<-as.numeric(name[[1]][2])
  taxa$day[nums]<-as.numeric(name[[1]][1])
  date<-as.POSIXlt(paste(taxa$day[nums[1]], taxa$month[nums[1]], taxa$year[nums[1]], sep="."), format="%d.%m.%Y")
  taxa$sample_id[nums]<-paste(taxa$Unique.ID[nums[1]], format(date, "%d.%m.%Y"), sep="_")
}

###---Reorder manually
write.csv(taxa, "GR long-form all sites w dates.csv", row.names=FALSE, fileEncoding = "latin1")
taxa2<-read.csv(file.choose(), header=TRUE, encoding="latin1")


###---Only sites with >=7 samples from the same season (any 3 consecutive months)

dates<-data.frame(sample_id=unique(taxa2$sample_id), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates$sample_id)) {
  num<-which(taxa2$sample_id==dates$sample_id[i])[1]
  dates$site[i]<-taxa2$Unique.ID[num]
  dates$year[i]<-taxa2$year[num]
  dates$month[i]<-taxa2$month[num]
  dates$day[i]<-taxa2$day[num]
}

#Windows
#1=1-3, 2=2-4, 3=3-5, 4=4-6, 5=5-7, 6=6-8, 7=7-9, 8=8-10, 9=9-11, 10=10-12
options(warn=2) #1 is default

sites<-unique(dates$site)
site.nums<-numeric()
sort(unique(dates$month))
table(dates$month)

count<-0.1
for (i in 1:length(sites)) {
  sub<-subset(dates, dates$site==sites[i])
  window<-c(1,3)
  win.num<-numeric()
  for (j in 1:10) {
    sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2])
    win.num[j]<-length(unique(sub2$year))
    window<-window+1
  }
  if (any(win.num>6)) {
    wins<-which(win.num>6)
    if (length(wins)>1) {
      if (length(which(wins==4))>0) {
        wins<-4
      } else if (length(which(wins==5))>0) {
        wins<-5
      } else if (length(which(wins==7))>0) {
        wins<-7
      } else if (length(which(wins==8))>0) {
        wins<-8
      } else if (length(which(wins==9))>0) {
        wins<-9
      } else {
        wins<-which(win.num==max(win.num))[1]
      }
    }
    site.nums[i]<-wins
  } else {
    site.nums[i]<-0
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}
site.nums
sites[which(site.nums>0)]

taxa3<-taxa2[1,]
count<-0.1
for (i in 1:length(sites)) {
  if (site.nums[i]>0) {
    if (site.nums[i]==1) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(1,2,3))
    }
    if (site.nums[i]==2) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(2,3,4))
    }
    if (site.nums[i]==3) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(3,4,5))
    }
    if (site.nums[i]==4) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(4,5,6))
    }
    if (site.nums[i]==5) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(5,6,7))
    }
    if (site.nums[i]==6) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(6,7,8))
    }
    if (site.nums[i]==7) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(7,8,9))
    }
    if (site.nums[i]==8) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(8,9,10))
    }
    if (site.nums[i]==9) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(9,10,11))
    }
    if (site.nums[i]==10) {
      sub<-subset(taxa2, taxa2$Unique.ID==sites[i] & taxa2$month %in% c(10,11,12))
    }
    taxa3<-rbind(taxa3, sub)
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}
taxa3<-taxa3[-1,]

###---Get rid of samples collected twice in the same year and season

taxa4<-taxa3

dates<-data.frame(sample_id=unique(taxa3$sample_id), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates$sample_id)) {
  num<-which(taxa3$sample_id==dates$sample_id[i])[1]
  dates$site[i]<-taxa3$Unique.ID[num]
  dates$year[i]<-taxa3$year[num]
  dates$month[i]<-taxa3$month[num]
  dates$day[i]<-taxa3$day[num]
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
      taxa4<-subset(taxa4, taxa4$sample_id %nin% rem)
    }
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}

###---Only sites with data spanning >=10 years
dates<-data.frame(sample_id=unique(taxa4$sample_id), site=NA, year=NA, month=NA, day=NA)
for (i in 1:length(dates$sample_id)) {
  num<-which(taxa4$sample_id==dates$sample_id[i])[1]
  dates$site[i]<-taxa4$Unique.ID[num]
  dates$year[i]<-taxa4$year[num]
  dates$month[i]<-taxa4$month[num]
  dates$day[i]<-taxa4$day[num]
}

sites<-unique(dates$site)
site.nums<-numeric()
count<-0.1
for (i in 1:length(sites)) {
  sub<-subset(taxa4, taxa4$Unique.ID==sites[1])
  if (((max(sub$year)-min(sub$year))+1)>=10) {
    site.nums[i]<-1
  } else {
    site.nums[i]<-0 #remove
  }
  if (i/length(sites)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}
which(site.nums==0)
#keep all sites