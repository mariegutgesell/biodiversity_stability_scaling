#this code selects sites and dates that meet specified criteria
#the "with EQRs" component means the community data must have matching EQR/EQC data

###---Load libraries
library(Hmisc)

###---Set the criteria for site inclusion
eco.type<-"LO" #lotic, _LE for lentic
samp.num<-7 #must have at least this many sampling years
dur.num<-10 #must have at least this much total time series duration

###---Set directories for saving files
eqr.dir<-paste0(getwd(), "/EQRs Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num)
taxa.dir<-paste0(getwd(), "/Taxa for EQRs Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num)
if(!dir.exists(eqr.dir)) dir.create(eqr.dir)
if(!dir.exists(taxa.dir)) dir.create(taxa.dir)

###---Load the overview, site list, and country codes from the master table, must be saved as separate CSVs
#Site list
sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #load the list for your taxonomic group
sites<-subset(sites, sites$ecosystem=="lotic") #filter to desired ecosystem type

###---Load the sites to be removed due to overlap either within or among datasets
#from Step 5 ("Step 5 - duplicated within & among removed part 2.csv")
rem<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

#######################################################################################################
#
#                                 LOAD AND CHECK THE EQR DATASETS
#
#######################################################################################################

###---Load all EQR files, set working directory to the EQR directory for the appropriate taxonomic group
(eqr.names<-list.files(pattern="*.csv"))
eqr.names<-eqr.names[grep(eco.type, eqr.names)] #refine to desired ecosystem type

eqr.files<-list() #empty list
for (i in 1:length(eqr.names)) { #for however many file names you need
  eqr.files[[i]]<-read.csv(eqr.names[i], header=TRUE, fileEncoding="latin1") #load the data into the ith list position 
}

#extract names to be matched to the taxa file names
eqr.names2<-gsub("_EQR", "", eqr.names)

###---Load all taxa files, set working directory to the directory with the unprocessed data for your taxonomic group
(taxa.names<-list.files(pattern="*.csv"))

#reduce to only those with EQRs
taxa.names<-taxa.names[which(taxa.names %in% eqr.names2)]

taxa.files<-list() #empty list
for (i in 1:length(taxa.names)) { #for however many filenames you need
  taxa.files[[i]]<-read.csv(taxa.names[i], header=TRUE, fileEncoding="latin1", sep=";") #load the data into the ith list position 
}

###################### START OF THE DATASET LOOP ###########################################

###---Load each dataset, identify sites that meet the criteria, and build a site list with those sites and dates
#will hang for ~20mins around 20% as it deals with the large England datasets but it will go much faster afterward

#use to load then augment an already created site list
site.list<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
#or use this to create a new site list
site.list<-data.frame(Filename=NA, Dataset.ID=NA, Unique.ID=NA, country=NA, first.prov=NA, river=NA, latitude=NA, longitude=NA, method=NA, units=NA, st.yr=NA, ed.yr=NA, num.yr=NA, dur.yr=NA)

options(warn=2) #will end the loop if any problems are encountered
exclude.data<-c("ENG_062_MZB_LO1.csv","ENG_062_MZB_LO2.csv") #these datasets are excluded from being checked for matching EQR and taxa IDs, some have EQRs with no associated taxonomic data
loop.num<-0.1 #for tracking progress through the for loop

eqr.names
data.nums<-c(1) #use to run a specific dataset or set of specific datasets
data.nums<-c(1:length(eqr.names)) #use to run datasets starting at a specified number until the end

for (i in data.nums) { #loop through each dataset
  
  ##--Load the dataset
  dat.eqr<-eqr.files[[i]]
  dat.eqr<-subset(dat.eqr, dat.eqr$eqr %nin% NA) #remove samples missing EQR values
  
  dat.taxa<-taxa.files[[i]]
  dat.taxa<-subset(dat.taxa, dat.taxa$Site.ID %in% dat.eqr$Site.ID)
  
  #Fix abundance values
  dat.taxa$Abundance<-as.numeric(gsub(",", replacement="\\.", dat.taxa$Abundance))
  dat.taxa<-subset(dat.taxa, dat.taxa$Abundance != 0) #remove 0s
  dat.taxa<-subset(dat.taxa, dat.taxa$Abundance %nin% NA) #remove NAs
  dat.taxa<-subset(dat.taxa, dat.taxa$Abundance %nin% "") #remove blanks
  
  data.check<-0 #used to check if any usable sites are present, reset each loop
  
###################### CHECK 1: if statement for multiple sampling methods #############################
  
  if (length(dat.taxa$sampling.method)==0) { #if a single sampling method is used
    
    ##--Add the sampling dates
    dat.eqr$year<-NA
    dat.eqr$month<-NA
    dat.eqr$day<-NA
    dat.eqr$jul<-NA
    
    dat.taxa$year<-NA
    dat.taxa$month<-NA
    dat.taxa$day<-NA
    dat.taxa$jul<-NA
    
    #EQR dates loop
    dates<-unique(dat.eqr$Sampling.date)
    for (j in 1:length(dates)) {
      dates.name<-strsplit(dates[j], split="\\.")
      rnums<-which(dat.eqr$Sampling.date==dates[j])
      dat.eqr$year[rnums]<-as.numeric(dates.name[[1]][3])
      dat.eqr$month[rnums]<-as.numeric(dates.name[[1]][2])
      
      #some days are missing
      if (dates.name[[1]][1] != "NA") { #if not missing
        dat.eqr$day[rnums]<-as.numeric(dates.name[[1]][1])
      }
      if (dates.name[[1]][1] == "NA") { #if missing just set the day to the first day of the month
        dat.eqr$day[rnums]<-1
      }
      
      date<-as.POSIXlt(paste(dat.eqr$day[rnums[1]], dat.eqr$month[rnums[1]], dat.eqr$year[rnums[1]], sep="."), format="%d.%m.%Y")
      dat.eqr$jul[rnums]<-as.POSIXlt(date, format="%m/%d/%Y")$yday+1
      dat.eqr$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
    }
    
    #Taxa dates loop
    dates<-unique(dat.taxa$Sampling.date)
    for (j in 1:length(dates)) {
      dates.name<-strsplit(dates[j], split="\\.")
      rnums<-which(dat.taxa$Sampling.date==dates[j])
      dat.taxa$year[rnums]<-as.numeric(dates.name[[1]][3])
      dat.taxa$month[rnums]<-as.numeric(dates.name[[1]][2])
      
      #some days are missing
      if (dates.name[[1]][1] != "NA") { #if not missing
        dat.taxa$day[rnums]<-as.numeric(dates.name[[1]][1])
      }
      if (dates.name[[1]][1] == "NA") { #if missing just set the day to the first day of the month
        dat.taxa$day[rnums]<-1
      }
      
      date<-as.POSIXlt(paste(dat.taxa$day[rnums[1]], dat.taxa$month[rnums[1]], dat.taxa$year[rnums[1]], sep="."), format="%d.%m.%Y")
      dat.taxa$jul[rnums]<-as.POSIXlt(date, format="%m/%d/%Y")$yday+1
      dat.taxa$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
    }
    
    #Check if there were any problems assigning sampling dates
    if (any(dat.eqr$Sampling.date %in% NA) | any(dat.taxa$Sampling.date %in% NA)) {
      print("Site selection stopped - problem assigning sampling dates, check and fix manually")
      break()
    }
    
    ##--Order data frames, not necessary but makes it easier if you need to check
    dat.eqr<-dat.eqr[order(dat.eqr$Site.ID, dat.eqr$year, dat.eqr$jul),]
    dat.taxa<-dat.taxa[order(dat.taxa$Site.ID, dat.taxa$year, dat.taxa$jul, dat.taxa$Taxon.name),]
    
    ##--Add unique dataset, site, and sample IDs
    dat.eqr$Temp.ID<-paste(dat.eqr$Site.ID, dat.eqr$Sampling.date, sep="_")
    dat.taxa$Temp.ID<-paste(dat.taxa$Site.ID, dat.taxa$Sampling.date, sep="_")
    
    dat.eqr$Unique.ID<-NA
    dat.eqr$Samp.ID<-NA
    
    dat.taxa$Unique.ID<-NA
    dat.taxa$Samp.ID<-NA
    
    #Check for duplicated EQRs, only one EQR per sample should exist
    if (length(dat.eqr$Site.ID) != length(unique(dat.eqr$Temp.ID))) {
      print("Site selection stopped - multiple EQR samples from the same site and date detected, check and fix manually")
      break()
    }
    
    #EQR IDs
    name<-eqr.names2[i]
    name<-gsub(".csv", "", name)
    name<-gsub(paste(eco.type, "\\d", sep=""), eco.type, name) #dataset ID for matching to sites
    for (j in 1:length(dat.eqr$Temp.ID)) {
      rnum<-which(sites$Site_ID_original==dat.eqr$Site.ID[j] & sites$Dataset.ID==name)
      dat.eqr$Unique.ID[j]<-sites$Unique.ID[rnum]
      dat.eqr$Samp.ID[j]<-paste(dat.eqr$Unique.ID[j], dat.eqr$Sampling.date[j], sep="-")
    }
    
    #Taxa IDs
    temp<-unique(dat.taxa$Temp.ID)
    for (j in 1:length(temp)) {
      rnums<-which(dat.taxa$Temp.ID==temp[j])
      rnum<-which(sites$Site_ID_original==dat.taxa$Site.ID[rnums[1]] & sites$Dataset.ID==name)
      dat.taxa$Unique.ID[rnums]<-sites$Unique.ID[rnum]
      dat.taxa$Samp.ID[rnums]<-paste(dat.taxa$Unique.ID[rnums[1]], dat.taxa$Sampling.date[rnums[1]], sep="-")
    }
    
    ##--Only sites and samples that meet the sampling number criteria and are from the same season (any 3 consecutive months)
    unq.site<-unique(dat.eqr$Unique.ID)
    num.site<-numeric()
    win.pri<-numeric()
    
    #Identify the most consistently sampled seasonal windows across years (3 consecutive months)
    #each year can only contribute once to each window
    #Windows: 1=1-3, 2=2-4, 3=3-5, 4=4-6, 5=5-7, 6=6-8, 7=7-9, 8=8-10, 9=9-11, 10=10-12
    win.dat<-data.frame(win=c(1:10), num=0)
    years<-sort(unique(dat.eqr$year))
    for (j in 1:length(years)) { #for each year samples were collected
      sub<-dat.eqr[dat.eqr$year==years[j],]
      window<-c(1,3)
      for (k in 1:10) { #1:10 because there are 10 potential seasonal windows
        sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2])
        if (length(sub2[,1])>0) {
          win.dat$num[k]<-win.dat$num[k]+log10(length(unique(sub2$Samp.ID))+1) #log-transform to downweight large sampling events isolated in individual years
        }
        window<-window+1 #tick up to the next window
      }
    }
    win.pri<-win.dat$win[order(win.dat$num, decreasing=TRUE)]
    
    #Window data check
    #Windows: 1=1-3, 2=2-4, 3=3-5, 4=4-6, 5=5-7, 6=6-8, 7=7-9, 8=8-10, 9=9-11, 10=10-12
    for (j in 1:length(unq.site)) {
      sub<-subset(dat.eqr, dat.eqr$Unique.ID==unq.site[j] & dat.eqr$eqr %nin% NA)
      window<-c(1,3)
      win.num<-numeric()
      win.dur<-numeric()
      for (k in 1:10) {
        sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2]) #data that fits within each window
        win.num[k]<-length(unique(sub2$year)) #save the number of years of data within each window
        if (win.num[k]==0) {
          win.dur[k]<-0
        }
        if (win.num[k]==1) {
          win.dur[k]<-1
        }
        if (win.num[k]>1) {
          win.dur[k]<-(max(sub2$year)-min(sub2$year))+1
        }
        window<-window+1 #tick up to the next window
      }
      if (any(win.num>=samp.num) & any(win.dur>=dur.num)) { #if something exists that may meet both criteria
        wins.num<-which(win.num>=samp.num) #select all windows that meet it
        wins.dur<-which(win.dur>=dur.num) #and the windows that meet the duration criteria
        wins.num<-wins.num[which(wins.num %in% wins.dur)] #match the two
        if (length(wins.num)>0) { #if there are windows that meet both the number and duration criteria
          check<-0
          for (m in 1:length(win.pri)) { #cycle through the prioritized windows
            if (any(wins.num==win.pri[m]) & check==0) { #if the given prioritized window has enough data and a window has yet to be saved
              num.site[j]<-win.pri[m] #save as the selected window for the site
              check<-1 #and mark that a window has been selected
            }
          }
        } else { #if no windows meet both the number and duration criteria
          num.site[j]<-0
        }
      } else { #if no windows may meet the criteria
        num.site[j]<-0
      }
    }
    
    ##--If any usable sites were found
    if (any(num.site>0)) {
      
      #include only data from the selected window for each site
      dat.eqr2<-dat.eqr[1,]
      for (j in 1:length(num.site)) {
        if (num.site[j]>0) {
          sub<-subset(dat.eqr, dat.eqr$Unique.ID==unq.site[j] & dat.eqr$month>=num.site[j] & dat.eqr$month<=(num.site[j]+2))
          dat.eqr2<-rbind(dat.eqr2, sub)
        }
      }
      dat.eqr2<-dat.eqr2[-1,]
      
      ##--Some sites may have two samples collected within the same window in the same year
      #if this occurs, select only the sample that is from the Julian day closest to samples from other years
      dat.eqr3<-dat.eqr2[1,]
      unq.site<-unique(dat.eqr2$Unique.ID)
      
      for (j in 1:length(unq.site)) {
        sub<-subset(dat.eqr2, dat.eqr2$Unique.ID==unq.site[j])
        years<-as.numeric(table(sub$year))
        if (any(years>1)) {
          years<-unique(sub$year)
          mean.jul<-mean(sub$jul)
          for (k in 1:length(years)) {
            sub2<-subset(sub, sub$year==years[k])
            if (length(sub2$Site.ID)>1) {
              diff<-abs(sub2$jul-mean.jul)
              rem.samp<-sub2$Samp.ID[-which(diff==min(diff))[1]]
              sub<-subset(sub, sub$Samp.ID %nin% rem.samp)
            }
          }
        }
        dat.eqr3<-rbind(dat.eqr3, sub)
      }
      dat.eqr3<-dat.eqr3[-1,]
      
      ##--Remove spatially overlapping sites
      if (any(num.site>0)) {
        dat.eqr4<-subset(dat.eqr3, dat.eqr3$Unique.ID %nin% rem$unique.id) #remove spatially overlapping sites
        if (length(dat.eqr4$Site.ID)>0) { #if any sites remain
          data.check<-1 #mark that usable data was found
        }
        if (length(dat.eqr4$Site.ID)==0) { #if all sites were removed
          data.check<-0 #mark that no usable data was found
        }
      }
    } #end of IF statement checking if any usable sites were found

#################################### END OF SINGLE METHOD IF STATEMENT ##################################### 
    
  } else { #if multiple methods are used then the site selection process has to be divided by method
    
    #Need to assign the methods to the EQRs based on the taxa dataset, which requires matching the site IDs
    
    ##--Add the sampling dates
    dat.eqr$year<-NA
    dat.eqr$month<-NA
    dat.eqr$day<-NA
    dat.eqr$jul<-NA
    
    dat.taxa$year<-NA
    dat.taxa$month<-NA
    dat.taxa$day<-NA
    dat.taxa$jul<-NA
    
    #EQR dates
    dates<-unique(dat.eqr$Sampling.date)
    for (j in 1:length(dates)) {
      dates.name<-strsplit(dates[j], split="\\.")
      rnums<-which(dat.eqr$Sampling.date==dates[j])
      dat.eqr$year[rnums]<-as.numeric(dates.name[[1]][3])
      dat.eqr$month[rnums]<-as.numeric(dates.name[[1]][2])
      
      #some days are missing
      if (dates.name[[1]][1] != "NA") { #if not missing
        dat.eqr$day[rnums]<-as.numeric(dates.name[[1]][1])
      }
      if (dates.name[[1]][1] == "NA") { #if missing just set the day to the first day of the month
        dat.eqr$day[rnums]<-1
      }
      
      date<-as.POSIXlt(paste(dat.eqr$day[rnums[1]], dat.eqr$month[rnums[1]], dat.eqr$year[rnums[1]], sep="."), format="%d.%m.%Y")
      dat.eqr$jul[rnums]<-as.POSIXlt(date, format="%m/%d/%Y")$yday+1
      dat.eqr$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
    }
    
    #Taxa dates
    dates<-unique(dat.taxa$Sampling.date)
    for (j in 1:length(dates)) {
      dates.name<-strsplit(dates[j], split="\\.")
      rnums<-which(dat.taxa$Sampling.date==dates[j])
      dat.taxa$year[rnums]<-as.numeric(dates.name[[1]][3])
      dat.taxa$month[rnums]<-as.numeric(dates.name[[1]][2])
      
      #some days are missing
      if (dates.name[[1]][1] != "NA") { #if not missing
        dat.taxa$day[rnums]<-as.numeric(dates.name[[1]][1])
      }
      if (dates.name[[1]][1] == "NA") { #if missing just set the day to the first day of the month
        dat.taxa$day[rnums]<-1
      }
      
      date<-as.POSIXlt(paste(dat.taxa$day[rnums[1]], dat.taxa$month[rnums[1]], dat.taxa$year[rnums[1]], sep="."), format="%d.%m.%Y")
      dat.taxa$jul[rnums]<-as.POSIXlt(date, format="%m/%d/%Y")$yday+1
      dat.taxa$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
    }
    
    #Check if there were any problems assigning sampling dates
    if (any(dat.eqr$Sampling.date %in% NA) | any(dat.taxa$Sampling.date %in% NA)) {
      print("Site selection stopped - problem assigning sampling dates, check and fix manually")
      break()
    }
    
    ##--Order data frames, not necessary but makes it easier if you need to check
    dat.eqr<-dat.eqr[order(dat.eqr$Site.ID, dat.eqr$year, dat.eqr$jul),]
    dat.taxa<-dat.taxa[order(dat.taxa$Site.ID, dat.taxa$year, dat.taxa$jul, dat.taxa$Taxon.name),]
    
    ##--Add unique dataset, site, and sample IDs
    dat.eqr$Temp.ID<-paste(dat.eqr$Site.ID, dat.eqr$Sampling.date, sep="_")
    dat.taxa$Temp.ID<-paste(dat.taxa$Site.ID, dat.taxa$Sampling.date, sep="_")
    
    dat.eqr$Unique.ID<-NA
    dat.eqr$Samp.ID<-NA
    
    dat.taxa$Unique.ID<-NA
    dat.taxa$Samp.ID<-NA
    
    #Check for duplicated EQRs, only one EQR per sample should exist
    if (length(dat.eqr$Site.ID) != length(unique(dat.eqr$Temp.ID))) {
      print("Site selection stopped - duplicate EQRs detected, check and fix manually")
      break()
    }
    
    #EQR IDs
    name<-eqr.names2[i]
    name<-gsub(".csv", "", name)
    name<-gsub(paste(eco.type, "\\d", sep=""), eco.type, name)
    for (j in 1:length(dat.eqr$Temp.ID)) {
      rnum<-which(sites$Site_ID_original==dat.eqr$Site.ID[j] & sites$Dataset.ID==name)
      dat.eqr$Unique.ID[j]<-sites$Unique.ID[rnum]
      dat.eqr$Samp.ID[j]<-paste(dat.eqr$Unique.ID[j], dat.eqr$Sampling.date[j], sep="-")
    }
    
    #Taxa IDs
    temp<-unique(dat.taxa$Temp.ID)
    for (j in 1:length(temp)) {
      rnums<-which(dat.taxa$Temp.ID==temp[j])
      rnum<-which(sites$Site_ID_original==dat.taxa$Site.ID[rnums[1]] & sites$Dataset.ID==name)
      dat.taxa$Unique.ID[rnums]<-sites$Unique.ID[rnum]
      dat.taxa$Samp.ID[rnums]<-paste(dat.taxa$Unique.ID[rnums[1]], dat.taxa$Sampling.date[rnums[1]], sep="-")
    }
    
    ##--Method matching
    dat.eqr$Method<-NA
    unq.site<-unique(dat.eqr$Unique.ID)
    
    for (j in 1:length(unq.site)) {
      rnums<-which(dat.eqr$Unique.ID==unq.site[j])
      rnum<-which(dat.taxa$Unique.ID==unq.site[j])[1]
      dat.eqr$Method[rnums]<-dat.taxa$sampling.method[rnum]
    }
    
    ##--Divide site checking by method, prioritize the most common and only select from less common if not enough data
    methods<-names(sort(table(dat.eqr$Method), decreasing=TRUE)) #most common goes first followed by less common
    eqr.meth<-dat.eqr[1,] #this dataset will be built as each method is checked
    
    for (n in 1:length(methods)) {
      eqr.sub<-subset(dat.eqr, dat.eqr$Method==methods[n])

      ##--Only sites and samples that meet the sampling number criteria and are from the same season (any 3 consecutive months)
      unq.site<-unique(eqr.sub$Unique.ID)
      num.site<-numeric()
      win.pri<-numeric()
      
      #Identify the most consistently sampled seasonal windows across years (3 consecutive months)
      #Windows: 1=1-3, 2=2-4, 3=3-5, 4=4-6, 5=5-7, 6=6-8, 7=7-9, 8=8-10, 9=9-11, 10=10-12
      win.dat<-data.frame(win=c(1:10), num=0)
      years<-sort(unique(eqr.sub$year))
      for (j in 1:length(years)) { #for each year samples were collected
        sub<-eqr.sub[eqr.sub$year==years[j],]
        window<-c(1,3)
        for (k in 1:10) { #1:10 because there are 10 potential seasonal windows
          sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2])
          if (length(sub2[,1])>0) {
            win.dat$num[k]<-win.dat$num[k]+log10(length(unique(sub2$Samp.ID))+1) #log-transform to downweight large sampling events isolated in individual years
          }
          window<-window+1 #tick up to the next window
        }
      }
      win.pri<-win.dat$win[order(win.dat$num, decreasing=TRUE)]
      
      #Window data check
      for (j in 1:length(unq.site)) {
        sub<-subset(eqr.sub, eqr.sub$Unique.ID==unq.site[j] & eqr.sub$eqr %nin% NA)
        window<-c(1,3)
        win.num<-numeric()
        win.dur<-numeric()
        for (k in 1:10) {
          sub2<-subset(sub, sub$month>=window[1] & sub$month<=window[2]) #data that fits within each window
          win.num[k]<-length(unique(sub2$year)) #save the number of years of data within each window
          if (win.num[k]==0) {
            win.dur[k]<-0
          }
          if (win.num[k]==1) {
            win.dur[k]<-1
          }
          if (win.num[k]>1) {
            win.dur[k]<-(max(sub2$year)-min(sub2$year))+1
          }
          window<-window+1 #tick up to the next window
        }
        if (any(win.num>=samp.num) & any(win.dur>=dur.num)) { #if something exists that may meet both criteria
          wins.num<-which(win.num>=samp.num) #select all windows that meet it
          wins.dur<-which(win.dur>=dur.num) #and the windows that meet the duration criteria
          wins.num<-wins.num[which(wins.num %in% wins.dur)] #match the two
          
          if (length(wins.num)>0) { #if there are windows that meet both the number and duration criteria
            check<-0
            for (m in 1:length(win.pri)) { #cycle through the prioritized windows
              if (any(wins.num==win.pri[m]) & check==0) { #if the given prioritized window has enough data and a window has yet to be saved
                num.site[j]<-win.pri[m] #save as the selected window for the site
                check<-1 #and mark that a window has been selected
              }
            }
          } else { #if no windows meet both the number and duration criteria
            num.site[j]<-0
          }
        } else { #if no windows may meet the criteria
          num.site[j]<-0
        }
      }
      
      ##--If any usable sites were found
      if (any(num.site>0)) {
        
        #include only data from the selected window for each site
        dat.eqr2<-eqr.sub[1,]
        for (j in 1:length(num.site)) {
          if (num.site[j]>0) {
            sub<-subset(eqr.sub, eqr.sub$Unique.ID==unq.site[j] & eqr.sub$month>=num.site[j] & eqr.sub$month<=(num.site[j]+2))
            dat.eqr2<-rbind(dat.eqr2, sub)
          }
        }
        dat.eqr2<-dat.eqr2[-1,]
        
        ##--Some sites may have two samples collected within the same window in the same year
        #if this occurs, select only the sample that is from the Julian day closest to samples from other years
        dat.eqr3<-dat.eqr2[1,]
        unq.site<-unique(dat.eqr2$Unique.ID)
        
        for (j in 1:length(unq.site)) {
          sub<-subset(dat.eqr2, dat.eqr2$Unique.ID==unq.site[j])
          years<-as.numeric(table(sub$year))
          if (any(years>1)) {
            years<-unique(sub$year)
            mean.jul<-mean(sub$jul)
            for (k in 1:length(years)) {
              sub2<-subset(sub, sub$year==years[k])
              if (length(sub2$Site.ID)>1) {
                diff<-abs(sub2$jul-mean.jul)
                rem.samp<-sub2$Samp.ID[-which(diff==min(diff))[1]]
                sub<-subset(sub, sub$Samp.ID %nin% rem.samp)
              }
            }
          }
          dat.eqr3<-rbind(dat.eqr3, sub)
        }
        dat.eqr3<-dat.eqr3[-1,]
        
        ##--Remove spatially overlapping sites
        dat.eqr4<-subset(dat.eqr3, dat.eqr3$Unique.ID %nin% rem$unique.id) #remove spatially overlapping sites
        if (length(dat.eqr4$Site.ID)>0) { #if any sites remain
          data.check<-1 #mark that usable data was found
          #Add the method associated with these sites to the site list
          nums<-which(sites$Unique.ID %in% dat.eqr4$Unique.ID) 
          sites$sampling.method[nums]<-methods[n]
        }
        if (length(dat.eqr4$Site.ID)==0) { #if all sites were removed
          data.check<-0 #mark that no usable data was found
        }
      } #end of IF statement checking if any usable sites were found
      
      ##--Add data
      if (length(eqr.meth$Site.ID)==1 & data.check==1 & any(num.site>0)) { #if usable data was found and it's the first iteration of the loop
        eqr.meth<-rbind(eqr.meth, dat.eqr4)
        eqr.meth<-eqr.meth[-1,]
      } else if (length(eqr.meth$Site.ID)!=1 & data.check==1 & any(num.site>0)) { #if usable data was found and it's not the first iteration of the loop
        eqr.meth<-rbind(eqr.meth, dat.eqr4)
      }
      
      ##--Remove the sites that have been selected and go to the next method
      dat.eqr<-subset(dat.eqr, dat.eqr$Unique.ID %nin% eqr.meth$Unique.ID)
      
    } #end of methods for loop
    
    if (data.check==1) {
      dat.eqr4<-eqr.meth #for file saving
    }
    
  } #end of check 1 else statement
  
############################# END OF MULTIPLE METHOD ELSE STATEMENT #############################################
  
  if (data.check==1) { #if usable data was found
    
    ##--Refine the taxa dataset to only sample IDs with matching EQRs that meet the criteria
    dat.taxa2<-subset(dat.taxa, dat.taxa$Samp.ID %in% dat.eqr4$Samp.ID)
    
    ##--Check that the number of sample IDs match, expand the list  of excluded datasets if they have EQR data for some sites but no associated taxonomic data (e.g., ENG_062_MZB_LO)
    if (taxa.names[i] %nin% exclude.data) {
      if (length(dat.eqr4$Samp.ID) != length(unique(dat.taxa2$Samp.ID))) {
        print("Site selection stopped - sample IDs do not match between EQRs and taxa, check and fix manually")
        break()
      }
    }
    
    ##--Refine EQRs to only sample IDs with taxonomic data
    dat.eqr4<-subset(dat.eqr4, dat.eqr4$Samp.ID %in% dat.taxa2$Samp.ID)
    
    ##--Re-order the datasets
    dat.eqr4<-dat.eqr4[order(dat.eqr4$Unique.ID, dat.eqr4$year),]
    dat.taxa2<-dat.taxa2[order(dat.taxa2$Unique.ID, dat.taxa2$year, dat.taxa2$Taxon.name),]
    
    ##--Ensure all included sites still meet the criteria
    ids<-unique(dat.eqr4$Unique.ID)
    site.sub<-site.list[rep(1, times=length(ids)),]
    site.sub[,]<-NA
    site.sub$Unique.ID<-ids
    for (j in 1:length(site.sub$Unique.ID)) {
      rnum<-which(sites$Unique.ID==site.sub$Unique.ID[j])
      sub<-dat.eqr4[which(dat.eqr4$Unique.ID==site.sub$Unique.ID[j]),]
      site.sub$Filename[j]<-sites$Filename[rnum]
      site.sub$Dataset.ID[j]<-sites$Dataset.ID[rnum]
      site.sub$country[j]<-sites$Country[rnum]
      site.sub$first.prov[j]<-sites$Data_owner[rnum]
      site.sub$river[j]<-sites$River.lake[rnum]
      site.sub$latitude[j]<-sites$Latitude_Y[rnum]
      site.sub$longitude[j]<-sites$Longitude_X[rnum]
      site.sub$method[j]<-sites$sampling.method[rnum]
      site.sub$units[j]<-sites$unit[rnum]
      site.sub$st.yr[j]<-min(sub$year)
      site.sub$ed.yr[j]<-max(sub$year)
      site.sub$num.yr[j]<-length(unique(sub$year))
      site.sub$dur.yr[j]<-(max(sub$year)-min(sub$year))+1
    }
    site.sub<-subset(site.sub, site.sub$num.yr>=samp.num & site.sub$dur.yr>=dur.num)
    
    ##--If any sites are left that meet the criteria
    if (length(site.sub$Unique.ID)>0) {
      ##--Select columns
      dat.eqr4<-dat.eqr4[,c("Unique.ID","Samp.ID","Sampling.date","year","month","day","eqr","eqc")]
      dat.taxa2<-dat.taxa2[,c("Unique.ID","Samp.ID","Sampling.date","year","month","day","Taxon.name","Abundance")]
      
      ##--Save the datasets to the working directory
      name<-eqr.names2[i]
      name<-gsub(".csv", "", name)
      
      write.csv(x=dat.eqr4, file=paste(eqr.dir, "/", name, " - selected EQRs.csv", sep=""), row.names=FALSE, fileEncoding="latin1")
      write.csv(x=dat.taxa2, file=paste(taxa.dir, "/", name, " - selected taxa.csv", sep=""), row.names=FALSE, fileEncoding="latin1")
      
      ##--Add sites to the site list
      site.list<-rbind(site.list, site.sub)
    }
  }
  
  ##---Report progress
  if (i/length(eqr.names)>=loop.num) {
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }

} #end of the dataset loop


###---Save the site list

#run this line only if making a new site list
site.list<-site.list[-1,]

write.csv(x=site.list, file=paste0("Site list for EQRs, Grp=MZB, Eco=",eco.type,", Num=",samp.num, ", Dur=",dur.num), row.names=FALSE, fileEncoding="latin1")
