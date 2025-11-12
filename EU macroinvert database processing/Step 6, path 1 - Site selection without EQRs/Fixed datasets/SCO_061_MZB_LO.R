dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
raw<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

dat$year<-NA
dat$month<-NA
dat$day<-NA

#Dates loop
dates<-unique(dat$Sampling.date)
for (j in 1:length(dates)) {
  dates.name<-strsplit(dates[j], split="\\.")
  rnums<-which(dat$Sampling.date==dates[j])
  dat$year[rnums]<-as.numeric(dates.name[[1]][3])
  dat$month[rnums]<-as.numeric(dates.name[[1]][2])
  dat$day[rnums]<-as.numeric(dates.name[[1]][1])
  
  date<-as.POSIXlt(paste(dat$day[rnums[1]], dat$month[rnums[1]], dat$year[rnums[1]], sep="."), format="%d.%m.%Y")
  dat$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
}

raw$year<-NA
raw$month<-NA
raw$day<-NA

dates<-unique(raw$Sampling.date)
for (j in 1:length(dates)) {
  dates.name<-strsplit(dates[j], split="/")
  rnums<-which(raw$Sampling.date==dates[j])
  raw$year[rnums]<-as.numeric(dates.name[[1]][3])
  raw$month[rnums]<-as.numeric(dates.name[[1]][1])
  raw$day[rnums]<-as.numeric(dates.name[[1]][2])
  
  date<-as.POSIXlt(paste(raw$day[rnums[1]], raw$month[rnums[1]], raw$year[rnums[1]], sep="."), format="%d.%m.%Y")
  raw$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
}

options(warn=2)
dat2<-dat
dat2$Abundance<-NA
dat2$Definition.of.abundance.class<-NA

raw2<-raw
raw2$Definition.of.abundance.class[which(raw$Definition.of.abundance.class=="unclear")]<-"100-999" #labeled as between B/C, so use minimum of C
raw2$Abundance<-gsub(pattern="~", replacement="", x=raw$Abundance)
raw2$Abundance<-as.numeric(raw2$Abundance)
raw2$Sample.ID2<-paste(raw2$Site.ID, raw2$Sampling.date, sep="_")
ids<-unique(raw2$Sample.ID2)

raw3<-raw2[1,]
loop.num<-0.1
for (i in 1:length(ids)) {
  sub<-raw2[which(raw2$Sample.ID2==ids[i]),]
  
  #if multiple samples, then pick the first one
  if (length(unique(sub$Sample.ID))>1) {
    sub<-sub[which(sub$Sample.ID==unique(sub$Sample.ID)[1]),]
  }
  
  #check abundance values against the abundance class, favor abundance class if they differ
  for (j in 1:length(sub$Site.ID)) {
    num<-as.character(gsub(pattern="~", replacement="", sub$Definition.of.abundance.class[j]))
    num<-strsplit(num, split="-")
    if (sub$Abundance[j] %in% NA | sub$Abundance[j]=="") { #if missing a value
      dat2$Abundance[j]<-as.numeric(num[[1]][1]) #use the min of the class
    } else { #if not missing an abundance value
      if (sub$Abundance[j]<as.numeric(num[[1]][1]) | sub$Abundance[j]>as.numeric(num[[1]][2])) { #check the value against the class and use the min of the class if they differ
        sub$Abundance[j]<-as.numeric(num[[1]][1])
      }
    }
  }
  
  #if multiple of the same taxon in a sample
  if (any(as.numeric(table(sub$Taxon.name..original.))>1)) {
    sub2<-sub[1,]
    names<-sort(unique(sub$Taxon.name..original.))
    for (j in 1:length(names)) {
      sub3<-sub[which(sub$Taxon.name..original.==names[j]),]
      sub3$Abundance[1]<-sum(sub3$Abundance)
      if (sub3$Abundance[1]<10) {
        sub3$Definition.of.abundance.class[1]<-"~1-9"
        sub3$Abundance.class[1]<-"A"
      }
      if (sub3$Abundance[1]>=10 & sub3$Abundance[1]<100) {
        sub3$Definition.of.abundance.class[1]<-"~10-99"
        sub3$Abundance.class[1]<-"B"
      }
      if (sub3$Abundance[1]>=100 & sub3$Abundance[1]<1000) {
        sub3$Definition.of.abundance.class[1]<-"~100-999"
        sub3$Abundance.class[1]<-"C"
      }
      if (sub3$Abundance[1]>=1000 & sub3$Abundance[1]<10000) {
        sub3$Definition.of.abundance.class[1]<-"~1000-9999"
        sub3$Abundance.class[1]<-"D"
      }
      if (sub3$Abundance[1]>=10000 & sub3$Abundance[1]<100000) {
        sub3$Definition.of.abundance.class[1]<-"~10000-99999"
        sub3$Abundance.class[1]<-"E"
      }
      sub2<-rbind(sub2, sub3[1,])
    }
    sub2<-sub2[-1,]
    raw3<-rbind(raw3, sub2)
  } else { #if no multiples then keep as is
    raw3<-rbind(raw3, sub)
  }
  
  if (i/length(ids)>=loop.num) {
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}
raw3<-raw3[-1,]

colnames(raw3)
colnames(dat)
dat<-raw3[,c(1,4,7,8,9,12,13,14)]
colnames(dat)[4]<-"Taxon.name"
dat[,"Site_ID_Ellen"]<-NA

write.csv2(dat, "SCO_061_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")
