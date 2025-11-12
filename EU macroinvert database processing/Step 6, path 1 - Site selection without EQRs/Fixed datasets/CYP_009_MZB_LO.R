dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1", sep=";")
dat$Temp.ID<-paste(dat$Site.ID, dat$Sampling.date, sep="_")
ids<-unique(dat$Temp.ID)

dat2<-dat[1,]
for (i in 1:length(ids)) {
  sub<-subset(dat, dat$Temp.ID==ids[i])
  names<-unique(sub$Taxon.name)
  for (j in 1:length(names)) {
    sub2<-subset(sub, sub$Taxon.name==names[j])
    sub2$Sample.ID[1]<-NA
    sub2$Abundance<-sum(sub2$Abundance)
    dat2<-rbind(dat2, sub2[1,])
  }
}
dat2<-dat2[-1,]
dat2<-dat2[,-10]

write.csv2(dat2, "CYP_009_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")
