samps<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
org<-dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

dat2<-subset(dat, dat$abundance %nin% NA)

#Site.ID	Sample.ID	Sampling.date	Taxon.name	Taxon.ID	Abundance	Definition.of.abundance.class	Abundance.class	Site_ID_Ellen
dat2$Site.ID<-NA
dat2$Sampling.date<-NA
dat2$Site_ID_Ellen<-NA

for (i in 1:length(samps$sample_id)) {
  rnums<-which(dat2$sample_id==samps$sample_id[i])
  dat2$Sampling.date[rnums]<-samps$sampling_date[i]
  dat2$Site.ID[rnums]<-samps$site_id[i]
}

colnames(dat2)[c(1,2,3,4)]<-c("Sample.ID","Taxon.ID","Taxon.name","Abundance")

site<-unique(dat2$Site.ID)
for (i in 1:length(site)) {
  rnum<-which(org$Site_ID_original==site[i] & org$Data_owner=="Lenka Kuglerová")
  rnums<-which(dat2$Site.ID==site[i])
  if (length(rnum)>0) {
    dat2$Site_ID_Ellen[rnums]<-org$Ellens_ID[rnum]
  }
}

dat3<-subset(dat2, dat2$Site_ID_Ellen %nin% NA)
dat3$sampling.method<-NA

samp<-unique(dat3$Sample.ID)
for (i in 1:length(samp)) {
  rnums<-which(dat3$Sample.ID==samp[i])
  rnum<-which(samps$sample_id==samp[i])
  dat3$sampling.method[rnums]<-samps$sampling_method[rnum]
}

dat3$Definition.of.abundance.class<-NA
dat3$Abundance.class<-NA
colnames(dat3)
dat4<-dat3[,c(5,1,6,3,2,4,9,10,7,8)]

write.csv2(dat4, "SWE_039_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")
