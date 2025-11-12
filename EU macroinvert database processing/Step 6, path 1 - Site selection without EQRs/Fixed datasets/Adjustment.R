library(Hmisc)

taxa.names[i]
samps[which(nums==1)]

dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1", sep=";")
dat2<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
dat2<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1", sep=";")


sub<-subset(dat2, dat2$Sample.ID=="114000006_21.04.2015")
sub<-subset(dat, dat$Sample.ID==131986 & dat$Abundance %nin% NA)
sub<-subset(dat, dat$Site.ID==46 & dat$Sampling.date=="17.09.2014")
sub<-sub[order(sub$Taxon.name),]

sub<-subset(dat, dat$Site.ID==2 & dat$Sampling.date=="01.10.1987" & dat$Abundance %nin% NA)
table(sub$Taxon.name)

write.csv(dat, "ITA_025_MZB_LO2.csv", row.names=FALSE, fileEncoding = "latin1")
write.csv2(dat, "ITA_025_MZB_LO2.csv", row.names=FALSE, fileEncoding = "latin1")

#[1] "ENG_043_MZB_LO_6-27.05.2009" "ENG_043_MZB_LO_6-21.04.2010" "ENG_043_MZB_LO_7-28.05.2003"
#[4] "ENG_043_MZB_LO_7-01.05.2009" "ENG_043_MZB_LO_7-29.04.2010"

options(warn=1)
dat.taxa$Abundance<-as.numeric(gsub(",", replacement="\\.", dat.taxa$Abundance))
dat$Abundance[which(dat.taxa$Abundance %in% NA)]
