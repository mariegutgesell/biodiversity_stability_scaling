sub<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
sub2<-data.frame(Site.ID=sub$monitoring.site..prefix..BEVL_VMM_., Sample.ID=NA, Sampling.date=sub$Sampling.date, Taxon.name=sub$macroinvertebrate.taxon, Taxon.ID=NA, Abundance=sub$Number, Definition.of.abundance.class=NA, Abundance.class=NA, Site_ID_Ellen=NA, sampling.method=sub$sampling.method)

dates<-unique(sub2$Sampling.date)
for (i in 1:length(dates)) {
  rnums<-which(sub2$Sampling.date==dates[i])
  dates.name<-strsplit(dates[i], split="/")
  year<-as.numeric(dates.name[[1]][3])
  month<-as.numeric(dates.name[[1]][2])
  day<-as.numeric(dates.name[[1]][1])
  
  date<-as.POSIXlt(paste(day, month, year, sep="."), format="%d.%m.%Y")
  sub2$Sampling.date[rnums]<-as.character(format(date, "%d.%m.%Y"))
}
unique(sub2$sampling.method)

for (i in 1:length(unique(sub2$Site.ID))) {
  rnums<-which(sub2$Site.ID==unique(sub2$Site.ID)[i])
  rnum<-which(sites$Site_ID_original==unique(sub2$Site.ID)[i])
  if (length(rnum)>0) {
    sub2$Site_ID_Ellen[rnums]<-sites$Ellens_ID[rnum]
  }
}

sub2$Site.ID<-paste("BEVL_VMM_", sub2$Site.ID, sep="")

rnums<-which(sites$Country=="Belgium" & sites$Data_owner=="Marie Anne Eurie Forio" & sites$ecosystem=="lotic")
sub3<-subset(sub2, sub2$Site.ID %in% sites$Site_ID_original[rnums])
write.csv2(sub3, "BEL_005_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")

rnums<-which(sites$Country=="Netherlands" & sites$Data_owner=="Marie Anne Eurie Forio" & sites$ecosystem=="lotic")
sub3<-subset(sub2, sub2$Site.ID %in% sites$Site_ID_original[rnums])
write.csv2(sub3, "NLD_005_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")

rnums<-which(sites$Country=="Belgium" & sites$Data_owner=="Marie Anne Eurie Forio" & sites$ecosystem=="lentic")
sub3<-subset(sub2, sub2$Site.ID %in% sites$Site_ID_original[rnums])
write.csv2(sub3, "BEL_005_MZB_LE.csv", row.names=FALSE, fileEncoding = "latin1")