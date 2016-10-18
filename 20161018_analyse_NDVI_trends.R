library(devtools)
install_github('dutri001/bfastSpatial')
install_github('bendv/rgrowth')
library(rgrowth)
require(bfast)
require(zoo)

setwd('R:/ZAB/kb308/GEE_RF')
fils<-list.files()

for (fil in fils){
	if(grep('csv',fil)>0){
	dat<-read.csv(fil)
	filSplt<-unlist(strsplit(fil,'_'))
	dates<-unlist(lapply(as.character(dat$system.index), function(x) unlist(strsplit(x,'_'))))
	dat$obsDat<-dates[which(nchar(dates)>1)]
	#dat$obsDat<-unlist(strsplit(as.character(dat$system.index),'_'))[6]
	#dat$obsDat<-as.Date(dat$obsDat,format = '%Y%m%d')
	dat$.geo<-as.character(dat$.geo)
	dat$site<-as.numeric(substr(dat$system.index,0,1))
	startDate<-as.Date(filSplt[1],format = '%Y-%m-%d')
	endDate<-as.Date(filSplt[2],format = '%Y-%m-%d')
	
	for (d in unique(dat$obsDat)){
		dat1<-dat[which(dat$site == 1 & dat$obsDat == d),]
		lat1<-unlist(strsplit(unlist(strsplit(dat1$.geo,'"'))[9],','))[1]
		lat1<-as.numeric(substr(lat1,3,nchar(lat1)))
		lon1<-unlist(strsplit(unlist(strsplit(dat1$.geo,'"'))[9],','))[2]
		lon1<-as.numeric(substr(lon1,0,nchar(lon1)-2))

		dat2<-dat[which(dat$site == 2 & dat$obsDat == d),]
		lat2<-unlist(strsplit(unlist(strsplit(dat2$.geo,'"'))[9],','))[1]
		lat2<-as.numeric(substr(lat2,3,nchar(lat2)))
		lon2<-unlist(strsplit(unlist(strsplit(dat2$.geo,'"'))[9],','))[2]
		lon2<-as.numeric(substr(lon2,0,nchar(lon2)-2))

		NDVIdiff<-dat1$mean-dat2$mean
		datDf<-data.frame(d,lon1,lon2,lat1,lat2,NDVIdiff)
		datDf$startDate<-startDate
		datDf$endDate<-endDate
		if(exists('NDVIdf') == F){
			NDVIdf<-datDf
		} else {
			NDVIdf<-rbind(NDVIdf, datDf)
		}	
	}

	if(exists('newDf')==F){
		newDf<-NDVIdf
	} else {
		newDf<-rbind(newDf,NDVIdf)
	}
	rm(datDf,NDVIdf)

	#NEXT UP NAME POINT PAIRS
}
}

#uniquePairs<-unique(newDf[c("lon1", "lon2","lat1","lat2")])
ID<-paste(newDf$lon1,newDf$lon2, sep = '')
ID<-paste(ID, newDf$lat1, sep = '')
ID<-paste(ID, newDf$lat2, sep ='')
newDf$ID<-ID
rm(ID)
no<-1
for (ID in unique(newDf$ID)){
	newDf$ID[which(newDf$ID == ID)]<-no
	no<- no+1
}
rm(ID, no)

newDf$d<-as.Date(newDf$d, format = '%Y%m%d')
#########################################
#BFAST STUFF

idIter<-1
siteData<-newDf[which(newDf$ID == idIter),]

## From 'numeric' to 'zoo' class
x <- zoo(siteData$NDVIdiff, siteData$d)

#z <- aggregate(zoo(siteData$NDVIdiff), as.Date(siteData$d), tail, 1)
#tst<-merge(z, zoo(, time(as.ts(z))), fill = 0)
#bfData<-merge(x, zoo(, time(as.ts(x))), fill = 0)

bts <- bfastts(x, dates = time(x), type = 'irregular')

bfm <- bfastmonitor(bts, start = c(2000, 1), formula = response~harmon, order = 1, plot = TRUE)

#http://bendevries.ca/rgrowth/
