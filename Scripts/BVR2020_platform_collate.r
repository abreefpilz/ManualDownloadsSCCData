#script written to coilate the BVR data before the gateway was up and running
#written by CCC, BB, Vahid-Dan, ABP
#10 DEC 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)



#time to now play with BVR data!
#upload the current BVR data from GitHub
download.file('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv', "BVRplatform.csv") 

bvrheader1<-read.csv("BVRplatform.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata1<-read.csv("BVRplatform.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata1)<-names(bvrheader1) #combine the names to deal with Campbell logger formatting

#put manual data from BVR platform into a file 
mydir = "BVRPlatform"
myfiles = list.files(path=mydir, pattern="CR6_BVR*", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
bvrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfilesBVR)){
  bvrheader2<-read.csv(myfilesBVR[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  bvrdata2<-read.csv(myfilesBVR[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting
  bvrdata3=rbind(bvrdata2, bvrdata3)
}

#combine manual and most recent files
bvrdata=rbind(bvrdata3, bvrdata1)

#taking out the duplicate values  
obs1=distinct(bvrdata)



#change the date from character to unknown making it easier to graph
#obs1$TIMESTAMP <- as.POSIXct(obs1$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+4") 
