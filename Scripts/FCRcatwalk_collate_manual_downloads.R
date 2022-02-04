#script written to combine manual downlaods to fill in gaps of missing data from the gateway
#written by CCC, BB, Vahid-Dan, ABP
#09 JUN 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)

#read in the collated manual file that we are adding to
FCRman_data=read.csv("CR6_Files/FCRcatwalk_manual_2021.csv")


#time to now play with FCR data!
#Gateway has missing data sections so combine manual data for EDI

#put manual data from FCR platform into a file 
mydir = "CR6_Files/Catwalk"
myfiles = list.files(path=mydir, pattern="CR6_FCRcatwalk_Catwalk_20210726", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
fcrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfiles)){
  fcrheader2<-read.csv(myfiles[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  fcrdata2<-read.csv(myfiles[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(fcrdata2)<-names(fcrheader2) #combine the names to deal with Campbell logger formatting
  fcrdata3=rbind(fcrdata2, fcrdata3)
}

#get rid of duplicates
fcrdata=fcrdata3[!duplicated(fcrdata3$TIMESTAMP), ]

#put manual data from FCR pressure sensor into a file 
mydir2 = "CR6_Files/Pressure_Sensor"
myfiles2 = list.files(path=mydir2, pattern="", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
presdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(j in 1:length(myfiles2)){
  presheader2<-read.csv(myfiles2[j], skip=1, as.is=T) #get header minus wonky Campbell rows
  presdata2<-read.csv(myfiles2[j], skip=4, header=F) #get data minus wonky Campbell rows
  names(presdata2)<-names(presheader2) #combine the names to deal with Campbell logger formatting
  presdata3=rbind(presdata2, presdata3)
}

#get rid of duplicates
presdata=presdata3[!duplicated(presdata3$TIMESTAMP), ]

#merge the catwalk files and the pressure sensor

FCR=merge(fcrdata,presdata, all.x=T)

FCR=rbind(FCRman_data,fcrdata3)

FCR2=FCR%>%
  filter(TIMESTAMP!="")

#create CSV of manual downloads which can be combined with Github files to fill in missing gaps

write.csv(FCR2, "CR6_Files/FCRcatwalk_manual_2021.csv", row.names=FALSE)
