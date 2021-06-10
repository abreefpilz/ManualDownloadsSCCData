#script written to combine manual downlaods to fill in gaps of missing data from the gateway
#written by CCC, BB, Vahid-Dan, ABP
#09 JUN 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)

#time to now play with BVR data!
#Gateway has missing data sections so combine manual data for EDI

#put manual data from BVR platform into a file 
mydir = "CR6_Files/Catwalk"
myfiles = list.files(path=mydir, pattern="", full.names=TRUE)#list the files from BVR platform

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

#create CSV of manual downloads which can be combined with Github files to fill in missing gaps

write.csv(bvrdata, "BVRplatform_manual_2020.csv")