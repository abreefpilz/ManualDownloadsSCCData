#script written to coilate the BVR data before the gateway was up and running
#written by CCC, BB, Vahid-Dan, ABP
#10 DEC 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(tidyverse)


#put manual data from BVR platform into a file 
mydir = "BVRPlatform"
myfiles = list.files(path=mydir, pattern="CR6Series_BVRplatform_bvre*", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
bvrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfiles)){
  bvrheader2<-read.csv(myfiles[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  bvrdata2<-read.csv(myfiles[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting
  bvrdata3=rbind(bvrdata2, bvrdata3)
}

#Take out the blank row
bvrdata3=bvrdata3%>%
  filter(TIMESTAMP!="")

#taking out the duplicate values  
obs1=distinct(bvrdata3)

write.csv(obs1, "BVRPlatform/BVR_manual_2021.csv", row.names=FALSE)
