#script written to coilate the BVR data before the gateway was up and running
#written by CCC, BB, Vahid-Dan, ABP
#10 DEC 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(tidyverse)


#put manual data from BVR platform into a file 
mydir = "BVRPlatform"
myfiles = list.files(path=mydir, pattern="_202[0-9]", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*|manual|collate", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
bvrdata3<-NULL

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfilesBVR)){
  bvrheader2<-read_csv(myfilesBVR[k], skip=1) #get header minus wonky Campbell rows
  bvrdata2<-read_csv(myfilesBVR[k], skip=3) #get data minus wonky Campbell rows
  names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting
  bvrdata2 <- bvrdata2%>%
    dplyr::rename(any_of(c(Turbidity_FNU_1="Turb_1")))
  bvrdata3=bind_rows(bvrdata2, bvrdata3)
}

#Take out the blank row
# bvrdata3=bvrdata3%>%
#   filter(TIMESTAMP!="")

#taking out the duplicate values  
obs1=distinct(bvrdata3)

# reorder the from oldest to most recent observation
obs1<-obs1[order(obs1$TIMESTAMP),]

# make sure no time duplicates. 
 current_df<-  obs1[duplicated(obs1$TIMESTAMP), ]

write.csv(obs1, "BVRPlatform/BVR_manual_2020_2023.csv", row.names=FALSE)
