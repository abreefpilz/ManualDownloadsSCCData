#combinig EXO data which can then be used to add to the ccre-waterquality.csv file

library(lubridate)
library(tidyverse)
library(ggplot2)


#load in data from the EXO Sensor

#All files including the one from 08 Nov 21(last reading actually happened on 16:10EST ) is EDT 
#so need to change to EST

mydir = "CCR_manual_downloads/CCR_1_5_EXO_downloads"
myfiles = list.files(path=mydir, pattern="", full.names=TRUE)

#combine the files
#create an out.file for the combined data
out.file<-""
#for loop to combine the files
for(i in 1:length(myfiles)){
  file <- read.csv(myfiles[i], header=T, skip=17, skipNul = TRUE)
    if (length(names(file))<23){
      file$Wiper.Position.volt<-"NA"#remove extra column
    file=file%>%relocate(Wiper.Position.volt, .after=TDS.mg.L)
    }

  out.file <- rbind(out.file,file)
}


#clean up the created file
CCR_1_5_EXO=out.file%>%
  filter(Site.Name!="")%>%
  select(-c(Site.Name,Time..Fract..Sec.,ODO...local,Sal.psu,))%>%
  unite(., col="TIMESTAMP", c("Date..MM.DD.YYYY.","Time..HH.mm.ss."), sep=" ")

#change TIMESTAMP to EST and change format. Most likely will only have to be until 08 Nov 21 download
CCR_1_5_EXO$TIMESTAMP <- as.POSIXct(strptime(CCR_1_5_EXO$TIMESTAMP, format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+5"))#format
CCR_1_5_EXO$TIMESTAMP<-with_tz(force_tz(CCR_1_5_EXO$TIMESTAMP,"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

#Naming the header to match what is on the data logger
colnames(CCR_1_5_EXO)=c("TIMESTAMP","Chla_RFU_1","Cond_1","EXO_depth_1","fDOM_QSU_1","fDOM_RFU_1","nLF.Cond.µS.cm",
"dosat_1","doobs_1","EXO_pressure_1","SpCond_1","BGAPC_RFU_1","TDS_1","EXO_wiper_1",
"EXO_wtr_1","Vertical.Position.m","EXO_battery_1","EXO_cablepower_1")

         
#take out duplicates
CCR_1_5_EXO$TIMESTAMP=as.character(CCR_1_5_EXO$TIMESTAMP)

write.csv(CCR_1_5_EXO, "CCR_manual_downloads/CCR_1_5_EXO_downloads/Collated_CCR_1_5_EXO_.csv",na="NAN", row.names = FALSE)
