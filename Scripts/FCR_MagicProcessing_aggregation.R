#Script to work with Magic data from Fall 2018

#Let's see if anything is worth anything

#Tasks:
#1. Load in data in legit way
#2. Post acid cleaning (October 10th ish), plot pre and post cleaning air valve (what is the best absorbance col to use?)
#3. Plot DI valve
#4. Plot Surface valve
#5. Compare magic data to DO + temp data at 1, 5, and 9m

#packages need
library(lubridate)

#loading in data
setwd('./MagicData')

#create list with files after cleaning protocol
magicfiles<-list.files(path=".", pattern = "810*")
#this isn't working how I want. Will fix by limiting time for now
fp.files <- magicfiles[grep(".FP", magicfiles, fixed=T)] #takes above list and keeps only FP file

#alternative loading. loads in all FP files. still limit by time after agg
#magicFPfiles<-list.files(magicfiles, pattern = ".FP")

###putting in met merge script as place holder
obs<-read.table(file=fp.files[1],skip=1,header=TRUE, row.names = NULL) #read in first file

#paste in actual 
names(obs) = c("Date","Time","Status","200.00","202.50","205.00","207.50","210.00","212.50","215.00","217.50","220.00","222.50","225.00","227.50","230.00")


for(i in 2:length(metfiles)){ #reads in all files within folder in Github
  temp<-read.csv(file=metfiles[i],skip=4,header=FALSE)
  if(length(names(temp))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
    temp$V17<-NULL #remove extra column
  }
  names(temp) = c("TIMESTAMP","RECORD","BattV","PTemp_C","PAR_Den_Avg","PAR_Tot_Tot","BP_kPa_Avg","AirTC_Avg","RH","Rain_mm_Tot","WS_ms_Avg","WindDir","SR01Up_Avg","SR01Dn_Avg","IR01UpCo_Avg","IR01DnCo_Avg","Albedo_Avg")
  temp$TIMESTAMP = as.POSIXct(temp$TIMESTAMP)
  obs<-rbind(obs,temp)
  #print(i)
}

for(i in 2:length(obs$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(obs$RECORD[i]-obs$RECORD[i-1]>1){
    print(c(obs$TIMESTAMP[i-1],obs$TIMESTAMP[i]))
  }
}

#limit data to after Oct 19 16:30

setwd("..") #goes up one directory so that metadata file is not written to /MetStationData

#write.csv(obs,"AllRawMetData.csv", row.names = FALSE) #create csv with all raw appended data
#this is the LEVEL_ZERO dataset: i.e., no QA/QC applied, complete dataset but has errors
