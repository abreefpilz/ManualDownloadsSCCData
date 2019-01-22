#Script to work with Magic data from Fall 2018

#Let's see if anything is worth anything

#Tasks:
#1. Post acid cleaning (October 10th ish), plot pre and post cleaning air valve
#2. Plot DI valve
#3. Plot Surface valve
#4. Compare magic data to DO + temp data at 1, 5, and 9m

#loading in data
setwd('./MagicData')

###putting in met merge script as place holder
metfiles<-list.files(path=getwd())
obs<-read.csv(file=metfiles[1],skip=4,header=FALSE) #read in first file
names(obs) = c("TIMESTAMP","RECORD","BattV","PTemp_C","PAR_Den_Avg","PAR_Tot_Tot","BP_kPa_Avg","AirTC_Avg","RH","Rain_mm_Tot","WS_ms_Avg","WindDir","SR01Up_Avg","SR01Dn_Avg","IR01UpCo_Avg","IR01DnCo_Avg","Albedo_Avg")
units = c("TS","RN","Volts","Deg C","umol/s/m^2","mmol/m^2","kPa","Deg C","%","mm","meters/second","degrees","W/m^2","W/m^2","W/m^2","W/m^2","W/m^2") #creates list of units, skipped in line above
obs$TIMESTAMP = as.POSIXct(obs$TIMESTAMP)

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

setwd("..") #goes up one directory so that metadata file is not written to /MetStationData

#write.csv(obs,"AllRawMetData.csv", row.names = FALSE) #create csv with all raw appended data
#this is the LEVEL_ZERO dataset: i.e., no QA/QC applied, complete dataset but has errors
