#merge previous metdata

mydir = "MetStation"
myfiles = list.files(path=mydir, pattern="", full.names=TRUE)#list the files from BVR platform


#create dataframe for the for loop
out.file<-""


#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfiles)){
  files<-read.csv(myfiles[k],skip= 4, header=F) #get header minus wonky Campbell rows
  if(length(names(files))==17){
    names(files) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
                     "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
                     "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
                     "ShortwaveRadiationDown_Average_W_m2", "InfraredRadiationUp_Average_W_m2",
                     "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2")#rename headers
  }
  if(length(names(files))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
    files$V17<-NULL #remove column
    names(files) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
                     "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
                     "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
                     "ShortwaveRadiationDown_Average_W_m2", "InfraredRadiationUp_Average_W_m2",
                     "InfraredRadiationDown_Average_W_m2", "Albedo_Average_W_m2")#rename headers
  }
  out.file=rbind(out.file, files)
}

out.file=out.file%>%filter(Record!="")
