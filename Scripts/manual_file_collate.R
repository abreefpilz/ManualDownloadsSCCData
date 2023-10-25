#' 
#' @author Adrienne BP
#' @title manual_file_collate
#' @description This function reads in all of the raw files for the current year and binds them together 
#' so they can be used for filling missing chunks of streaming data
#' 
#' @param raw_files directory where raw files from the data logger or instrument are stored
#' @param year year of the files you want to collate. The default is the current year
#' @param just_CCR_EXO a True/False switch if the files are from the EXO at Carvins Cove 
#' @param outfile The name of the L1 compiled file with all observations for the year
#'
#' @return no output
#'

pacman::p_load(tidyverse, lubridate)
  
  
manual_file_collate <- function(raw_files = "../../ManualDownloadsSCCData",
                     year = format(Sys.Date(), "%Y"),
                     just_CCR_EXO = F, 
                     outfile = "../../fileL1"){
  
  # List files based on current year
  
  myfiles = list.files(path=raw_files, pattern=paste0("_",year), full.names=TRUE)
  
  # Take out manual files
  files <- myfiles[ !grepl("BVR_manual_*", myfiles) ]
  
  # Add an if statement if this is just the EXO
  
  if(just_CCR_EXO==T){
    
    # Merge the EXO files for CCR
    
    CCR<-files %>%
      map_df(~ read.csv(.x, header=T, skip=17, skipNul = TRUE, check.names = F))
    
    
    #clean up the created file
    CCR_1_5_EXO=CCR%>%
      select(-c("Site Name","Time (Fract. Sec)","ODO % local","Sal psu", 
                "Vertical Position m", "nLF Cond \xb5S/cm"))%>%
      unite(., col="TIMESTAMP", c("Date (MM/DD/YYYY)","Time (HH:mm:ss)"), sep=" ")
    
    
    # Create a column to say if the observation was in EDT or EST because the EXO observes daylight savings
    CCR_1_5_EXO$Date = dst(parse_date_time(CCR_1_5_EXO[,"TIMESTAMP"], 'mdy HMS', tz="America/New_York"))
    
    # Give datetime the EST timestamp before converting
    CCR_1_5_EXO$TIMESTAMP = parse_date_time(CCR_1_5_EXO$TIMESTAMP, 'mdy HMS', tz="Etc/GMT+5")
    
    # Use ifelse statement to convert EDT to EST
    
    all<-CCR_1_5_EXO%>%
      mutate(TIMESTAMP=ifelse(Date==T, with_tz(force_tz(TIMESTAMP-3600,"Etc/GMT+4"), "Etc/GMT+5"), TIMESTAMP),
        TIMESTAMP=as.POSIXct(TIMESTAMP, origin="1970-01-01"))%>%
      select(-Date)
  
    
    #Naming the header to match what is on the data logger
    colnames(all)=c("TIMESTAMP","Chla_RFU_1","Cond_1","EXO_depth_1","fDOM_QSU_1","fDOM_RFU_1",
                            "dosat_1","doobs_1","EXO_pressure_1","SpCond_1","BGAPC_RFU_1","TDS_1","EXO_wiper_1",
                            "EXO_wtr_1","EXO_battery_1","EXO_cablepower_1")
    
    
  }else{
    # Now merge them all together
    
    all<-files %>%
      map_df(~ read_csv(.x, skip = 1))%>%
      filter(grepl("^20", TIMESTAMP))%>% #keep only the right TIMESTAMP rows 
      distinct(TIMESTAMP, .keep_all= TRUE) 
    
  }
  
  # Write to a csv
  write.csv(all, paste0(raw_files, outfile, ".csv"), row.names = F)
  
}

# Write out the functions with arguments for all the manual download files

# # BVR
# manual_file_collate(raw_files = "./BVRPlatform", year="2023", just_CCR_EXO = F, outfile= "/BVRplatform_L1")
# 
# # FCR
# manual_file_collate(raw_files = "./CR6_Files/Catwalk", year="2023", just_CCR_EXO = F, outfile= "/FCRCatwalk_L1")
# 
# # FCR Met
# manual_file_collate(raw_files = "./MetStation", year="2023", just_CCR_EXO = F, outfile= "/FCRMet_L1")
# 
# # Weir 
# manual_file_collate(raw_files = "./WeirData", year="2023", just_CCR_EXO = F, outfile= "/WeirData_L1")
# 
# # CCR
# manual_file_collate(raw_files = "./CCR_manual_downloads/CCR_dam_downloads/Waterquality", year="2023", just_CCR_EXO = F, outfile= "/CCRWaterquality_L1")
# manual_file_collate(raw_files = "./CCR_manual_downloads/CCR_dam_downloads/Metstation", year="2023", just_CCR_EXO = F, outfile= "/CCRMetstation_L1")
# manual_file_collate(raw_files = "./CCR_manual_downloads/CCR_1_5_EXO_downloads", year="2023", just_CCR_EXO = T, outfile= "/CCR_1_5_EXO_L1")

