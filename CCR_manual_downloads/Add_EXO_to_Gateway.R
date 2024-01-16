#Combine the manual EXO data into the streamed file and push to Annie
#by ABP 18 NOV 21

pacman::p_load("tidyverse", "lubridate","rqdatatable")

#Get up to date collated EXO file

CATPRES_COL_NAMES = c("DateTime", "RECORD", "CR3000Battery_V", "CR3000Panel_Temp_C", 
                      "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                      "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                      "ThermistorTemp_C_9","ThermistorTemp_C_10","ThermistorTemp_C_11", "ThermistorTemp_C_12",
                      "ThermistorTemp_C_13","EXO_Date_1", "EXO_Time_1", "EXOTemp_C_1", "EXOCond_uScm_1",
                      "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                      "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1",
                      "EXOPressure_psi_1", "EXODepth_m_1", "EXOBattery_V_1", "EXOCablepower_V_1", "EXOWiper_V_1",
                      "EXO_Date_9", "EXO_Time_9", "EXOTemp_C_9", "EXOCond_uScm_9",
                      "EXOSpCond_uScm_9", "EXOTDS_mgL_9", "EXODOsat_percent_9", "EXODO_mgL_9", 
                      "EXOfDOM_RFU_9", "EXOfDOM_QSU_9","EXOPressure_psi_9", "EXODepth_m_9", "EXOBattery_V_9",
                      "EXOCablepower_V_9", "EXOWiper_V_9","LvlPressure_psi_13", "LvlTemp_C_13")


ccrwaterdata <- read_csv("./CCR_manual_downloads/CCRWaterquality_L1_2021_2023.csv", skip = 1, col_names = CATPRES_COL_NAMES,
                         col_types = cols(.default = col_double(), DateTime = col_datetime()))
EXO <-  read_csv("./CCR_manual_downloads/CCR_1_5_EXO_2021_2023.csv", col_names=T,
              col_types = cols(.default = col_double(), DateTime = col_datetime()), show_col_types = T)

# rename the EXO Time 
#Take out the columns that don't match
EXO <- EXO%>%
  select(-any_of(c("nLF.Cond.uS.cm", "Vertical.Position.m")))

#Join the CCR-waterquality data and the EXO data
CCR<- natural_join(ccrwaterdata, EXO, 
                          by = "DateTime",
                          jointype = "LEFT")

#name of the columns to get in the right order
wQnames=colnames(ccrwaterdata)

#rearrange the column headers based on the original file since they get jumbled during the join 
CCR=CCR%>%
  select(wQnames)

#Always remember to go and change the NAN to "NAN". I do this is notepad

write.csv(CCR, "CCRCatwalk_EXO_manual_2021_2023.csv",na="NAN", row.names = FALSE)

