## Compiling CCR hobos 
## DWH and CAG
## April 2022
## This script compiles all the HOBO logger files from 2020-2021, puts them in one file, plots the observations, removes duplicates, and saves the file. 

##load libraries 
library(tidyverse)
library(lubridate)


#### Read in and format each csv file by depth/location ####

##### HOBO 3 / 0.1m 
#read in csv 2021-08-25, we're skipping the first since that's just one column with the Hobos name; it's good to first read in without the skip to check the format 
hobo3_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_hobos/hobo3_CCR_0.1m_20210825.csv", skip = 1, check.names = F)

#view 
view(hobo3_0825)

#example code for steps I took to figure out how to covert datetime to a datetime (POSIXt) format and how to convert Temp to C 
# hobo3_0825 <- hobo3_0825 %>% 
#   select(-X.) %>% 
#   rename(DateTime = 1,
#          Temp_F = 2) %>% 
#   mutate(datetimetest = mdy_hms(DateTime)) %>% 
#   mutate(Temp_C = (Temp_F - 32) * (5/9) )

#code for actually cleaning up the data 
hobo3_0825 <- hobo3_0825 %>% 
  rename(DateTime = 2,
         Temp_F = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  mutate(Temp_C = (Temp_F - 32) * (5/9) ) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 0.1)

#2021-04-19
hobo3_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo3_CCR_0.1m_202190419.csv", skip = 1, check.names = F)
view(hobo3_0419)

hobo3_0419 <- hobo3_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 0.1)

#Join hobo3/0.1m 
hobo3_0.1m <- rbind(hobo3_0419, hobo3_0825)


#### From here keep following this format for each hobo # / depth, once we have all depths formated like 'hobo3_0.1m'above then we'll join all toghether. 
#### Some notes to keep in mind. 1) check the JPEG in the folder to make sure the hobo #s are assigned to the right depth. 
####                    2) Check the columns each time to make sure the right column is being renamed and if Temp units need to be changed
####                    3) The 7m Hobo has had two numbers 24 and 48
####                    4) all Hobos that have a depth should be site 50 and there corresponding depth 
####                    5) Hobo 65 was deployed in one of the inflow streams so it's Site = 300 and depth = 0.1

#all numbers lined up with depths however, 21m is 21.45 in image
#also some only had 1 hobo file or 3. 

#2021-04-19
hobo5_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo5_CCR_1m_20210419.csv", skip = 1, check.names = F)
view(hobo5_0419)

hobo5_0419 <- hobo5_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 1.0)

#2021-08-25
hobo5_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo5_CCR_1m_20210825.csv", skip = 1, check.names = F)
view(hobo5_0825)

hobo5_0825 <- hobo5_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 1.0)

#Join hobo5/1m 
hobo5_1m <- rbind(hobo5_0419, hobo5_0825)


#2m 2021-04-19
hobo7_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo7_CCR_2m_20210419.csv", skip = 1, check.names = F)
view(hobo7_0419)

hobo7_0419 <- hobo7_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 2)

#2m 2021-08-25
hobo7_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo7_CCR_2m_20210825.csv", skip = 1, check.names = F)
view(hobo7_0825)

hobo7_0825 <- hobo7_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 2)

#Join hobo7/2m 
hobo7_2m <- rbind(hobo7_0419, hobo7_0825)


#3m 2021-04-19
hobo9_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo9_CCR_3m_20210419.csv", skip = 1, check.names = F)
view(hobo9_0419)

hobo9_0419 <- hobo9_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 3)

#3m 2021-08-25
hobo9_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo9_CCR_3m_20210825.csv", skip = 1, check.names = F)
view(hobo9_0825)

hobo9_0825 <- hobo9_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 3)

#Join hobo9/3m 
hobo9_3m <- rbind(hobo9_0419, hobo9_0825)

#5m 2021-04-19
hobo10_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo10_CCR_5m_20210419.csv", skip = 1, check.names = F)
view(hobo10_0419)

hobo10_0419 <- hobo10_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 5)

#5m 2021-08-25
hobo10_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo10_CCR_5m_20210825.csv", skip = 1, check.names = F)
view(hobo10_0825)

hobo10_0825 <- hobo10_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 5)

#Join hobo10/5m 
hobo10_5m <- rbind(hobo10_0419, hobo10_0825)


#4m 2021-04-19
hobo19_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo19_CCR_4m_20210419.csv", skip = 1, check.names = F)
view(hobo19_0419)

hobo19_0419 <- hobo19_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 4)

#4m 2021-08-25
hobo19_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo19_CCR_4m_20210825.csv", skip = 1, check.names = F)
view(hobo19_0825)

hobo19_0825 <- hobo19_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 4)

#Join hobo19/4m 
hobo19_4m <- rbind(hobo19_0419, hobo19_0825)


#6m 2021-04-19
hobo20_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo20_CCR_6m_20210419.csv", skip = 1, check.names = F)
view(hobo20_0419)

hobo20_0419 <- hobo20_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 6)

#rename 
hobo20_6m <- hobo20_0419


#7m 2021-04-19
hobo24_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo24_CCR_7m_20210419.csv", skip = 1, check.names = F)
view(hobo24_0419)

hobo24_0419 <- hobo24_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 7)

#7m 2021-08-25
hobo48_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo48_readout_20210825.csv", skip = 1, check.names = F)
view(hobo48_0825)

hobo48_0825 <- hobo48_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 7)


#Join hobo24_48/7m 
hobo24_48_7m <- rbind(hobo24_0419, hobo48_0825)


#8m 2021-04-19
hobo25_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo25_CCR_8m_20210419.csv", skip = 1, check.names = F)
view(hobo25_0419)

hobo25_0419 <- hobo25_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 8)

#8m 2021-08-25
hobo25_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo25_CCR_8m_20210825.csv", skip = 1, check.names = F)
view(hobo25_0825)

hobo25_0825 <- hobo25_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 8)

#Join hobo25/8m 
hobo25_8m <- rbind(hobo25_0419, hobo25_0825)


#9m 2021-04-19
hobo26_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo26_CCR_9m_20210419.csv", skip = 1, check.names = F)
view(hobo26_0419)

hobo26_0419 <- hobo26_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 9)

#9m 2021-08-25
hobo26_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo26_CCR_9m_20210825.csv", skip = 1, check.names = F)
view(hobo26_0825)

hobo26_0825 <- hobo26_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 8)

#Join hobo26/9m 
hobo26_9m <- rbind(hobo26_0419, hobo26_0825)


#10m 2021-04-19
hobo33_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo33_CCR_10m_20210419.csv", skip = 1, check.names = F)
view(hobo33_0419)

hobo33_0419 <- hobo33_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 10)

#10m 2021-08-25
hobo33_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/Hobo33_CCR_10m_20210825.csv", skip = 1, check.names = F)
view(hobo33_0825)

hobo33_0825 <- hobo33_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 10)

#Join hobo33/10m 
hobo33_10m <- rbind(hobo33_0419, hobo33_0825)



#15m 2021-04-19
hobo35_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo35_CCR_15m_20210419.csv", skip = 1, check.names = F)
view(hobo35_0419)

hobo35_0419 <- hobo35_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 15)

#15m 2021-08-25
hobo35_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/Hobo35_CCR_15m_20210825.csv", skip = 1, check.names = F)
view(hobo35_0825)

hobo35_0825 <- hobo35_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 15)

#Join hobo35/15m 
hobo35_15m <- rbind(hobo35_0419, hobo35_0825)


#20m 2021-04-19 NO DATA
hobo36_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo36_CCR_20m_20210419.csv", skip = 1, check.names = F)
view(hobo36_0419)

hobo36_0419 <- hobo36_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 20)

#20m 2021-08-25
hobo36_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/Hobo36_CCR_20m_20210825.csv", skip = 1, check.names = F)
view(hobo36_0825)

hobo36_0825 <- hobo36_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 20)

#Join hobo36/20m 
hobo36_20m <- rbind(hobo36_0419, hobo36_0825)


#21m 2021-04-19
hobo44_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/hobo44_CCR_21m_20210419.csv", skip = 1, check.names = F)
view(hobo44_0419)

hobo44_0419 <- hobo44_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 21)

#21m 2021-08-25
hobo44_0825 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/Hobo44_CCR_21m_20210825.csv", skip = 1, check.names = F)
view(hobo44_0825)

hobo44_0825 <- hobo44_0825 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 50,
         Depth_m = 21)

#Join hobo44/21m 
hobo44_21m <- rbind(hobo44_0419, hobo44_0825)


#Inflow
#0.1m 2021-04-19
hobo65_0419 <- read.csv("./CCR_manual_downloads/CCR_HOBOs/CCR_Hobos/Hobo65_CCR_inflow_300_20210419.csv", skip = 1, check.names = F)
view(hobo65_0419)

hobo65_0419 <- hobo65_0419 %>% 
  rename(DateTime = 2,
         Temp_C = 3) %>% 
  mutate(DateTime = mdy_hms(DateTime)) %>% 
  select(DateTime, Temp_C) %>% 
  mutate(Site = 300,
         Depth_m = 0.1)

#rename 
hobo65_Site_300_0.1m <- hobo65_0419


Hobo_Compiled <- rbind(hobo3_0.1m, hobo5_1m, hobo7_2m, hobo9_3m, hobo19_4m, hobo10_5m, hobo20_6m, hobo24_48_7m, hobo25_8m, hobo26_9m, hobo33_10m, hobo35_15m, hobo44_21m, hobo65_Site_300_0.1m)
view(Hobo_Compiled)
head(Hobo_Compiled)

Hobo_Compiled <- Hobo_Compiled %>% 
  mutate(Reservoir = "CCR") %>% 
  select(Reservoir, Site, DateTime, Depth_m, Temp_C)
head(Hobo_Compiled)

#viewing data
Hobo_Compiled_plot <-Hobo_Compiled %>% 
  filter(Site != 300) %>% 
  ggplot( mapping = aes(x = DateTime, y = Temp_C, color = Depth_m))+
 geom_point() 

Hobo_Compiled_plot

#filter out values when Hobos were out of water 

Hobo_Compiled_Filtered <- Hobo_Compiled %>%    #################### FIX 4-19 issues
  filter(DateTime <= dmy_hms("19-08-2021 00:00:00")) %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  mutate(Temp_C = ifelse(Date == dmy("19-04-2021"), NA, Temp_C)) 
                        

Hobo_Compiled_colorbydepth <-Hobo_Compiled_Filtered %>% 
  filter(Site != 300) %>% 
  ggplot( mapping = aes(x = DateTime, y = Temp_C, color = Depth_m))+
  geom_point() 

Hobo_Compiled_colorbydepth 

inflow_plot <- Hobo_Compiled_Filtered %>% 
  filter(Site == 300) %>% 
  ggplot( mapping = aes(x = DateTime, y = Temp_C))+
  geom_point() 

inflow_plot  

#make final csv for export
head(Hobo_Compiled_Filtered)

ccr_hobos_final <- Hobo_Compiled_Filtered %>% 
  select(Reservoir, Site, DateTime, Depth_m, Temp_C) %>% 
  filter(!is.na(Temp_C)) %>% 
  mutate(Flag = 0) %>% 
  distinct()

write.csv(ccr_hobos_final, "./CCR_manual_downloads/CCR_HOBOs/CCR_hobos_20_21.csv", row.names = FALSE)  


  
