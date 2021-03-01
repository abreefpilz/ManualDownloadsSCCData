#### Script for exploratory data analysis of MUX predictions and potential covariates ####
### Author: Nick Hammond
### Last Edited: 02/09/2021

# Set wd, load packages, source code
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)
library(readxl)
library(pls) 
library(scales)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

setwd("C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/")





# Read in catwalk data
catwalk = read.csv(paste0(getwd(),"/Data/Covariate data/Catwalk_EDI_2020.csv"))

# Read in MUX PLSR predictions
MUX_preds = read.csv(paste0(getwd(),"/Raw_predictions/MUX_Oct_Nov_2020_predictions_020921.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime)

# Select the variables we want
catwalk_exp = catwalk %>% select(Reservoir,Site,DateTime,RDO_mgL_5,RDOsat_percent_5,RDO_mgL_5_adjusted,RDOTemp_C_5,
                                 RDO_mgL_9,RDOsat_percent_9,RDO_mgL_9_adjusted,RDOsat_percent_9_adjusted,RDOTemp_C_9,
                                 EXOTemp_C_1,EXODOsat_percent_1,EXODO_mgL_1)
# Convert DateTime to PosixCT
catwalk_exp$DateTime = mdy_hm(catwalk_exp$DateTime)

# Select the reservoir, site, and date range we want
catwalk_exp = catwalk_exp %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-11-03 12:00") %>%
  filter(DateTime<"2020-11-09 14:00")

# Select date range for MUX predictions
MUX_preds = MUX_preds %>%
  filter(DateTime>"2020-11-03 12:00") %>%
  filter(DateTime<"2020-11-09 14:00")

# Merge MUX metals predictions dataframe with catwalk dataframe
#combined = merge(MUX_preds,catwalk_exp,by="DateTime",all = TRUE)


nine_m = MUX_preds %>% filter(Depth == 9.0)



# Value used to transform the data
coeff_r <- (4)

# A few constants
FeColor <- "#69b3a2"
DOColor <- rgb(0.2, 0.6, 0.9, 1)

png('MUX_Fe_DO_post_turnover.png', width = 15, height = 12, units = 'in', res = 300)
DO_plot = ggplot() +
  
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgl), size=2, color=FeColor) + 
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted/coeff_r), size=2, color=DOColor) +
  
  xlab("Date") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "TFe_mgL",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="DO (mg/L)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = FeColor, size=25),
    axis.title.y.right = element_text(color = DOColor, size=25)
  ) 

#ggtitle(" ISCO Discharge and Fe Loads 2018")

DO_plot

dev.off()







# alternate code
DO_plot <- ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgl)) +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted), colour="blue") +
  #ylim(0, 0.5)+
  labs(x="Date", y = "TFe_mgl", title = "Predicted TFe") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")
DO_plot

