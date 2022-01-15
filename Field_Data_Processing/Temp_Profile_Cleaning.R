# Project: Modeling lake mixing dynamics with submersed macrophytes, energy dissipation by biomass
# Last Modified 14 January 2022
# Contributors: Ellen Albright, Robert Ladwig, Grace Wilkinson, Tyler Butts
# Description: Data cleaning - high frequency temperature profile data

# Data citation: ______________

# The cleaning code produces the following data tables:
#   1. Pond F, Deep Site: 
#      "temp_hf_Fdeep_wide.txt" - text file of high frequency temperature profile in wide format for use in rLakeAnalyzer
#      "temp_hf_Fdeep.csv" - high frequency temperature profile data, long format
#   2. Pond B, Deep Site:
#      "temp_hf_Bdeep_wide.txt" - text file of high frequency temperature profile in wide format for use in rLakeAnalyzer
#      "temp_hf_Bdeep.csv" - high frequency temperature profile data, long format
#   3. Pond F, Shallow Site:
#      "temp_hf_Fshallow_wide.txt" - text file of high frequency temperature profile in wide format for use in rLakeAnalyzer
#      "temp_hf_Fshallow.csv" - high frequency temperature profile data, long format
#   4. Pond B, Shallow Site:
#      "temp_hf_Bshallow_wide.txt" - text file of high frequency temperature profile in wide format for use in rLakeAnalyzer
#      "temp_hf_Bshallow.csv" - high frequency temperature profile data, long format
#   5. Pond F, Middle Site:
#      "temp_hf_Fmid_wide.txt" - text file of high frequency temperature profile in wide format for use in rLakeAnalyzer
#      "temp_hf_Fmid.csv" - high frequency temperature profile data, long format
#   6. Pond B, Middle Site:
#      "temp_hf_Bmid_wide.txt" - text file of high frequency temperature profile in wide format for use in rLakeAnalyzer
#      "temp_hf_Bmid.csv" - high frequency temperature profile data, long format


# Clear environment, set working directory - UPDATE AS NEEDED --------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd('C:/Users/Ellen/Desktop/Box Sync/Albright DISSERTATION/Albright_Chapter_4/DATA/SUBMIT')

# REQUIRED PACKAGES FOR CODE - install as needed
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("viridis")
# install.packages("rLakeAnalyzer")
# install.packages("lubridate")
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(rLakeAnalyzer)
library(lubridate)

### OVERVIEW OF SCRIPT ORGANIZATION ### ------------------------------------------------------------------------------------------------------------------------------------
# PART 1 - Data cleaning
#      1A - Pond F, Deep Site
#      1B - Pond B, Deep Site
#      1C - Pond F, Shallow Site
#      1D - Pond B, Shallow Site
#      1E - Pond F, Middle Site
#      1F - Pond B, Middle Site
#      1G - Bind it all together!

### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### PART 1A - Pond F, Deep Site
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Deep_0<-read.csv("F_Deep_0m.csv")
F_Deep_0<-F_Deep_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 
  
F_Deep_0.25<-read.csv("F_Deep_0.25m.csv")
F_Deep_0.25<-F_Deep_0.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_0.5<-read.csv("F_Deep_0.5m.csv")
F_Deep_0.5<-F_Deep_0.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_0.75<-read.csv("F_Deep_0.75m.csv")
F_Deep_0.75<-F_Deep_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1<-read.csv("F_Deep_1m.csv")
F_Deep_1<-F_Deep_1 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1.25<-read.csv("F_Deep_1.25m.csv")
F_Deep_1.25<-F_Deep_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1.5<-read.csv("F_Deep_1.5m.csv")
F_Deep_1.5<-F_Deep_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_2<-read.csv("F_Deep_2m.csv")
F_Deep_2<-F_Deep_2 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_deep<-rbind(F_Deep_0,F_Deep_0.25,F_Deep_0.5,F_Deep_0.75,F_Deep_1,F_Deep_1.25,F_Deep_1.5,F_Deep_2)
F_deep<-filter(F_deep,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(F_deep) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
F_deep_date = F_deep %>% 
  mutate(datetime = mdy_hm(datetime))
View(F_deep_date)
str(F_deep_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=F_deep_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(F_deep_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("F_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
F_deep_wide<-F_deep_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.25="0.25", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1="1", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(F_deep_wide, file="temp_hf_Fdeep_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_deep_date,file="temp_hf_Fdeep.csv",row.names = FALSE)


### PART 1B - Pond B, Deep Site
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
B_Deep_0<-read.csv("B_Deep_0m.csv")
B_Deep_0<-B_Deep_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_0.25<-read.csv("B_Deep_0.25m.csv")
B_Deep_0.25<-B_Deep_0.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_0.5<-read.csv("B_Deep_0.5m.csv")
B_Deep_0.5<-B_Deep_0.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_0.75<-read.csv("B_Deep_0.75m.csv")
B_Deep_0.75<-B_Deep_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_1.25<-read.csv("B_Deep_1.25m.csv")
B_Deep_1.25<-B_Deep_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_1.5<-read.csv("B_Deep_1.5m.csv")
B_Deep_1.5<-B_Deep_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Deep_2<-read.csv("B_Deep_2m.csv")
B_Deep_2<-B_Deep_2 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=19, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_deep<-rbind(B_Deep_0,B_Deep_0.25,B_Deep_0.5,B_Deep_0.75,B_Deep_1.25,B_Deep_1.5,B_Deep_2)
B_deep<-filter(B_deep,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(B_deep) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
B_deep_date = B_deep %>% 
  mutate(datetime = mdy_hm(datetime))
View(B_deep_date)
str(B_deep_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=B_deep_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(B_deep_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
B_deep_wide<-B_deep_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.25="0.25", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(B_deep_wide, file="temp_hf_Bdeep_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond B
write.csv(B_deep_date,file="temp_hf_Bdeep.csv",row.names = FALSE)

### PART 1C - Pond F, Shallow Site
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Shallow_0<-read.csv("F_Shallow_0m.csv")
F_Shallow_0<-F_Shallow_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow_0.75<-read.csv("F_Shallow_0.75m.csv")
F_Shallow_0.75<-F_Shallow_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow_1<-read.csv("F_Shallow_1m.csv")
F_Shallow_1<-F_Shallow_1 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c)

F_Shallow_1.25<-read.csv("F_Shallow_1.25m.csv")
F_Shallow_1.25<-F_Shallow_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frace, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow_1.5<-read.csv("F_Shallow_1.5m.csv")
F_Shallow_1.5<-F_Shallow_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=21, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Shallow<-rbind(F_Shallow_0,F_Shallow_0.75,F_Shallow_1,F_Shallow_1.25,F_Shallow_1.5)
F_Shallow<-filter(F_Shallow,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(F_Shallow) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
F_Shallow_date = F_Shallow %>% 
  mutate(datetime = mdy_hm(datetime))
str(F_Shallow_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=F_Shallow_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(F_Shallow_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
F_shallow_wide<-F_Shallow_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.75="0.75", wtr_1="1", wtr_1.25="1.25", wtr_1.5="1.5")
write.table(F_shallow_wide, file="temp_hf_Fshallow_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_Shallow_date,file="temp_hf_Fshallow.csv",row.names = FALSE)

### PART 1D - Pond B, Shallow Site
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
B_Shallow_0<-read.csv("B_Shallow_0m.csv")
B_Shallow_0<-B_Shallow_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow_0.75<-read.csv("B_Shallow_0.75m.csv")
B_Shallow_0.75<-B_Shallow_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow_1.25<-read.csv("B_Shallow_1.25m.csv")
B_Shallow_1.25<-B_Shallow_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow_1.5<-read.csv("B_Shallow_1.5m.csv")
B_Shallow_1.5<-B_Shallow_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=21, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Shallow<-rbind(B_Shallow_0,B_Shallow_0.75,B_Shallow_1.25,B_Shallow_1.5)
B_Shallow<-filter(B_Shallow,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(B_Shallow) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
B_Shallow_date = B_Shallow %>% 
  mutate(datetime = mdy_hm(datetime))
str(B_Shallow_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=B_Shallow_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(B_Shallow_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
B_shallow_wide<-B_Shallow_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.75="0.75", wtr_1.25="1.25", wtr_1.5="1.5")
write.table(B_shallow_wide, file="temp_hf_Bshallow_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(B_Shallow_date,file="temp_hf_Bshallow.csv",row.names = FALSE)

### PART 1E - Pond F, Middle Site
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Mid_0<-read.csv("F_Mid_0m.csv")
F_Mid_0<-F_Mid_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_0.5<-read.csv("F_Mid_0.5m.csv")
F_Mid_0.5<-F_Mid_0.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_0.75<-read.csv("F_Mid_0.75m.csv")
F_Mid_0.75<-F_Mid_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_1<-read.csv("F_Mid_1m.csv")
F_Mid_1<-F_Mid_1 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_1.25<-read.csv("F_Middle_1.25m.csv")
F_Mid_1.25<-F_Mid_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_1.5<-read.csv("F_Middle_1.5m.csv")
F_Mid_1.5<-F_Mid_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid_2<-read.csv("F_Mid_2m.csv")
F_Mid_2<-F_Mid_2 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=20, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Mid<-rbind(F_Mid_0, F_Mid_0.5, F_Mid_0.75, F_Mid_1, F_Mid_1.25, F_Mid_1.5, F_Mid_2)
F_Mid<-filter(F_Mid,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(F_Mid) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
F_Mid_date = F_Mid %>% 
  mutate(datetime = mdy_hm(datetime))
View(F_Mid_date)
str(F_Mid_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=F_Mid_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(F_Mid_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("F_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
F_Mid_wide<-F_Mid_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1="1", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(F_Mid_wide, file="temp_hf_Fmid_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_Mid_date,file="temp_hf_Fmid.csv",row.names = FALSE)

### PART 1F - Pond B, Middle Site
# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
B_Mid_0<-read.csv("B_Mid_0m.csv")
B_Mid_0<-B_Mid_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_0.25<-read.csv("B_Mid_0.25m.csv")
B_Mid_0.25<-B_Mid_0.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_0.5<-read.csv("B_Mid_0.5m.csv")
B_Mid_0.5<-B_Mid_0.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_0.75<-read.csv("B_Mid_0.75m.csv")
B_Mid_0.75<-B_Mid_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_1.25<-read.csv("B_Middle_1.25m.csv")
B_Mid_1.25<-B_Mid_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_1.5<-read.csv("B_Mid_1.5m.csv")
B_Mid_1.5<-B_Mid_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_Mid_2<-read.csv("B_Mid_2m.csv")
B_Mid_2<-B_Mid_2 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="B", site_id=20, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

B_mid<-rbind(B_Mid_0,B_Mid_0.25,B_Mid_0.5,B_Mid_0.75,B_Mid_1.25,B_Mid_1.5,B_Mid_2)
B_mid<-filter(B_mid,doy>"142"&doy<"241") #filter for data range of all sensors in lake

str(B_mid) #datetime became a character somehow, need to address that
# convert character data to date and time using lubridate (code from T. Butts)
B_mid_date = B_mid %>% 
  mutate(datetime = mdy_hm(datetime))
str(B_mid_date) #yay it's a date in ISO 8601 format!

# Quick visualizations to make sure everything looks alright - looking good!
ggplot(data=B_mid_date, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

ggplot(B_mid_date, aes(doy, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))
# ggsave("B_deep_heatmap.png",width=6.5, height=3.3, units="in", dpi=300)

#pivot to wide format to work with lake analyzer and similar ... save as tab-delimited file
B_mid_wide<-B_mid_date %>%
  select(datetime,temp_depth_m,temp_c) %>% 
  pivot_wider(names_from=temp_depth_m,values_from=temp_c) %>% 
  rename(wtr_0="0", wtr_0.25="0.25", wtr_0.5="0.5", wtr_0.75="0.75", wtr_1.25="1.25", wtr_1.5="1.5", wtr_2="2")
write.table(B_mid_wide, file="temp_hf_Bmid_wide.txt",row.names = FALSE,sep = "\t")

# Write one csv for temperature profiles from deep site, pond B
write.csv(B_mid_date,file="temp_hf_Bmid.csv",row.names = FALSE)

# Bind all long-format tables together to produce one temperature chain data frame (keep wide format separate for LakeAnalyzer)
tchain_dat<-rbind(F_deep_date, F_Mid_date, F_Shallow_date, B_deep_date, B_mid_date, B_Shallow_date)
write.csv(tchain_dat, file="temp_profiles_2020.csv", row.names=FALSE)








#' @title Calculate the Schmidt stability
#' 
#' @description Schmidt stability, or the resistance to mechanical mixing due to the
#' potential energy inherent in the stratification of the water column.
#' 
#' Schmidt stability was first defined by Schmidt (1928) and later modified by
#' Hutchinson (1957). This stability index was formalized by Idso (1973) to
#' reduce the effects of lake volume on the calculation (resulting in a mixing
#' energy requirement per unit area).
#' 
#' @param wtr a numeric vector of water temperature in degrees C
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr
#' measurements
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to
#' bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal
#' measures in bthA
#' @param sal a numeric vector of salinity in Practical Salinity Scale units
#' @return a numeric vector of Schmidt stability (J/m^2)
#' @seealso \code{\link{ts.schmidt.stability}} \code{\link{lake.number}}
#' \code{\link{wedderburn.number}}
#' @references Schmidt, W., 1928. \emph{Ueber Temperatur and
#' Stabilitaetsverhaltnisse von Seen}. Geo- graphiska Annaler 10, 145-177.
#' 
#' Hutchinson, G.E., 1957. \emph{A Treatise on Limnology}, vol. 1. John Wiley &
#' Sons, Inc., New York.
#' 
#' Idso, S.B., 1973. \emph{On the concept of lake stability}. Limnology and
#' Oceanography 18, 681-683.
#' @keywords arith
#' @examples
#' 
#' 
#' 	bthA	<-	c(1000,900,864,820,200,10)
#' 	bthD	<-	c(0,2.3,2.5,4.2,5.8,7)
#' 	
#' 	wtr	<-	c(28,27,26.4,26,25.4,24,23.3)
#' 	depths	<-	c(0,1,2,3,4,5,6)
#' 	
#' 	cat('Schmidt stability for input is: ')
#' 	cat(schmidt.stability(wtr, depths, bthA, bthD))
#' @export

wtr<-load.ts("temp_profiles_Fdeep_wide.txt")
wtr<-load.ts("temp_hf_Bdeep_wide.txt")

bathy<-load.bathy("pond_hypsography.txt")

ts_SSI<-ts.schmidt.stability(wtr,bathy,na.rm=FALSE)
SSI<-as.data.frame(ts_SSI)
plot(schmidt.stability~datetime,data=SSI)
range(SSI$schmidt.stability,na.rm=T)

library(lubridate)
SSI$doy<-yday(SSI$datetime)
SSI$doy_frac<-hour(SSI$datetime)
SSI$min<-minute(SSI$datetime)
SSI$minfrac[SSI$min=="30"] <- 0.5
SSI$minfrac[SSI$min=="0"] <- 0
SSI$hourfrac<-(SSI$doy_frac + SSI$minfrac)/24

SSI$doy_frac<-SSI$doy+SSI$hourfrac
SSI<-SSI %>% 
  select(datetime,doy,doy_frac,schmidt.stability)


SSI_daily<-SSI %>% 
  group_by(doy) %>% 
  summarize(mean_schmidt.stability=mean(schmidt.stability))

#plot daily means
ggplot(data=SSI_daily, aes(x=doy, y=mean_schmidt.stability))+
  geom_line(size=1)+xlim(143,233)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Daily Mean SSI (J/m2)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))
ggsave("mean_SSI.png",width=6.5, height=3.3, units="in", dpi=300)  

ggplot(data=SSI, aes(x=doy_frac, y=schmidt.stability))+
  geom_line(size=1)+xlim(143,233)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Schmidt's Stability Index (J/m2)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))
ggsave("raw_SSI.png",width=6.5, height=3.3, units="in", dpi=300)  

ggplot(data=SSI, aes(x=doy_frac, y=schmidt.stability))+
  geom_line(size=1)+xlim(220,225)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Schmidt's Stability Index (J/m2)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))
ggsave("derecho_SSI.png",width=3, height=3, units="in", dpi=300)  

SSI_canopy<-subset(SSI_daily,doy==143|doy==150|doy==157|doy==166|doy==171|doy==178|doy==186|doy==192|doy==199|doy==206|doy==213|doy==220|doy==228)
F19_SSI<-subset(F19, doy!=129&doy!=234)
plot(SSI_canopy$mean_schmidt.stability~F19_SSI$canopy_per,pch=16,cex=1.5,cex.axis=1.5)
View(F19)
