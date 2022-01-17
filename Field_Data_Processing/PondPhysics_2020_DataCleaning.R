# Project: "Macrophytes alter thermal stratification and dissolved oxygen dynamics in shallow ponds"
#           Part of ISU Hort Farm Pond Study, 2020
# Last Modified 17 January 2022
# Contributors: Ellen Albright, Robert Ladwig, Grace Wilkinson, Tyler Butts
# Description: Field data cleaning - high frequency temperature loggers (t-chains) and manual YSI profiles for DO

# Data citation: ______________

# The cleaning code produces the following data tables:
# 1. Six text files of high frequency temperature profiles in wide format for use in r Lake Analyzer (one file per site)
#      "temp_hf_Fdeep_wide.txt" - Pond F, deep site
#      "temp_hf_Bdeep_wide.txt" - Pond B, deep site
#      "temp_hf_Fshallow_wide.txt" - Pond F, shallow site
#      "temp_hf_Bshallow_wide.txt" - Pond B, shallow site
#      "temp_hf_Fmid_wide.txt" - Pond F, middle site
#      "temp_hf_Bmid_wide.txt" - Pond B, middle site
# 2. One long-format csv file of all high frequency temperature profile data (all sites combined)
#      "temp_profiles_2020.csv"
# 3. One csv file of all manual YSI multi-parameter profiles (18 sites across 2 ponds)
#      "DO_profiles.csv"


# Clear environment, set working directory - UPDATE AS NEEDED --------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd('C:/Users/Ellen/Desktop/Box Sync/Albright DISSERTATION/Albright_Chapter_4/DATA/SUBMIT')

# REQUIRED PACKAGES FOR CODE - install as needed
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("viridis")
# install.packages("lubridate")
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(lubridate)

### OVERVIEW OF SCRIPT ORGANIZATION ### ------------------------------------------------------------------------------------------------------------------------------------
# PART 1 - High Frequency Temperature Profiles - Data cleaning
#      1A - Pond F, Deep Site
#      1B - Pond B, Deep Site
#      1C - Pond F, Shallow Site
#      1D - Pond B, Shallow Site
#      1E - Pond F, Middle Site
#      1F - Pond B, Middle Site
#      1G - Bind it all together!
# PART 2 - Manual Sonde Profiles - Data Cleaning

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

### PART 1G - Bind all long-format tables together to produce one temperature chain data frame (keep wide format separate for LakeAnalyzer)
tchain_dat<-rbind(F_deep_date, F_Mid_date, F_Shallow_date, B_deep_date, B_mid_date, B_Shallow_date)
write.csv(tchain_dat, file="temp_profiles_2020.csv", row.names=FALSE)

### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### PART 2 - Manual Sonde Profiles
# Load in data files (one csv for each pond, DOY). Clean up variable names, add necessary variables (DOY, Pond ID), prepare to merge files

### DOY 150, Pond F--------------------------------------------------------------------------------------------
DOY150F<-read.csv("spatialdo_150_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY150F['temp_c']<-((DOY150F$Temp...F.)-32)/1.8
DOY150F['pond']<-'F'
DOY150F['doy']<-150

names(DOY150F) # The variable names from the sonde are a mess

# Rename and select columns of interest
F_150<-DOY150F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)
### DOY 150, Pond B--------------------------------------------------------------------------------------------
DOY150B<-read.csv("spatialdo_150_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY150B['temp_c']<-((DOY150B$Temp...F.)-32)/1.8
DOY150B['pond']<-'B'
DOY150B['doy']<-150

# Rename and select columns of interest
B_150<-DOY150B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)


### DOY 157, Pond F--------------------------------------------------------------------------------------------
DOY157F<-read.csv("spatialdo_157_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY157F['temp_c']<-((DOY157F$Temp...F.)-32)/1.8
DOY157F['pond']<-'F'
DOY157F['doy']<-157

# Rename and select columns of interest
F_157<-DOY157F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 157, Pond B--------------------------------------------------------------------------------------------
DOY157B<-read.csv("spatialdo_157_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY157B['temp_c']<-((DOY157B$Temp...F.)-32)/1.8
DOY157B['pond']<-'B'
DOY157B['doy']<-157

# Rename and select columns of interest
B_157<-DOY157B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 166, Pond F--------------------------------------------------------------------------------------------
DOY166F<-read.csv("spatialdo_166_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY166F['temp_c']<-((DOY166F$Temp...F.)-32)/1.8
DOY166F['pond']<-'F'
DOY166F['doy']<-166

# Rename and select columns of interest
F_166<-DOY166F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 166, Pond B--------------------------------------------------------------------------------------------
DOY166B<-read.csv("spatialdo_166_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY166B['temp_c']<-((DOY166B$Temp...F.)-32)/1.8
DOY166B['pond']<-'B'
DOY166B['doy']<-166

# Rename and select columns of interest
B_166<-DOY166B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 171, Pond F--------------------------------------------------------------------------------------------
DOY171F<-read.csv("spatialdo_171_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY171F['temp_c']<-((DOY171F$Temp...F.)-32)/1.8
DOY171F['pond']<-'F'
DOY171F['doy']<-171

# Rename and select columns of interest
F_171<-DOY171F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 171, Pond B--------------------------------------------------------------------------------------------
DOY171B<-read.csv("spatialdo_171_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY171B['temp_c']<-((DOY171B$Temp...F.)-32)/1.8
DOY171B['pond']<-'B'
DOY171B['doy']<-171

# Rename and select columns of interest
B_171<-DOY171B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 178, Pond F--------------------------------------------------------------------------------------------
DOY178F<-read.csv("spatialdo_178_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY178F['temp_c']<-((DOY178F$Temp...F.)-32)/1.8
DOY178F['pond']<-'F'
DOY178F['doy']<-178

# Rename and select columns of interest
F_178<-DOY178F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 178, Pond B--------------------------------------------------------------------------------------------
DOY178B<-read.csv("spatialdo_178_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY178B['temp_c']<-((DOY178B$Temp...F.)-32)/1.8
DOY178B['pond']<-'B'
DOY178B['doy']<-178

# Rename and select columns of interest
B_178<-DOY178B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 186, Pond F--------------------------------------------------------------------------------------------
DOY186F<-read.csv("spatialdo_186_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY186F['temp_c']<-((DOY186F$Temp...F.)-32)/1.8
DOY186F['pond']<-'F'
DOY186F['doy']<-186

# Rename and select columns of interest
F_186<-DOY186F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 186, Pond B--------------------------------------------------------------------------------------------
DOY186B<-read.csv("spatialdo_186_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY186B['temp_c']<-((DOY186B$Temp...F.)-32)/1.8
DOY186B['pond']<-'B'
DOY186B['doy']<-186

# Rename and select columns of interest
B_186<-DOY186B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 192, Pond F--------------------------------------------------------------------------------------------
DOY192F<-read.csv("spatialdo_192_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY192F['temp_c']<-((DOY192F$Temp...F.)-32)/1.8
DOY192F['pond']<-'F'
DOY192F['doy']<-192

# Rename and select columns of interest
F_192<-DOY192F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 192, Pond B--------------------------------------------------------------------------------------------
DOY192B<-read.csv("spatialdo_192_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY192B['temp_c']<-((DOY192B$Temp...F.)-32)/1.8
DOY192B['pond']<-'B'
DOY192B['doy']<-192

# Rename and select columns of interest
B_192<-DOY192B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 199, Pond F--------------------------------------------------------------------------------------------
DOY199F<-read.csv("spatialdo_199_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY199F['temp_c']<-((DOY199F$Temp...F.)-32)/1.8
DOY199F['pond']<-'F'
DOY199F['doy']<-199

# Rename and select columns of interest
F_199<-DOY199F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 199, Pond B--------------------------------------------------------------------------------------------
DOY199B<-read.csv("spatialdo_199_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY199B['temp_c']<-((DOY199B$Temp...F.)-32)/1.8
DOY199B['pond']<-'B'
DOY199B['doy']<-199

# Rename and select columns of interest
B_199<-DOY199B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 206, Pond F--------------------------------------------------------------------------------------------
DOY206F<-read.csv("spatialdo_206_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY206F['temp_c']<-((DOY206F$Temp...F.)-32)/1.8
DOY206F['pond']<-'F'
DOY206F['doy']<-206

# Rename and select columns of interest
F_206<-DOY206F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 206, Pond B--------------------------------------------------------------------------------------------
DOY206B<-read.csv("spatialdo_206_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY206B['temp_c']<-((DOY206B$Temp...F.)-32)/1.8
DOY206B['pond']<-'B'
DOY206B['doy']<-206

# Rename and select columns of interest
B_206<-DOY206B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 213, Pond F--------------------------------------------------------------------------------------------
DOY213F<-read.csv("spatialdo_213_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY213F['temp_c']<-((DOY213F$Temp...F.)-32)/1.8
DOY213F['pond']<-'F'
DOY213F['doy']<-213

# Rename and select columns of interest
F_213<-DOY213F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 213, Pond B--------------------------------------------------------------------------------------------
DOY213B<-read.csv("spatialdo_213_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY213B['temp_c']<-((DOY213B$Temp...F.)-32)/1.8
DOY213B['pond']<-'B'
DOY213B['doy']<-213

# Rename and select columns of interest
B_213<-DOY213B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 220, Pond F--------------------------------------------------------------------------------------------
DOY220F<-read.csv("spatialdo_220_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY220F['temp_c']<-((DOY220F$Temp...F.)-32)/1.8
DOY220F['pond']<-'F'
DOY220F['doy']<-220

# Rename and select columns of interest
F_220<-DOY220F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 220, Pond B--------------------------------------------------------------------------------------------
DOY220B<-read.csv("spatialdo_220_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY220B['temp_c']<-((DOY220B$Temp...F.)-32)/1.8
DOY220B['pond']<-'B'
DOY220B['doy']<-220

# Rename and select columns of interest
B_220<-DOY220B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 228, Pond F--------------------------------------------------------------------------------------------
DOY228F<-read.csv("spatialdo_228_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY228F['temp_c']<-((DOY228F$Temp...F.)-32)/1.8
DOY228F['pond']<-'F'
DOY228F['doy']<-228

# Rename and select columns of interest
F_228<-DOY228F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 228, Pond B--------------------------------------------------------------------------------------------
DOY228B<-read.csv("spatialdo_228_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY228B['temp_c']<-((DOY228B$Temp...F.)-32)/1.8
DOY228B['pond']<-'B'
DOY228B['doy']<-228

# Rename and select columns of interest
B_228<-DOY228B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 234, Pond F--------------------------------------------------------------------------------------------
DOY234F<-read.csv("spatialdo_234_PondF.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY234F['temp_c']<-((DOY234F$Temp...F.)-32)/1.8
DOY234F['pond']<-'F'
DOY234F['doy']<-234

# Rename and select columns of interest
F_234<-DOY234F %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

### DOY 234, Pond B--------------------------------------------------------------------------------------------
DOY234B<-read.csv("spatialdo_234_PondB.csv")

# Add columns to indicate pond and DOY and to calculate temperature in Celsius 
DOY234B['temp_c']<-((DOY234B$Temp...F.)-32)/1.8
DOY234B['pond']<-'B'
DOY234B['doy']<-234

# Rename and select columns of interest
B_234<-DOY234B %>%
  rename(site_id=SITE, 
         barometer=Barometer..mmHg., 
         sp_cond=Sp.Cond..µS.cm.,
         tds=TDS..mg.L.,
         chla_RFU=Chlorophyll..RFU.,
         chla_ugL=Chlorophyll..µg.L.,
         pc_RFU=PC..RFU.,
         pc_ugL=PC..ug.L.,
         odo_sat=ODO....Sat.,
         odo_mgL=ODO..mg.L.,
         vertical_m=Vertical.Position..m.) %>% 
  select(doy, pond, site_id, vertical_m, odo_mgL, odo_sat, temp_c, barometer, sp_cond, tds, chla_RFU, chla_ugL, pc_RFU, pc_ugL)

# Join all spatial profile data frames together
spatial_DO<-rbind(B_150, B_157, B_166, B_171, B_178, B_186, B_192, B_199, B_206, B_213, B_220, B_228, B_234,
                  F_150, F_157, F_166, F_171, F_178, F_186, F_192, F_199, F_206, F_213, F_220, F_228, F_234)


write.csv(spatial_DO, file="DO_profiles_2020.csv", row.names=FALSE)
