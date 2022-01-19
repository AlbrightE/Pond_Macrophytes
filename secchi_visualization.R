# For Ellen's Macrophytes -- did Daphnia biomass differ between ponds F&B
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

secchi = read_csv("Field_Data_Processing/secchi_data_hort2020.csv") %>%
  rename(pond_id = pond)

secchi = as.data.frame(secchi)

col_B = rgb(244,109,67, max = 255, alpha = 150)
col_F = rgb(69,117,180, max = 255, alpha = 150)

plot(secchi[secchi$pond_id=="B", "doy"], 
     secchi[secchi$pond_id=="B", "secchi"], 
     type = "l", col = col_B, lwd = 4, ylim = c(2.5,0),
     xlab = "Day of Year, 2020", ylab = "Secchi Depth (m)")
points(secchi[secchi$pond_id=="F", "doy"], 
       secchi[secchi$pond_id=="F", "secchi"], 
       type = "l", col = col_F, lwd = 4)
legend("topright", legend = c("Pond B", "Pond F"), 
       pch = 15, col = c(col_B, col_F), pt.cex = 2)
