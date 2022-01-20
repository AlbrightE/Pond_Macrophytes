# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#=============================================================
pondB_chl = read_csv("Field_Data_Processing/hort20_surface_dat.csv") %>%
  filter(pond_id == 'B') %>%
  filter(!(is.na(chla)))

pondF_chl = read_csv("Field_Data_Processing/hort20_surface_dat.csv") %>%
  filter(pond_id == 'F') %>%
  filter(!(is.na(chla)))

#Chlorophyll GAM - Pond B
chlB_gam <- gam(chla ~ s(doy, k = 40), 
               data = pondB_chl, method = 'REML')
summary(chlB_gam)
gam.check(chlB_gam)


#Chlorophyll GAM - Pond F
chlF_gam <- gam(chla ~ s(doy, k = 40), 
                data = pondF_chl, method = 'REML')
summary(chlF_gam)
gam.check(chlF_gam)


#Plotting Colors
pondB_col_transparent = rgb(215,48,39, max = 255, alpha = 150)
pondB_col = rgb(215,48,39, max = 255, alpha = 200)
pondF_col_transparent = rgb(69,117,180, max = 255, alpha = 150)
pondF_col = rgb(69,117,180, max = 255, alpha = 200)

#============================
# Set up the Plot
windows(height = 5, width = 6.5)
par(omi = c(0.5,0.5,0.1,0.1), mai = c(0.5,0.5,0.1,0.1))

#Plot of the chlorophyll GAM for POND B ===============
plot(chlB_gam, select = 1, 
     seWithMean = TRUE, shift = coef(chlB_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = pondB_col_transparent, 
     cex = 0.75, pch = 19, lwd = 2, col = pondB_col,
     xlab = "", ylab = "", cex.axis= 1.2, ylim = c(0,12))

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2)

#Plot of the chlorophyll GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(chlF_gam, select = 1,  
     seWithMean = TRUE, shift = coef(chlF_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = pondF_col_transparent, 
     cex = 0.75, pch = 19, lwd = 2, col = pondF_col,
     main = "", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
mtext(side = 2, line = 3, "Chlorophyll a (ug/L)", cex = 1.25)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 1.25)
legend("topright", legend = c("Nutrient Addition Pond", "Reference Pond"), 
       pch = 15, pt.cex = 2, col = c(pondB_col, pondF_col), bg = "white")