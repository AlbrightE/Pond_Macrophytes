# code to calculate atmospheric heatwaves according to Tassone et al. (2021)
# Air temperature historical data from Ames airport, accessed through Iowa mesonet
# Calculated daily mean air temp and then the 90th percentile clipped to the sampling season for 2020
#Identified if there were 3 consecutive dates with temps above that threshold in 2020

## CONCLUSION: there was effectively an atmospheric heatwave for DOY 154-156

temp = read.csv("AMW_tempData_1997_2020.csv")

summer_temp = temp %>%
  filter(doy<242) %>%
  filter(doy>141) %>%
  filter(!(tmpc == "M")) %>%
  mutate(tmpc = as.numeric(tmpc))

daily_temp = summer_temp %>%
  group_by(doy, year) %>%
  summarize(air_temp = mean(tmpc, na.rm = TRUE)) %>%
  ungroup()

quantile(daily_temp$air_temp, probs = c(0.1, 0.9))

temp2020 = daily_temp %>%
  filter(year == 2020)

plot(temp2020$doy, temp2020$air_temp, type = "o", lwd = 3, pch = 19)
abline(26.21, 0)
abline(17.54,0)


