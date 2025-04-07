#load("/Users/mustafaosman/Desktop/IPAC Data/NDI_2016_June_09.07.16_Public.dta")
install.packages("haven")
library(haven)

data <-read_dta("/Users/mustafaosman/Desktop/IPAC Data/caucus barometer/CB_2017_Georgia_public_17.11.17.dta")
write.csv(data, "/Users/mustafaosman/Desktop/IPAC Data/caucus barometer/raw_data.csv", row.names = FALSE)

data
colnames(data)
data$INT_START
install.packages("dplyr")
library("dplyr")
new_data <- select(data, INT_DATE, INT_START, SUBSTRATUM, STRATUM,COUNTRY, ID, AGE, RESPSEX, RESEMPL, RESPPOB, RESPMAR, TRUCRTS,,TRUPARL, TRUEXEC, TRUPRES)

filt_barom_2017 <- subset(new_data, COUNTRY == 3 & STRATUM ==1)
filt_barom_2017$INT_DATE
filt_barom_2017 <- filt_barom_2017 %>%
  arrange(INT_DATE, INT_START)
filt_barom_2017
write.csv(filt_barom_2017, "/Users/mustafaosman/Desktop/IPAC Data/caucus barometer/2017_filtered_data.csv", row.names = FALSE)

#raw_barom_2024 <- read_dta("/Users/mustafaosman/Desktop/IPAC Data/caucus barometer/cb2024ge_CB_2024_Geo_Public_10.02.2025.dta")
#colnames(raw_barom_2024)
#raw_barom_2024$SUBSTRATUM
#filter(SUBSTRATUM)

  
write.csv(new_subset_data, "/Users/mustafaosman/Desktop/IPAC Data/filtered_data.csv", row.names = FALSE)




