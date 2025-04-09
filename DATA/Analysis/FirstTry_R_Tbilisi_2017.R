air_data = read.csv('Desktop/IPAC/Complete_Average_Hourly_Tbilisi_2017.csv')
original_data = read_dta('Desktop/GitHub/AIR-QUALITY-PROJECT/DATA/Raw Data/Caucus Barometer/CB_2017_Georgia_public_17.11.17.dta')
original_data$SUBSTRATUM

library(haven)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)





######## DEMOGRAPHICS ###############
air_data_clean <- subset(air_data, 
                          RESPSEX>0 & 
                           RESEMPL>0 & 
                           RESPPOB > 0 & 
                           RESPMAR>0)

air_data_clean$RESPSEX <- factor(air_data_clean$RESPSEX,
                                 levels = c(1,2),
                                 labels = c("Male", "Female"))
air_data_clean$RESEMPL <- factor(air_data_clean$RESEMPL,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))

air_data_clean$RESPPOB <- factor(air_data_clean$RESPPOB,
                                 levels = c(1,2,3,4,5),
                                 labels = c("In this settlement", "In another settlement of the same region of the country",
                                            "In a settlement in another region of the country", " Outside the country, but in the FSU",
                                            "Outside the FSU"))

air_data_clean$RESPMAR <- factor(air_data_clean$RESPMAR,
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("Never married", "Married: official state marriage", "Married: religious ceremony", "Married: both religious ceremomy and state marriage",
                                            "Cohabiting", "Divorced", "Separated", "Widow, Widower"))



############ EXEC ############
air_data_clean_exec <- subset(air_data_clean, TRUEXEC>0)

air_data_clean_exec$TRUEXEC <- factor(air_data_clean_exec$TRUEXEC,
                    levels = c(1,2,3,4,5),
                    labels = c("Fully Distrust", 
                               "Rather Distrust",
                               "Neither Trust Nor Distrust",
                               "Rather Trust", 
                               "Fully Trust"))

modelEXEC <- polr(TRUEXEC ~ AGE + as.factor(RESPSEX) + as.factor(RESEMPL) + as.factor(RESPPOB) + as.factor(RESPMAR) + 
                    NO2 + 
                    SO2 + 
                    PM2.5 +
                    PM10 + 
                    O3 + 
                    CO +
                    SO2_Daily + 
                    PM2.5_Daily + 
                    PM10_Daily +
                    O3_Daily +
                    CO_Daily
                  , 
                  data = air_data_clean_exec, Hess=TRUE)
summary(modelEXEC)
modelEXEC_parsed = step(modelEXEC)
summary(modelEXEC_parsed)

############### PARL ###################
air_data_clean_parl <- subset(air_data_clean, TRUPARL>0)

air_data_clean_parl$TRUPARL <- factor(air_data_clean_parl$TRUPARL,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Fully Distrust", 
                                            "Rather Distrust",
                                            "Neither Trust Nor Distrust", 
                                            "Rather Trust", 
                                            "Fully Trust"))


modelPARLM <- polr(as.factor(TRUPARL) ~ AGE + as.factor(RESPSEX) + as.factor(RESEMPL) + as.factor(RESPPOB) + as.factor(RESPMAR) + 
                    NO2 + 
                     SO2 + 
                     PM2.5 +
                    PM10 + 
                    O3 + 
                    CO +
                   SO2_Daily + 
                     PM2.5_Daily+ 
                     PM10_Daily + 
                     O3_Daily + 
                     CO_Daily
                  , 
                  data = air_data_clean, Hess=TRUE)

summary(modelPARLM)
modelPARLM_parsed = step(modelPARLM)
summary(modelPARLM_parsed)


summary(modelEXEC_parsed)
summary(modelPARLM_parsed)

summary(polr(formula = as.factor(TRUPARL) ~ PM10 + CO, data = air_data_clean, 
     Hess = TRUE))
