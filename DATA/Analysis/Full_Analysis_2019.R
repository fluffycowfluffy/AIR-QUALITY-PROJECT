library(MASS)
library(stargazer)


air_data = read.csv('/Users/nicorapallo/Desktop/GitHub/AIR-QUALITY-PROJECT/DATA/Filtered Data/Complied Data/Compiled_Data_2019_withHourly.csv')
#original_data = read_dta('Desktop/GitHub/AIR-QUALITY-PROJECT/DATA/Raw Data/Caucus Barometer/CB_2017_Georgia_public_17.11.17.dta')
score_data = read.csv('/Users/nicorapallo/Desktop/GitHub/AIR-QUALITY-PROJECT/DATA/Filtered Data/Complied Data/compiled_data_2019_withTrustScore.csv')

#### Set Demographic Factors ####
air_data$RESPSEX <- droplevels(factor(air_data$RESPSEX,
                  levels = c(-1, 1, 2),
                  labels = c("No Answer", "Male", "Female")))


air_data$RESPEMP <- droplevels(factor(air_data$RESPEMP,
                                 levels = c(-1, 0, 1),
                                 labels = c("No Answer", "No", "Yes")))

air_data$RESPPOB <- droplevels(factor(air_data$RESPPOB,
                                 levels = c(-1, 1,2,3,4,5),
                                 labels = c("No answer",
                                            "In this settlement", 
                                            "In another settlement of the same region of the country",
                                            "In a settlement in another region of the country", 
                                            "Outside the country, but in the FSU",
                                            "Outside the FSU")))

air_data$RESPMAR <- droplevels(factor(air_data$RESPMAR,
                                 levels = c(-1, 1, 2, 5, 6, 8),
                                 labels = c("No Answer",
                                            "Never married", 
                                            "Married - Combined",
                                            "Cohabiting", 
                                            "Divorced, Separated", 
                                            "Widow, Widower")))
air_data$SUBSTRATUM <- droplevels(factor(air_data$SUBSTRATUM,
                                      levels = c(-1, 1, 9),
                                      labels = c("No Answer",
                                                 "Tbilisi", 
                                                 "Batumi")))
air_data$RESPSEX <- droplevels(factor(air_data$RESPSEX,
                                      levels = c(-1, 1, 2),
                                      labels = c("No Answer", "Male", "Female")))


score_data$RESPEMP <- droplevels(factor(score_data$RESPEMP,
                                      levels = c(-1, 0, 1),
                                      labels = c("No Answer", "No", "Yes")))

score_data$RESPPOB <- droplevels(factor(score_data$RESPPOB,
                                      levels = c(-1, 1,2,3,4,5),
                                      labels = c("No answer",
                                                 "In this settlement", 
                                                 "In another settlement of the same region of the country",
                                                 "In a settlement in another region of the country", 
                                                 "Outside the country, but in the FSU",
                                                 "Outside the FSU")))

score_data$RESPMAR <- droplevels(factor(score_data$RESPMAR,
                                      levels = c(-1, 1, 2, 5, 6, 8),
                                      labels = c("No Answer",
                                                 "Never married", 
                                                 "Married - Combined",
                                                 "Cohabiting", 
                                                 "Divorced, Separated", 
                                                 "Widow, Widower")))
score_data$SUBSTRATUM <- droplevels(factor(score_data$SUBSTRATUM,
                                         levels = c(-1, 1, 9),
                                         labels = c("No Answer",
                                                    "Tbilisi", 
                                                    "Batumi")))


####Trust Score ####
modelScorePM2.5<- lm(TRUST_SCORE
                     ~ RESPAGE + 
                       #RESPSEX + RESPEMP + RESPPOB + RESPMAR + 
                       SUBSTRATUM +
                       PM2.5_hour# + PM2.5_lag1 + PM2.5_lag2
                     ,
                     data = score_data)
summary(modelScorePM2.5)
#summary(stepAIC(modelScorePM2.5))


modelScorePM10<- lm(TRUST_SCORE
                    ~ RESPAGE + 
                      #RESPSEX + RESPEMP + RESPPOB +RESPMAR + 
                      SUBSTRATUM +
                      PM10_hour# + PM10_lag1 + PM10_lag2
                    ,
                    data = score_data)
summary(modelScorePM10)
#summary(stepAIC(modelScorePM10))

modelScoreO3<- lm(TRUST_SCORE
                  ~ RESPAGE + 
                    #RESPSEX + RESPEMP + RESPPOB +RESPMAR + 
                    SUBSTRATUM +
                    O3_hour# + O.sub.3_lag1 + O.sub.3_lag2
                  ,
                  data = score_data)
summary(modelScoreO3)
#summary(stepAIC(modelScoreO3))

modelScoreCO<- lm(TRUST_SCORE
                  ~ RESPAGE + 
                    #RESPSEX + RESPEMP + RESPPOB +RESPMAR + 
                    SUBSTRATUM +
                    CO_hour# + CO_lag1 + CO_lag2
                  ,
                  data = score_data)
summary(modelScoreCO)
#summary(stepAIC(modelScoreCO))

modelScoreNO2<- lm(TRUST_SCORE
                   ~ RESPAGE +
                     #RESPSEX + RESPEMP + RESPPOB +RESPMAR + 
                     SUBSTRATUM +
                     NO2_hour# + NO.sub.2_lag1 + NO.sub.2_lag2
                   ,
                   data = score_data)
summary(modelScoreNO2)
#summary(stepAIC(modelScoreNO2))

modelScoreSO2<- lm(TRUST_SCORE
                   ~ RESPAGE + 
                     #RESPSEX + RESPEMP + RESPPOB +RESPMAR + 
                     SUBSTRATUM +
                     SO2_hour# + SO.sub.2_lag1 + SO.sub.2_lag2
                   ,
                   data = score_data)
summary(modelScoreSO2)
#summary(stepAIC(modelScoreSO2))

m1 <- modelScorePM2.5
m2 <- modelScorePM10
m3 <- modelScoreO3
m4 <- modelScoreCO
m5 <- modelScoreNO2
m6 <- modelScoreSO2


stargazer(m1, m2, m3, m4, m5, m6, type='html')

#### Set Trust Factors ####
trust_factors <- function(column){
  return(ordered(column,
                #ordered = TRUE,
                levels = c(-1, 1, 2, 3, 4, 5),
                labels = c("No Answer",
                            "Fully Distrust", 
                            "Rather Distrust",
                            "Neither Trust Nor Distrust",
                            "Rather Trust", 
                            "Fully Trust")))
}

air_data$TRUCRTS <- trust_factors(air_data$TRUCRTS)
air_data$TRUPARL <- trust_factors(air_data$TRUPARL)
air_data$TRUEXEC <- trust_factors(air_data$TRUEXEC)
air_data$TRUPRES <- trust_factors(air_data$TRUPRES)
air_data$TRUHLTH <- trust_factors(air_data$TRUHLTH)
air_data$TRUNGOS <- trust_factors(air_data$TRUNGOS)
air_data$TRUMEDI <- trust_factors(air_data$TRUMEDI)
air_data$TRULOCG <- trust_factors(air_data$TRULOCG)
air_data$TRURELI <- trust_factors(air_data$TRURELI)
air_data$TRUOMB <- trust_factors(air_data$TRUOMB)
air_data$TRUSTEU <- trust_factors(air_data$TRUSTEU)
air_data$TRUSTUN <- trust_factors(air_data$TRUSTUN)

summary(air_data$TRUST_SCORE)

install.packages("olsrr")
library("olsrr")

#### Create Summary Statistics Tables####
numerical_table <- stargazer(air_data, type = 'html')


#### Prepare Individual dataframes for regression ####
Courts_df <- subset(air_data, TRUCRTS != 'No Answer')
Courts_df$TRUCRTS <- droplevels(Courts_df$TRUCRTS)
Parl_df <- subset(air_data, TRUPARL != 'No Answer')
Parl_df$TRUPARL <- droplevels(Parl_df$TRUPARL)
Exec_df <- subset(air_data, TRUEXEC != 'No Answer')
Exec_df$TRUEXEC <- droplevels(Exec_df$TRUEXEC)
Local_df <- subset(air_data, TRULOCG != 'No Answer')
Local_df$TRULOCG <- droplevels(Local_df$TRULOCG)
Health_df <- subset(air_data, TRUHLTH != 'No Answer')
Health_df$TRUHLTH <- droplevels(Health_df$TRUHLTH)
NGO_df <- subset(air_data, TRUNGOS != 'No Answer')
NGO_df$TRUNGOS <- droplevels(NGO_df$TRUNGOS)
Media_df <- subset(air_data, TRUMEDI != 'No Answer')
Media_df$TRUMEDI <- droplevels(Media_df$TRUMEDI)
Religion_df <- subset(air_data, TRURELI != 'No Answer')
Religion_df$TRUMEDI <- droplevels(Religion_df$TRURELI)
Ombudsman_df <- subset(air_data, TRUOMB != 'No Answer')
Ombudsman_df$TRUOMB <- droplevels(Ombudsman_df$TRUOMB)
EU_df <- subset(air_data, TRUSTEU != 'No Answer')
EU_df$TRUSTEU <- droplevels(EU_df$TRUSTEU)
UN_df <- subset(air_data, TRUSTUN != 'No Answer')
UN_df$TRUSTUN <- droplevels(UN_df$TRUSTUN)


#### PM2.5 Regressions ####
modelCourts <- polr(TRUCRTS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      PM2.5_hour + PM2.5_lag1 + PM2.5_lag2,
                    data = Courts_df, Hess=TRUE)
#summary(modelCourts)
#modelCourts_parsed = step(modelCourts)
#summary(modelCourts_parsed)

modelParl <- polr(TRUPARL
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    PM2.5_hour + PM2.5_lag1 + PM2.5_lag2,
                    data = Parl_df, Hess=TRUE)
#summary(modelParl)
#modelParl_parsed = step(modelParl)
#summary(modelParl_parsed)

modelExec <- polr(TRUEXEC
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    PM2.5_hour + PM2.5_lag1 + PM2.5_lag2,
                    data = Exec_df, Hess=TRUE)
#summary(modelExec)
#modelExec_parsed = step(modelExec)
#summary(modelExec_parsed)

modelLocal <- polr(TRULOCG
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     #NO.sub.2 + 
                     #NO2_hour + 
                     #NO.sub.2_lag1 + NO.sub.2_lag2 + NO.sub.2_lag3 + NO.sub.2_lag4 + NO.sub.2_lag5 +
                     #SO.sub.2 + 
                     #SO2_hour +
                     #SO.sub.2_lag1 + SO.sub.2_lag2 + SO.sub.2_lag3 + SO.sub.2_lag4 + SO.sub.2_lag5 +
                     #PM2.5 + 
                     PM2.5_hour +
                     PM2.5_lag1 + PM2.5_lag2# + PM2.5_lag3# + PM2.5_lag4 + PM2.5_lag5 +
                     #PM10 +
                     #PM10_hour + 
                     #PM10_lag1 + PM10_lag2 + PM10_lag3 + PM10_lag4 + PM10_lag5 + PM10_lag5 +
                     #O.sub.3 + 
                     #O3_hour +
                     #O.sub.3_lag1 + O.sub.3_lag2 + O.sub.3_lag3 + O.sub.3_lag4 + O.sub.3_lag5 +
                     #CO +
                     #CO_hour +
                     #CO_lag1 + CO_lag2 + CO_lag3 + CO_lag4 + CO_lag5 
                   ,
                  data = Local_df, Hess=TRUE)
#summary(modelLocal)

modelHealth <- polr(TRUHLTH
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                  ,
                  data = Health_df, Hess=TRUE)
summary(modelHealth)

modelNGO <- polr(TRUNGOS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                    ,
                    data = NGO_df, Hess=TRUE)
summary(modelNGO)

modelMedia <- polr(TRUMEDI
                 ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                   PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                 ,
                 data = Media_df, Hess=TRUE)
summary(modelMedia)

modelReligion <- polr(TRURELI
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                   ,
                   data = Religion_df, Hess=TRUE)
summary(modelReligion)

modelOmbudsman <- polr(TRUOMB
                      ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                        PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                      ,
                      data = Ombudsman_df, Hess=TRUE)
summary(modelOmbudsman)

modelEU <- polr(TRUSTEU
                       ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                         PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                       ,
                       data = EU_df, Hess=TRUE)
summary(modelEU)

modelUN <- polr(TRUSTUN
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  PM2.5_hour + PM2.5_lag1 + PM2.5_lag2
                ,
                data = UN_df, Hess=TRUE)
summary(modelUN)



m1 <- modelCourts
m2 <- modelExec
m3 <- modelParl
m4 <- modelLocal
m5 <- modelHealth
m6 <- modelNGO
m7 <- modelMedia
m8 <- modelReligion
m9 <- modelOmbudsman
m10 <- modelEU
m11 <- modelUN
          
PM2.5_table <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, title="PM2.5 Regressions", type='html')

#### PM10 Regressions ####
modelCourts <- polr(TRUCRTS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      PM10_hour# + PM10_lag1 + PM10_lag2,
                    data = Courts_df, Hess=TRUE)
#summary(modelCourts)
#modelCourts_parsed = step(modelCourts)
#summary(modelCourts_parsed)

modelParl <- polr(TRUPARL
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    PM10_hour + PM10_lag1 + PM10_lag2,
                  data = Parl_df, Hess=TRUE)
#summary(modelParl)
#modelParl_parsed = step(modelParl)
#summary(modelParl_parsed)

modelExec <- polr(TRUEXEC
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    PM10_hour + PM10_lag1 + PM10_lag2,
                  data = Exec_df, Hess=TRUE)
#summary(modelExec)
#modelExec_parsed = step(modelExec)
#summary(modelExec_parsed)

modelLocal <- polr(TRULOCG
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     PM10_hour + PM10_lag1 + PM10_lag2,
                      data = Local_df, Hess=TRUE)

modelHealth <- polr(TRUHLTH
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      PM10_hour + PM10_lag1 + PM10_lag2,
                    data = Health_df, Hess=TRUE)
#summary(modelHealth)

modelNGO <- polr(TRUNGOS
                 ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                   PM10_hour + PM10_lag1 + PM10_lag2,
                 data = NGO_df, Hess=TRUE)
#summary(modelNGO)

modelMedia <- polr(TRUMEDI
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     PM10_hour + PM10_lag1 + PM10_lag2,
                   data = Media_df, Hess=TRUE)
#summary(modelMedia)

modelReligion <- polr(TRURELI
                      ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                        PM10_hour + PM10_lag1 + PM10_lag2,
                      data = Religion_df, Hess=TRUE)
#summary(modelReligion)

modelOmbudsman <- polr(TRUOMB
                       ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                         PM10_hour + PM10_lag1 + PM10_lag2,
                       data = Ombudsman_df, Hess=TRUE)
#summary(modelOmbudsman)

modelEU <- polr(TRUSTEU
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  PM10_hour + PM10_lag1 + PM10_lag2,
                data = EU_df, Hess=TRUE)
#summary(modelEU)

modelUN <- polr(TRUSTUN
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  PM10_hour + PM10_lag1 + PM10_lag2,
                data = UN_df, Hess=TRUE)
#summary(modelUN)
m1 <- modelCourts
m2 <- modelExec
m3 <- modelParl
m4 <- modelLocal
m5 <- modelHealth
m6 <- modelNGO
m7 <- modelMedia
m8 <- modelReligion
m9 <- modelOmbudsman
m10 <- modelEU
m11 <- modelUN

PM10_table <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, title="PM10 Regressions", type='html')

#### O3 Regressions ####
modelCourts <- polr(TRUCRTS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                    data = Courts_df, Hess=TRUE)
#summary(modelCourts)
#modelCourts_parsed = step(modelCourts)
#summary(modelCourts_parsed)

modelParl <- polr(TRUPARL
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                  data = Parl_df, Hess=TRUE)
#summary(modelParl)
#modelParl_parsed = step(modelParl)
#summary(modelParl_parsed)

modelExec <- polr(TRUEXEC
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                  data = Exec_df, Hess=TRUE)
#summary(modelExec)
#modelExec_parsed = step(modelExec)
#summary(modelExec_parsed)

modelLocal <- polr(TRULOCG
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                   data = Local_df, Hess=TRUE)

modelHealth <- polr(TRUHLTH
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                    data = Health_df, Hess=TRUE)
#summary(modelHealth)

modelNGO <- polr(TRUNGOS
                 ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                   O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                 data = NGO_df, Hess=TRUE)
#summary(modelNGO)

modelMedia <- polr(TRUMEDI
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                   data = Media_df, Hess=TRUE)
#summary(modelMedia)

modelReligion <- polr(TRURELI
                      ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                        O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                      data = Religion_df, Hess=TRUE)
#summary(modelReligion)

modelOmbudsman <- polr(TRUOMB
                       ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                         O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                       data = Ombudsman_df, Hess=TRUE)
#summary(modelOmbudsman)

modelEU <- polr(TRUSTEU
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                data = EU_df, Hess=TRUE)
#summary(modelEU)

modelUN <- polr(TRUSTUN
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  O3_hour + O.sub.3_lag1 + O.sub.3_lag2,
                data = UN_df, Hess=TRUE)
#summary(modelUN)
m1 <- modelCourts
m2 <- modelExec
m3 <- modelParl
m4 <- modelLocal
m5 <- modelHealth
m6 <- modelNGO
m7 <- modelMedia
m8 <- modelReligion
m9 <- modelOmbudsman
m10 <- modelEU
m11 <- modelUN

O3_table <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, title="O<sub>3</sub> Regressions", type='html')

#### CO Regressions ####
modelCourts <- polr(TRUCRTS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      CO_hour + CO_lag1 + CO_lag2,
                    data = Courts_df, Hess=TRUE)
#summary(modelCourts)
#modelCourts_parsed = step(modelCourts)
#summary(modelCourts_parsed)

modelParl <- polr(TRUPARL
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    CO_hour + CO_lag1 + CO_lag2,
                  data = Parl_df, Hess=TRUE)
#summary(modelParl)
#modelParl_parsed = step(modelParl)
#summary(modelParl_parsed)

modelExec <- polr(TRUEXEC
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    CO_hour + CO_lag1 + CO_lag2,
                  data = Exec_df, Hess=TRUE)
#summary(modelExec)
#modelExec_parsed = step(modelExec)
#summary(modelExec_parsed)

modelLocal <- polr(TRULOCG
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     CO_hour + CO_lag1 + CO_lag2,
                   data = Local_df, Hess=TRUE)

modelHealth <- polr(TRUHLTH
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      CO_hour + CO_lag1 + CO_lag2,
                    data = Health_df, Hess=TRUE)
#summary(modelHealth)

modelNGO <- polr(TRUNGOS
                 ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                   CO_hour + CO_lag1 + CO_lag2,
                 data = NGO_df, Hess=TRUE)
#summary(modelNGO)

modelMedia <- polr(TRUMEDI
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     CO_hour + CO_lag1 + CO_lag2,
                   data = Media_df, Hess=TRUE)
#summary(modelMedia)

modelReligion <- polr(TRURELI
                      ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                        CO_hour + CO_lag1 + CO_lag2,
                      data = Religion_df, Hess=TRUE)
#summary(modelReligion)

modelOmbudsman <- polr(TRUOMB
                       ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                         CO_hour + CO_lag1 + CO_lag2,
                       data = Ombudsman_df, Hess=TRUE)
#summary(modelOmbudsman)

modelEU <- polr(TRUSTEU
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  CO_hour + CO_lag1 + CO_lag2,
                data = EU_df, Hess=TRUE)
#summary(modelEU)

modelUN <- polr(TRUSTUN
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  CO_hour + CO_lag1 + CO_lag2,
                data = UN_df, Hess=TRUE)
#summary(modelUN)
m1 <- modelCourts
m2 <- modelExec
m3 <- modelParl
m4 <- modelLocal
m5 <- modelHealth
m6 <- modelNGO
m7 <- modelMedia
m8 <- modelReligion
m9 <- modelOmbudsman
m10 <- modelEU
m11 <- modelUN

CO_table <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, title="CO Regressions", type='html')

#### NO2 Regressions ####
modelCourts <- polr(TRUCRTS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                    data = Courts_df, Hess=TRUE)
#summary(modelCourts)
#modelCourts_parsed = step(modelCourts)
#summary(modelCourts_parsed)

modelParl <- polr(TRUPARL
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                  data = Parl_df, Hess=TRUE)
#summary(modelParl)
#modelParl_parsed = step(modelParl)
#summary(modelParl_parsed)

modelExec <- polr(TRUEXEC
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                  data = Exec_df, Hess=TRUE)
#summary(modelExec)
#modelExec_parsed = step(modelExec)
#summary(modelExec_parsed)

modelLocal <- polr(TRULOCG
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                   data = Local_df, Hess=TRUE)

modelHealth <- polr(TRUHLTH
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                    data = Health_df, Hess=TRUE)
#summary(modelHealth)

modelNGO <- polr(TRUNGOS
                 ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                   NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                 data = NGO_df, Hess=TRUE)
#summary(modelNGO)

modelMedia <- polr(TRUMEDI
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                   data = Media_df, Hess=TRUE)
#summary(modelMedia)

modelReligion <- polr(TRURELI
                      ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                        NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                      data = Religion_df, Hess=TRUE)
#summary(modelReligion)

modelOmbudsman <- polr(TRUOMB
                       ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                         NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                       data = Ombudsman_df, Hess=TRUE)
#summary(modelOmbudsman)

modelEU <- polr(TRUSTEU
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                data = EU_df, Hess=TRUE)
#summary(modelEU)

modelUN <- polr(TRUSTUN
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  NO2_hour + NO.sub.2_lag1 + NO.sub.2_lag2,
                data = UN_df, Hess=TRUE)
#summary(modelUN)
m1 <- modelCourts
m2 <- modelExec
m3 <- modelParl
m4 <- modelLocal
m5 <- modelHealth
m6 <- modelNGO
m7 <- modelMedia
m8 <- modelReligion
m9 <- modelOmbudsman
m10 <- modelEU
m11 <- modelUN

NO2_table <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, title="NO<sub>2</sub> Regressions", type='html')

#### SO2 Regressions ####
modelCourts <- polr(TRUCRTS
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                    data = Courts_df, Hess=TRUE)
#summary(modelCourts)
#modelCourts_parsed = step(modelCourts)
#summary(modelCourts_parsed)

modelParl <- polr(TRUPARL
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                  data = Parl_df, Hess=TRUE)
#summary(modelParl)
#modelParl_parsed = step(modelParl)
#summary(modelParl_parsed)

modelExec <- polr(TRUEXEC
                  ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                    SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                  data = Exec_df, Hess=TRUE)
#summary(modelExec)
#modelExec_parsed = step(modelExec)
#summary(modelExec_parsed)

modelLocal <- polr(TRULOCG
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                   data = Local_df, Hess=TRUE)

modelHealth <- polr(TRUHLTH
                    ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                      SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                    data = Health_df, Hess=TRUE)
#summary(modelHealth)

modelNGO <- polr(TRUNGOS
                 ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                   SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                 data = NGO_df, Hess=TRUE)
#summary(modelNGO)

modelMedia <- polr(TRUMEDI
                   ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                     SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                   data = Media_df, Hess=TRUE)
#summary(modelMedia)

modelReligion <- polr(TRURELI
                      ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                        SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                      data = Religion_df, Hess=TRUE)
#summary(modelReligion)

modelOmbudsman <- polr(TRUOMB
                       ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                         SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                       data = Ombudsman_df, Hess=TRUE)
#summary(modelOmbudsman)

modelEU <- polr(TRUSTEU
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                data = EU_df, Hess=TRUE)
#summary(modelEU)

modelUN <- polr(TRUSTUN
                ~ RESPAGE + RESPSEX + RESPEMP + RESPPOB +RESPMAR + SUBSTRATUM +
                  SO2_hour + SO.sub.2_lag1 + SO.sub.2_lag2,
                data = UN_df, Hess=TRUE)
#summary(modelUN)
m1 <- modelCourts
m2 <- modelExec
m3 <- modelParl
m4 <- modelLocal
m5 <- modelHealth
m6 <- modelNGO
m7 <- modelMedia
m8 <- modelReligion
m9 <- modelOmbudsman
m10 <- modelEU
m11 <- modelUN

SO2_table <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, title="SO<sub>2</sub> Regressions", type='html')












