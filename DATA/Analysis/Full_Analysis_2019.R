library(MASS)

air_data = read.csv('/Users/nicorapallo/Desktop/GitHub/AIR-QUALITY-PROJECT/DATA/Filtered Data/Complied Data/compiled_data_2019.csv')
original_data = read_dta('Desktop/GitHub/AIR-QUALITY-PROJECT/DATA/Raw Data/Caucus Barometer/CB_2017_Georgia_public_17.11.17.dta')

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

#### Courts Regression ####
Courts_df <- subset(air_data, TRUCRTS != 'No Answer')
Courts_df$TRUCRTS <- droplevels(Courts_df$TRUCRTS)

modelCourts <- polr(as.factor(TRUCRTS)
                     ~ RESPAGE + as.factor(RESPSEX) + as.factor(RESPEMP) + as.factor(RESPPOB) + as.factor(RESPMAR) + 
                      NO.sub.2 + 
                        NO.sub.2_lag1 + NO.sub.2_lag2 + NO.sub.2_lag3 + NO.sub.2_lag4 + NO.sub.2_lag5 +
                      SO.sub.2 + 
                        SO.sub.2_lag1 + SO.sub.2_lag2 + SO.sub.2_lag3 + SO.sub.2_lag4 + SO.sub.2_lag5 +
                      PM2.5 + 
                        PM2.5_lag1 + PM2.5_lag2 + PM2.5_lag3 + PM2.5_lag4 + PM2.5_lag5 +
                      PM10 + 
                        PM10_lag1 + PM10_lag2 + PM10_lag3 + PM10_lag4 + PM10_lag5 + PM10_lag5 +
                      O.sub.3 + 
                        O.sub.3_lag1 + O.sub.3_lag2 + O.sub.3_lag3 + O.sub.3_lag4 + O.sub.3_lag5 +
                      CO +
                        CO_lag1 + CO_lag2 + CO_lag3 + CO_lag4 + CO_lag5 
                    , 
                    data = Courts_df, Hess=TRUE)
summary(modelCourts)
modelCourts_parsed = step(modelCourts)
summary(modelCourts_parsed)

#### Parliament Regression ####
Parl_df <- subset(air_data, TRUCRTS != 'No Answer')
Parl_df$TRUPARL <- droplevels(Courts_df$TRUPARL)

modelParl <- polr(as.factor(TRUPARL)
                    ~ RESPAGE + as.factor(RESPSEX) + as.factor(RESPEMP) + as.factor(RESPPOB) + as.factor(RESPMAR) + 
                      NO.sub.2 + 
                      NO.sub.2_lag1 + NO.sub.2_lag2 + NO.sub.2_lag3 + NO.sub.2_lag4 + NO.sub.2_lag5 +
                      SO.sub.2 + 
                      SO.sub.2_lag1 + SO.sub.2_lag2 + SO.sub.2_lag3 + SO.sub.2_lag4 + SO.sub.2_lag5 +
                      PM2.5 + 
                      PM2.5_lag1 + PM2.5_lag2 + PM2.5_lag3 + PM2.5_lag4 + PM2.5_lag5 +
                      PM10 + 
                      PM10_lag1 + PM10_lag2 + PM10_lag3 + PM10_lag4 + PM10_lag5 + PM10_lag5 +
                      O.sub.3 + 
                      O.sub.3_lag1 + O.sub.3_lag2 + O.sub.3_lag3 + O.sub.3_lag4 + O.sub.3_lag5 +
                      CO +
                      CO_lag1 + CO_lag2 + CO_lag3 + CO_lag4 + CO_lag5 
                    , 
                    data = Parl_df, Hess=TRUE)
summary(modelParl)
modelParl_parsed = step(modelParl)
summary(modelParl_parsed)

#### Executive Regression ####
Exec_df <- subset(air_data, TRUEXEC != 'No Answer')
Exec_df$TRUEXEC <- droplevels(Exec_df$TRUEXEC)

modelExec <- polr(as.factor(TRUEXEC)
                  ~ RESPAGE + as.factor(RESPSEX) + as.factor(RESPEMP) + as.factor(RESPPOB) + as.factor(RESPMAR) + 
                    NO.sub.2 + 
                    NO.sub.2_lag1 + NO.sub.2_lag2 + NO.sub.2_lag3 + NO.sub.2_lag4 + NO.sub.2_lag5 +
                    SO.sub.2 + 
                    SO.sub.2_lag1 + SO.sub.2_lag2 + SO.sub.2_lag3 + SO.sub.2_lag4 + SO.sub.2_lag5 +
                    PM2.5 + 
                    PM2.5_lag1 + PM2.5_lag2 + PM2.5_lag3 + PM2.5_lag4 + PM2.5_lag5 +
                    PM10 + 
                    PM10_lag1 + PM10_lag2 + PM10_lag3 + PM10_lag4 + PM10_lag5 + PM10_lag5 +
                    O.sub.3 + 
                    O.sub.3_lag1 + O.sub.3_lag2 + O.sub.3_lag3 + O.sub.3_lag4 + O.sub.3_lag5 +
                    CO +
                    CO_lag1 + CO_lag2 + CO_lag3 + CO_lag4 + CO_lag5 
                  , 
                  data = Exec_df, Hess=TRUE)
summary(modelExec)
modelExec_parsed = step(modelExec)
summary(modelExec_parsed)

#### Local Government Regression ####
Local_df <- subset(air_data, TRULOCG != 'No Answer')
Local_df$TRULOCG <- droplevels(Local_df$TRULOCG)

modelLocal <- polr(as.factor(TRULOCG)
                   ~ RESPAGE + as.factor(RESPSEX) + as.factor(RESPEMP) + as.factor(RESPPOB) + as.factor(RESPMAR) + 
                     NO.sub.2 + 
                     NO.sub.2_lag1 + NO.sub.2_lag2 + NO.sub.2_lag3 + NO.sub.2_lag4 + NO.sub.2_lag5 +
                     SO.sub.2 + 
                     SO.sub.2_lag1 + SO.sub.2_lag2 + SO.sub.2_lag3 + SO.sub.2_lag4 + SO.sub.2_lag5 +
                     PM2.5 + 
                     PM2.5_lag1 + PM2.5_lag2 + PM2.5_lag3 + PM2.5_lag4 + PM2.5_lag5 +
                     PM10 + 
                     PM10_lag1 + PM10_lag2 + PM10_lag3 + PM10_lag4 + PM10_lag5 + PM10_lag5 +
                     O.sub.3 + 
                     O.sub.3_lag1 + O.sub.3_lag2 + O.sub.3_lag3 + O.sub.3_lag4 + O.sub.3_lag5 +
                     CO +
                     CO_lag1 + CO_lag2 + CO_lag3 + CO_lag4 + CO_lag5 
                   , 
                  data = Local_df, Hess=TRUE)

summary(modelLocal)

