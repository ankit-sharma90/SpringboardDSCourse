## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

## Load the data
  setwd("/Volumes/GoogleDrive/My Drive/TouchBistro/Other/Springboard Data Science Course/Exercise Files/ML/logistic_regression")
  NH11 <- readRDS("dataSets/NatHealth2011.rds")
  glimpse(NH11)
  
## Review data - everwrk. Conclusion - many missing values (~57%)
  levels(NH11$everwrk)
  NH11 %>% count(everwrk)
  
## Review data - age_p.  Conclusion - no missing values
  summary(NH11$age_p)
  
## Review data - r_maritl. Conclusion - few missing values (~2%)
  levels(NH11$r_maritl)
  NH11 %>% count(r_maritl)

## Identify pattern for missing data, can we fill it in intelligently?
  data1 <- select(NH11,everwrk,age_p,r_maritl) #create df limited to the 3 variables being evaluated
  glimpse(data1)
  
  data_everwrk_na <- filter(data1,is.na(everwrk)) ##filter for missing values in everwrk
  summary(data_everwrk_na$age_p) ##missing values in everwrk span all ages, no pattern observed
  data_everwrk_na %>% count(r_maritl) ##missing values in everwrk span all marital statuses, no pattern observed
  
## No pattern found in missing data, we will exclude these observations from our model
  data1$everwrk <- factor(data1$everwrk, levels=c("2 No", "1 Yes"))
  data1clean <- na.omit(data1)
  glimpse(data1clean)  
  summary(data1clean)  

## Build regression model
  mod1 <- glm(everwrk ~ age_p + r_maritl,family = "binomial",data = data1clean)
  summary(mod1)
  mod1.tab <- coef(summary(mod1))
  mod1.tab[,"Estimate"] <- exp(coef(mod1))
  mod1.tab
  
## Use model to predict
  mod1predict <- predict.glm(mod1,type="response")
  plot(predictorEffect("r_maritl",mod1))
  data.frame(Effect("r_maritl", mod1))
  
 
