
##### load libraries
library(tidyverse)
library(ROCR)
library(caret)
library(e1071)
library(readr)
library(lubridate)
library(tree)



##### Import Data #####
Feb_24_SF_report <- read_csv("Feb-24-SF-report-updated.csv", 
    col_types = cols(`Admin Training Completed` = col_datetime(format = "%d-%m-%Y %H:%M"), 
        `Customer Acq Date` = col_datetime(format = "%d-%m-%Y"), 
        `Menu Training Completed` = col_datetime(format = "%d-%m-%Y %H:%M"), 
        `POS Cloud 100th Bill Processed On (sys)` = col_datetime(format = "%d-%m-%Y %H:%M"), 
        `POS Cloud Last Bill Processed On (sys)` = col_datetime(format = "%d-%m-%Y %H:%M"), 
        `POS Last Checkin (sys)` = col_datetime(format = "%d-%m-%Y %H:%M")))
glimpse(Feb_24_SF_report)

Feb_25_Support_Cases_report <- read_csv("Feb-25-Support-Cases-report.csv", 
                                        col_types = cols(`Date/Time Closed` = col_datetime(format = "%d/%m/%Y %I:%M %p"), 
                                                         `Date/Time Opened` = col_datetime(format = "%d/%m/%Y %I:%M %p")))
glimpse(Feb_25_Support_Cases_report)


Cloudenabled_and_licensetype <- read_csv("Cloudenabled_and_licensetype.csv")
glimpse(Cloudenabled_and_licensetype)

OverdueInvoices_forR_v2 <- read_csv("OverdueInvoices_forR_v2.csv")

P7d_sales <- read_csv("Sales_data/P7d_sales.csv", 
                      col_types = cols(`Last Bill Upload` = col_skip()))

P14d_sales <- read_csv("Sales_data/P14d_sales.csv", 
                      col_types = cols(`Last Bill Upload` = col_skip()))

P30d_sales <- read_csv("Sales_data/P30d_sales.csv", 
                       col_types = cols(`Last Bill Upload` = col_skip()))

P60d_sales <- read_csv("Sales_data/P60d_sales.csv", 
                       col_types = cols(`Last Bill Upload` = col_skip()))

P90d_sales <- read_csv("Sales_data/P90d_sales.csv", 
                       col_types = cols(`Last Bill Upload` = col_skip()))

dataSF <- Feb_24_SF_report
dataSQL <- Cloudenabled_and_licensetype
dataSC <- Feb_25_Support_Cases_report
dataInvoices <- OverdueInvoices_forR_v2


#########################################
#Cleanup SF Support Cases data
#########################################

glimpse(dataSC)

dataSC %>% 
  rename (
    CaseID = `Case ID`,
    AccountName = `Account Name`,
    AssociatedCaseReason = `Associated Case Reason`,
    PrimaryReason = `Primary Reason`,
    OpenDate = `Date/Time Opened`,
    CloseDate = `Date/Time Closed`,
    Age = `Age (Days)`
  ) -> dataSC

# count missing values in Restaurant ID column
dataSC %>% 
  count(is.na(RestaurantID))

    dataSC %>% 
      filter(is.na(RestaurantID)) %>%
        count(is.na(RestaurantID)==TRUE,AccountName) %>%
        arrange(desc(n))  #most missing values are for "Touchbistro" Account
    
    dataSC %>% 
      na.omit(RestaurantID) -> dataSC ##no more missing values

# check for missing values in other columns
dataSC %>% 
  count(Status)

dataSC %>% 
  count(AssociatedCaseReason)

dataSC %>% 
  count(PrimaryReason)

dataSC %>% 
  count(OpenDate-OpenDate)

dataSC %>% 
  count(CloseDate-CloseDate)

#cases by Age
dataSC %>% 
  count(Age*24) %>% 
    mutate(`% Total` = round(n / sum(n)*100,1))

dataSC %>% 
  filter(Age == 0) %>% 
    count(Status) #most cases that are closed/resolved have an Age of 0.  This is normal according to Michael from Support.

dataSC %>% 
  count(RestaurantID,Age*24) -> dataSummarizedSC

dataSummarizedSC %>% 
  rename(
    AgeHrs = `Age * 24`
  ) -> dataSummarizedSC

dataSummarizedSC %>% 
  mutate(
    AgeType = case_when (
      AgeHrs == 0 ~ "CaseResolve_Immediate",
      AgeHrs < 24 ~ "CaseResolve_<24hrs",
      AgeHrs < 168 ~ "CaseResolve_<1wk",
      AgeHrs >= 168 ~ "CaseResolve_>1wk") 
    ) -> dataSummarizedSC

dataSummarizedSC %>% 
  group_by(AgeType) %>% summarise(sum(n)) %>% 
    mutate(`% Total` = round(`sum(n)` / sum(`sum(n)`)*100,1)) %>% 
      arrange(desc(`% Total`))

glimpse(dataSummarizedSC)

dataSummarizedSC %>% 
  group_by(RestaurantID,AgeType) %>% 
    summarise(sum(n)) -> dataSummarizedSC

dataSummarizedSC %>% rename(TotalCases=`sum(n)`) -> dataSummarizedSC

glimpse(dataSummarizedSC)

dataSummarizedSC %>% 
  spread(AgeType,TotalCases,fill=0) -> dataSummarizedSC_Spread

### How many Venues have called in with at least one support ticket since Sept?
n_distinct(dataSC$RestaurantID) #--> #Unique RestaurantIDs that have Support Call
n_distinct(dataSF$ID) # --> #Total Restaurants based on Salesforce

###############################
#Cleanup Overdue Invoices data
###############################

glimpse(dataInvoices)
summary(dataInvoices)

# dataInvoices <- dataInvoices %>% gather("Num_overdue_invoices","n",2:7)

# dataInvoices %>% 
# na.omit(n) -> dataInvoices ##no more missing values

# glimpse(dataInvoices)
# summary(dataInvoices)



##### explore data - CxAcqDate
data <- mutate(data,CxAcqYrMnth = format(as.Date(data$CxAcqDate, format="%Y-%m-%d"),"%y-%b"))

# ggplot(data,aes(CxAcqYrMnth,fill = CxType)) +
#   geom_bar(position="fill") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########################################
#Cleanup Sales volume data
#########################################
PXd_sales <- inner_join(P7d_sales,P14d_sales,by="ID")
PXd_sales <- inner_join(PXd_sales,P30d_sales,by="ID")
PXd_sales <- inner_join(PXd_sales,P60d_sales,by="ID")
PXd_sales <- inner_join(PXd_sales,P90d_sales,by="ID")
glimpse(PXd_sales)

PXd_sales %>% 
  mutate (
    Daily_Sales_7d=Sales_7d/7,
    Daily_Sales_14d=Sales_14d/14,
    Daily_Sales_30d=Sales_30d/30,
    Daily_Sales_60d=Sales_60d/60,
    Daily_Sales_90d=Sales_90d/90
  ) -> PXd_sales

PXd_sales %>% 
  mutate (
    P7d_90d_ratio=Daily_Sales_7d/Daily_Sales_90d,
    P14d_90d_ratio=Daily_Sales_14d/Daily_Sales_90d,
    P30d_90d_ratio=Daily_Sales_30d/Daily_Sales_90d,
    P60d_90d_ratio=Daily_Sales_60d/Daily_Sales_90d
  ) -> PXd_sales

#########################################
############### Cleanup SF data #########
#########################################

## rename columns
glimpse(dataSF)

dataSF %>% 
  rename (
    ID = RestaurantID
    ,AccountName = `Account Name`
    ,Country = `Venue Country`
    ,LastCloudBill = `POS Cloud Last Bill Processed On (sys)`
    ,`100thBill`=`POS Cloud 100th Bill Processed On (sys)`
    ,CxLive = `Customer Live`
    ,CxType = Type
    ,LastCheckin = `POS Last Checkin (sys)`
    ,TBVersion = `POS Version (sys)`
    ,State = `Venue State/Province`
    ,City = `Venue City`
    ,VenueType = `Venue Type`
    ,ConceptType = `Concept Type`
    ,CxAcqDate = `Customer Acq Date`
    ,TrainingCompleted = `Training Completed`
          ) -> dataSF

glimpse(dataSF)

## remove missing values from SF data (6 rows)
dataSF <- filter(dataSF,is.na(AccountName)==FALSE)

##create column for dependent variable (HasChurned)
dataSF <- mutate(dataSF,HasChurned=ifelse(dataSF$CxType=="CHURN",1,0))
dataSF$HasChurned <- as.factor(dataSF$HasChurned)
glimpse(dataSF)
dataSF %>% count(HasChurned)

##### Cleanup SQL data #####
glimpse(dataSQL)
dataSQL <- rename(dataSQL,ID = RestaurantUser_Id)
dataSQL <- rename(dataSQL,LicenseType = `License Type`)

## rename inconsistent values in Cloud_enabled column
dataSQL %>% count(Cloud_Enabled)
dataSQL$Cloud_Enabled <- gsub("true",1,dataSQL$Cloud_Enabled)
dataSQL %>% count(Cloud_Enabled)

## Fill in missing values for Cloud_enabled column (SQL data)
dataSQL %>% count(Cloud_Enabled)
dataSQL$Cloud_Enabled <- replace_na(dataSQL$Cloud_Enabled,"Unknown")
dataSQL %>% count(Cloud_Enabled)

## Remove Venues with Cloud disabled (SQL data)
dataSQL %>% count(Cloud_Enabled)
dataSQL <- dataSQL %>% filter(Cloud_Enabled != 0)
dataSQL %>% count(Cloud_Enabled)

## check remaining columns for anomalies
dataSQL %>% count(LicensePlan)
dataSQL %>% count(LicenseType)

  
## Remove Venues without 100th Bill (ie Cx not Live)
dataSF %>% count(`100thBill`==FALSE)
dataSF <- dataSF %>% filter(is.na(`100thBill`)==FALSE)
dataSF %>% count(`100thBill`==FALSE)

# ## Fill missing values in Last Checkin
dataSF %>% count(is.na(LastCheckin))

dataSF <- dataSF %>% 
  mutate(LastCheckinFilled = ifelse( is.na(LastCheckin),LastCloudBill,LastCheckin)) %>% 
    mutate(LastCheckinFilled = as_datetime(LastCheckinFilled))

glimpse(dataSFtest)

dataSF %>% count(is.na(LastCheckinFilled))
dataSF %>% select(LastCheckin,LastCheckinFilled,LastCloudBill)

####################################
##### Merge Datasets & Compare #####
####################################

datafull <- full_join(dataSF,dataSQL,by = "ID")
glimpse(datafull)

datainner <- inner_join(dataSF,dataSQL,by = "ID")
glimpse(datainner)

  difftotal <- anti_join(datafull,datainner)
  glimpse(difftotal)

dataleft <- left_join(dataSF,dataSQL,by = "ID")
glimpse(dataleft)

  diffleft <- anti_join(datafull,dataleft)
  glimpse(diffleft)
  diffleft %>% count(LicensePlan)

dataright <- right_join(dataSF,dataSQL,by = "ID")
glimpse(dataright)

  diffright <- anti_join(datafull,dataright)
  glimpse(diffright)
  diffright %>% count(CxType)

##### data in SQL data pull (rldco, rd, & rcs tables) excludes many Churned Customers.  Not sure why.

## Merge SQL data & Salesforce Data
data <- left_join(dataSF,dataSQL,by = "ID")
data$CxType <- as.factor(data$CxType)
glimpse(data)
data %>% count(CxType)

## Merge above with Support Ticket data
data <- left_join(data,dataSummarizedSC_Spread,by = c("ID" = "RestaurantID"))
glimpse(data)
data$`CaseResolve_Immediate` <- replace_na(data$CaseResolve_Immediate,0)
data$`CaseResolve_<24hrs` <- replace_na(data$`CaseResolve_<24hrs`,0)
data$`CaseResolve_<1wk` <- replace_na(data$`CaseResolve_<1wk`,0)
data$`CaseResolve_>1wk` <- replace_na(data$`CaseResolve_>1wk`,0)
glimpse(data)

data <- left_join(data,dataInvoices,by=c("AccountName"="Account_Name"))
glimpse(data)
data$Num_overdue_invoices <- data$Num_overdue_invoices %>% replace_na(0)
glimpse(data)

## Merge above with Sales data
data <- inner_join(data,PXd_sales,by = "ID")
anti_join(data,PXd_sales,by = "ID") #only 81 venues are not in both datasets

##### explore data - Sales

data %>% summary

##### explore data - Country #####

# % of venues by Country
data %>% 
  count(Country) %>% 
    arrange(desc(n))

# Churn rate by Country
data %>% 
  count(Country,CxType) %>% 
    arrange(desc(n)) -> data.cntry

data.cntry <- spread(data.cntry,"CxType","n")
data.cntry$Customer <- data.cntry$Customer %>% replace_na(0)
data.cntry$CHURN <- data.cntry$CHURN %>% replace_na(0)

data.cntry %>% 
  mutate(Churn_rate = round(CHURN/(Customer + CHURN)*100)) -> data.cntry
         
data.cntry %>% arrange(desc(CHURN))

##### explore data - Cloud enabled #####
data %>% 
  count(Cloud_Enabled)

##### explore data - Venue Type #####

# % of venues by Venue Type
data %>% 
  count(VenueType) %>% 
    arrange(desc(n))

# Churn rate by Venue Type

data %>% 
  count(VenueType,CxType) %>% 
    arrange(desc(n)) -> data.venuetype

data.venuetype <- spread(data.venuetype,"CxType","n")

data.venuetype %>% 
  mutate(Churn_rate = round(CHURN/(Customer + CHURN)*100)) -> data.venuetype

data.venuetype %>% arrange(desc(CHURN))

chisq.test(data$VenueType,data$HasChurned)
  
# ggplot(data,aes(VenueType,fill = CxType,na.rm = TRUE)) +
#   geom_bar(position="fill") +
#   scale_y_continuous(labels =percent_format())

# Missing Venue Type data
data %>% count(VenueType)
data.venuetypemissing <- filter(data,is.na(VenueType)==TRUE)
data.venuetypemissing <- mutate(data.venuetypemissing,`100thBillYear` = format(as.Date(data.venuetypemissing$`100thBill`, format="%Y-%m-%d"),"%Y"))
data.venuetypemissing %>% count(`100thBillYear`) #most missing data is from 2016 & 2017


# Churn rate by Venue Type

data %>% 
  count(LicenseType,CxType) %>% 
    arrange(desc(n)) -> data.licensetype

data.licensetype <- spread(data.licensetype,"CxType","n")

data.licensetype %>% 
  mutate(Churn_rate = round(CHURN/(Customer + CHURN)*100)) -> data.licensetype

data.licensetype %>% arrange(desc(CHURN))

chisq.test(data$LicenseType,data$HasChurned)
  
# ggplot(data,aes(LicenseType,fill = CxType,na.rm = TRUE)) +
#   geom_bar(position="fill") +
#   scale_y_continuous(labels =percent_format())

# Missing Venue Type data
data %>% count(LicenseType)
data.licensetypemissing <- filter(data,is.na(LicenseType)==TRUE)
data.licensetypemissing <- mutate(data.licensetypemissing,`100thBillYear` = format(as.Date(data.licensetypemissing$`100thBill`, format="%Y-%m-%d"),"%Y"))
data.licensetypemissing %>% count(`100thBillYear`)


##### explore data - Concept Type #####
# % of venues by Concept Type
data %>% 
  count(ConceptType) %>% 
    arrange(desc(n))

# Churn rate by Concept Type

data %>% 
  count(ConceptType,CxType) %>% 
    arrange(desc(n)) -> data.concepttype

data.concepttype <- spread(data.concepttype,"CxType","n")

data.concepttype %>% 
  mutate(Churn_rate = round(CHURN/(Customer + CHURN)*100)) -> data.concepttype

data.concepttype %>% arrange(desc(CHURN))

# view(data.concepttype %>% arrange(desc(CHURN)))

##### explore data - Customer Age #####
data <- mutate(data,HundredthBillYrMnth = format(as.Date(data$`100thBill`, format="%Y-%m-%d"),"%y-%m"))

ggplot(data,aes(HundredthBillYrMnth,fill = CxType)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(VenueType ~ .)
# conclusion: churn rate is not lower for old Customers


##### explore data - Training completed #####

# data.traincomplete <- 
  
data %>% count(TrainingCompleted,CxType)

round(prop.table(data.traincomplete$n)*100,0)

# view(data %>% count(TrainingCompleted,HundredthBillYrMnth))

# ggplot(data,aes(HundredthBillYrMnth,fill=as.factor(TrainingCompleted))) +
#   geom_bar(position="fill") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_grid(VenueType ~ .)

data$TrainingCompleted <- as.factor(data$TrainingCompleted)

##### explore data - Latest Cloud Bill #####
summary(data$LastCloudBill)
data <- mutate(data,LatestCloudBillYrMnth = format(as.Date(data$LastCloudBill, format="%Y-%m-%d %H:%M:%S"),"%y-%m"))

# ggplot(data,aes(LatestCloudBillYrMnth,fill=as.factor(CxType))) +
#   geom_bar(position="fill") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))



##### explore data - #Churned Cx that had a Churn date (LatestCloudBill) after Sept 2018 (ie where Support Ticket Data exists) #####
datatest <- data %>% filter(HasChurned==1)

# ggplot(datatest,aes(LatestCloudBillYrMnth)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

datatest %>% 
  count(LatestCloudBillYrMnth>'18-08') %>% 
    mutate(`% Total` = round(n / sum(n)*100,1))
    ##Conclusion: 30% of Venues Churned in the period where we have Support Ticket data

##########################################
###Subset Cloud Bill Date & Checkin Date##
##########################################

data %>% mutate(Bill = ifelse((as_date("2019-02-25")-as_date(LastCloudBill))>7,0,1))

data <- data %>% mutate(LastCloudBillCategory = case_when(
  as_date("2019-02-25") - as_date(LastCloudBill) <= 7 ~ "P7D",
  as_date("2019-02-25") - as_date(LastCloudBill) <= 14 ~ "P14D",
  as_date("2019-02-25") - as_date(LastCloudBill) > 14 ~ ">14D"
))

data <- data %>% 
  mutate(DaysSinceCloudBill = as.integer(as_date("2019-02-2") - as_date(LastCloudBill)))

glimpse(data)

data %>% count(LastCloudBillCategory)
data %>% count(CxType,LastCloudBillCategory)
## 33% of Venues with no Cloud bill within P14D are still marked as Cx

data %>% mutate(Bill = ifelse((as_date("2019-02-25")-as_date(LastCheckinFilled))>7,0,1))

data <- data %>% mutate(LastCheckinCategory = case_when(
  as_date("2019-02-25") - as_date(LastCheckinFilled) <= 7 ~ "P7D",
  as_date("2019-02-25") - as_date(LastCheckinFilled) <= 14 ~ "P14D",
  as_date("2019-02-25") - as_date(LastCheckinFilled) > 14 ~ ">14D"
))

data <- data %>% 
  mutate(DaysSinceCheckin = as.integer(as_date("2019-02-25") - as_date(LastCheckinFilled)))

glimpse(data)

data %>% count(LastCheckinCategory)

################################################################################
### Remove bad data (last bill upload >14 days ago & still marked as Customer)##
################################################################################

data %>% count(CxType,LastCloudBillCategory)

dataexclude <- data %>% filter(LastCloudBillCategory==">14D" & CxType == "Customer")
data2 <- anti_join(data,dataexclude,by = "ID")

data2 <- 
  data2 %>% 
    rename(
      CaseResolve_oneDay = `CaseResolve_<24hrs`,
      CaseResolve_lessthanoneWeek = `CaseResolve_<1wk`,
      CaseResolve_morethanoneWeek = `CaseResolve_>1wk`
)

data2 <-
  data2 %>% 
    mutate(
      CasesTotal = 
         CaseResolve_Immediate +
         CaseResolve_oneDay +
         CaseResolve_lessthanoneWeek +
         CaseResolve_morethanoneWeek
         
      # CxAcqYrMnth = format(as.Date(data$CxAcqDate, format="%Y-%m-%d"),"%y-%b"),
      
      # CxAcqYr = format(as.Date(data$CxAcqDate, format="%Y-%m-%d"),"%y"),
      
      # LatestCloudBillYr = format(as.Date(data$LastCloudBill, format="%Y-%m-%d %H:%M:%S"),"%y")
      
)


data2 %>% count(CxType,LastCloudBillCategory)

glimpse(data2)

########################################
########## Build Prediction Model #####
########################################

## define training set
churntrain <- 
  select(
    data,
    CxType,
    HasChurned,
    TrainingCompleted,
    LastCloudBill,
    LicensePlan,
    LicenseType,
    contains("CaseResolve"),
    LastCheckinFilled,
    LastCloudBillCategory,
    LastCheckinCategory,
    DaysSinceCloudBill,
    DaysSinceCheckin
    )

data %>% count(HasChurned)
data %>% count(TrainingCompleted)
data %>% count(LicensePlan)
data %>% count(LicenseType)
data %>% count(CaseResolve_Immediate)
summary(churntrain$LastCloudBill)
summary(churntrain$LastCheckinFilled)
glimpse(churntrain)

#summary(churntrain$LastCheckin)
#table(is.na(churntrain$LastCheckin),churntrain$CxType)

##assign zero as value for LastCheckin data missing

##############
## Model 1 ##
##############

##create logistic model
  mod1 <- glm(HasChurned ~ 
                LastCloudBillCategory + 
                as.factor(TrainingCompleted),
              family="binomial",
              churntrain)

  summary(mod1)

##evaluate model predictions
  predict1 <- predict(mod1,type="response")
  
  summary(predict1)  
  
##build ROC curve
  ROCRpred1 <- prediction(predict1,data$HasChurned)

##model AUC
  auc_ROCR1 <- performance(ROCRpred1, measure = "auc")
  auc_ROCR1 <- auc_ROCR1@y.values[[1]]
  auc_ROCR1  
  
##ROC for Positives
  ROCRperf1 <- performance(ROCRpred1,"tpr","fpr")
  plot(ROCRperf1,print.cutoffs.at=seq(0,1,0.1),text.adj = c(-0.2,1.7))

  
##final chosen threshold - goal is to minimize False Negatives (Cx has churned but we predict they did not)
  predict1_output <-
    as.factor(ifelse(predict1 > 0.5,
                     "CHURN",
                     "Customer"
    ))

##Assess accuracy of threshold within model
  confusionMatrix(predict1_output,data$CxType)


  data <- data %>% mutate(ChurnPrediction_mod1 = predict1_output)
  data <- data %>% mutate(CxTypeMatch = ifelse((ChurnPrediction_mod1 == CxType),"Match","Not Match"))
  
  glimpse(data)
  
  data %>% count(CxTypeMatch)
  
  data.output <- 
    select(data,ID, AccountName,CxType,CxTypeMatch,ChurnPrediction_mod1,LastCloudBill,TrainingCompleted,LastCloudBillCategory,LastCheckinCategory) 
  # %>% 
  #   filter(CxTypeMatch=="Not Match")
  
  glimpse(data.output)
  view(data.output)
  
  data.output %>% filter(ID ==3143) %>% view()
  
  data.output %>% count(ChurnPrediction_mod1,LastCloudBillCategory)
  data.output %>% count(CxType,LastCloudBillCategory)
  
  data.output %>% count(ChurnPrediction_mod1,LastCheckinCategory)

##############
## Model 2 ##
##############    

##create logistic model  
  mod2 <- glm(HasChurned ~ 
                LastCloudBillCategory + 
                as.factor(TrainingCompleted) +
                LastCheckinCategory,
              family="binomial",
              churntrain)
  summary(mod2)  
  

##evaluate model predictions
predict2 <- predict(mod2,type="response")
summary(predict2)

##build ROC curve
ROCRpred2 <- prediction(predict2,data$HasChurned)

##model AUC
auc_ROCR2 <- performance(ROCRpred2, measure = "auc")
auc_ROCR2 <- auc_ROCR2@y.values[[1]]
auc_ROCR2

##ROC for Positives
  ROCRperf2 <- performance(ROCRpred2,"tpr","fpr")
  plot(ROCRperf2,print.cutoffs.at=seq(0,1,0.5),text.adj = c(-0.2,1.7))

##final chosen threshold - goal is to minimize False Negatives (Cx has churned but we predict they did not)
  predict2_output <-
    as.factor(ifelse(predict2 > 0.5,
                     "CHURN",
                     "Customer"
    ))

##Assess accuracy of threshold within model
  confusionMatrix(predict2_output,data$CxType)
  
##############
## Model 3 ##
##############    

##create logistic model  
mod3 <- glm(HasChurned ~ 
              LastCloudBill + 
              as.factor(TrainingCompleted) +
              LastCheckinFilled +
              CaseResolve_Immediate +
              `CaseResolve_<24hrs` +
              `CaseResolve_<1wk` +
              `CaseResolve_>1wk`,
            family="binomial",
            churntrain)

summary(mod3)



##evaluate model predictions
predict3 <- predict(mod3,type="response")
summary(predict3)

##build ROC curve
ROCRpred3 <- prediction(predict3,data$HasChurned)

##model AUC
auc_ROCR3 <- performance(ROCRpred3, measure = "auc")
auc_ROCR3 <- auc_ROCR3@y.values[[1]]
auc_ROCR3

##ROC for Positives
ROCRperf3 <- performance(ROCRpred3,"tpr","fpr")
plot(ROCRperf3,print.cutoffs.at=seq(0,1,0.05),text.adj = c(-0.2,1.7))

##final chosen threshold - goal is to minimize False Negatives (Cx has churned but we predict they did not)
predict3_output <-
  as.factor(ifelse(predict3 > 0.09,
                   "CHURN",
                   "Customer"
  ))

##Assess accuracy of threshold within model
confusionMatrix(predict3_output,data$CxType)

data <- data %>% mutate(ChurnPrediction_mod3 = predict3_output)
data <- data %>% mutate(CxTypeMatch = ifelse((ChurnPrediction_mod3 == CxType),"Match","Not Match"))

glimpse(data)

data %>% count(CxTypeMatch)

data.output <- 
  select(data,ID, AccountName,CxType,CxTypeMatch,ChurnPrediction_mod3,LastCloudBill,TrainingCompleted,LastCheckinFilled,contains("CaseResolve")) 
# %>% 
#   filter(CxTypeMatch=="Not Match")

glimpse(data.output)
view(data.output)

data.output %>% filter(ID ==3143) %>% view()



##############
## Model 4 ##
##############  

##create random forest model  
mod4 <- train(HasChurned ~ 
                LastCloudBill + 
                TrainingCompleted +
                CaseResolve_Immediate +
                `CaseResolve_<24hrs` +
                `CaseResolve_<1wk` +
                `CaseResolve_>1wk`,
              tuneLength = 3,
              data = churntrain,
              method = "ranger",
              trControl = trainControl(
                method = "cv",
                number = 5,
                verboseIter = TRUE
              )
)
plot(mod4)
print(mod4)

data <- data %>% mutate(ChurnPrediction_mod4 = predict(mod4))
data <- data %>% mutate(CxTypeMatch = ifelse((ChurnPrediction_mod4 == HasChurned),"Match","Not Match"))

glimpse(data)

data %>% count(CxTypeMatch)

data.output <- 
  select(data,ID, AccountName,CxType,CxTypeMatch,ChurnPrediction_mod4,LastCloudBill,TrainingCompleted,LastCheckinFilled,contains("CaseResolve")) 
# %>% 
#   filter(CxTypeMatch=="Not Match")

glimpse(data.output)
view(data.output)

data.output %>% filter(ID ==3143) %>% view()


##################################
## Model 5 - tree (full data) ##
##################################

glimpse(data)

data$Country <- as.factor(data$Country)
data$VenueType <- as.factor(data$VenueType)
data$LicensePlan <- as.factor(data$LicensePlan)
data$LicenseType <- as.factor(data$LicenseType)

churntrain5 <- 
  data %>% select(
    CxType,
    Country,
    VenueType,
    TrainingCompleted,
    LicensePlan,
    LicenseType,
    DaysSinceCloudBill,
    DaysSinceCheckin,
    contains("Resolve")
  )

churntrain5 <- churntrain5 %>% 
  rename(
      CaseResolve_oneDay = `CaseResolve_<24hrs`,
      CaseResolve_lessthanoneWeek = `CaseResolve_<1wk`,
      CaseResolve_morethanoneWeek = `CaseResolve_>1wk`
  )

# churntrain5 %>% count(Country) %>% arrange(desc(n))
# churntrain5 <- churntrain5 %>%  #dummy var added to see if Tree would split.  It did.
#   mutate(p7dSales = case_when(
#     CxType=="CHURN" ~ 1000,
#     TRUE ~ 10000
#       ))

mod5 <- tree(
  CxType ~ 
    DaysSinceCloudBill +
    DaysSinceCheckin +
    TrainingCompleted +
    # VenueType + #added variables, but tree did not create new branches
    # LicensePlan +
    # LicenseType +
    CaseResolve_Immediate +
    CaseResolve_oneDay +
    CaseResolve_lessthanoneWeek +
    CaseResolve_morethanoneWeek,
    # Country, #108 levels.  Model errored, need <= 32 levels.
    data = churntrain5
)

plot(mod5)
text(mod5, pretty = 0)

summary(mod5)
mod5

pred5 <- predict(mod5, type="class")
confusionMatrix(pred5,data$CxType) ##10,006 rows in original data but only 9866 rows in final output

pred5
glimpse(churntrain5)

##########################################################################################
## Model 5b - tree - remove Venues with > 14 days since last Bill upload & marked as Cx ##
##########################################################################################

glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)

churntrain5b <- 
  data2 %>% select(
    CxType,
    Country,
    #VenueType,
    TrainingCompleted,
    #LicensePlan,
    #LicenseType,
   DaysSinceCloudBill,
   DaysSinceCheckin,
    contains("Resolve")
  )

churntrain5b <- churntrain5b %>% 
  rename(
    CaseResolve_oneDay = `CaseResolve_<24hrs`,
    CaseResolve_lessthanoneWeek = `CaseResolve_<1wk`,
    CaseResolve_morethanoneWeek = `CaseResolve_>1wk`
  )

mod5b <- tree(
  CxType ~ 
   DaysSinceCloudBill +
   DaysSinceCheckin +
    TrainingCompleted +
    # VenueType + #added variables, but tree did not create new branches
    #LicensePlan +
    #LicenseType +
    CaseResolve_Immediate +
    CaseResolve_oneDay +
    CaseResolve_lessthanoneWeek +
    CaseResolve_morethanoneWeek,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain5b
)

plot(mod5b)
text(mod5b, pretty = 0)

summary(mod5b)
mod5b

pred5b <- predict(mod5b, type="class")
confusionMatrix(pred5b,data2$CxType)

################################################
## Model 5c - tree - remove last cloud bill date ##
################################################

glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)

churntrain5c <- 
  data2 %>% select(
    CxType,
    Country,
    #VenueType,
    TrainingCompleted,
    #LicensePlan,
    #LicenseType,
    #    DaysSinceCloudBill,
    #    DaysSinceCheckin,
    contains("Resolve")
  )

churntrain5c <- churntrain5c %>% 
  rename(
    CaseResolve_oneDay = `CaseResolve_<24hrs`,
    CaseResolve_lessthanoneWeek = `CaseResolve_<1wk`,
    CaseResolve_morethanoneWeek = `CaseResolve_>1wk`
  )

mod5c <- tree(
  CxType ~ 
    #    DaysSinceCloudBill +
    #    DaysSinceCheckin +
    TrainingCompleted +
    # VenueType + #added variables, but tree did not create new branches
    #LicensePlan +
    #LicenseType +
    CaseResolve_Immediate +
    CaseResolve_oneDay +
    CaseResolve_lessthanoneWeek +
    CaseResolve_morethanoneWeek,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain5b
)

plot(mod5c)
text(mod5c, pretty = 0)

summary(mod5c)
mod5c

pred5c <- predict(mod5c, type="class")
confusionMatrix(pred5c,data2$CxType)

########################################################
## Model 6 - tree - full data (incl. overdue invoices) ##
########################################################

glimpse(data)

data$Country <- as.factor(data$Country)
data$VenueType <- as.factor(data$VenueType)
data$LicensePlan <- as.factor(data$LicensePlan)
data$LicenseType <- as.factor(data$LicenseType)

churntrain6 <- 
  data %>% select(
    CxType,
    Country,
    VenueType,
    TrainingCompleted,
    LicensePlan,
    LicenseType,
    DaysSinceCloudBill,
    DaysSinceCheckin,
    contains("Resolve"),
    Num_overdue_invoices
  )

churntrain6 <- churntrain6 %>% 
  rename(
    CaseResolve_oneDay = `CaseResolve_<24hrs`,
    CaseResolve_lessthanoneWeek = `CaseResolve_<1wk`,
    CaseResolve_morethanoneWeek = `CaseResolve_>1wk`
  )

mod6 <- tree(
  CxType ~ 
    DaysSinceCloudBill +
    DaysSinceCheckin +
    TrainingCompleted +
    # VenueType + #added variables, but tree did not create new branches
    # LicensePlan +
    # LicenseType +
    CaseResolve_Immediate +
    CaseResolve_oneDay +
    CaseResolve_lessthanoneWeek +
    CaseResolve_morethanoneWeek +
    Num_overdue_invoices,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain6
)

plot(mod6)
text(mod6, pretty = 0)

summary(mod6)
mod6

pred6 <- predict(mod6, type="class")
confusionMatrix(pred6,data$CxType) ##10,006 rows in original data but only 9866 rows in final output

pred6
glimpse(churntrain6)

##########################################################################################
# Model 6b - tree 
   # removed Venues with > 14 days since last Bill upload & marked as Cx
   # also includes overdue bills
##########################################################################################

data <- data %>% mutate(overdueinvoices = 
                  case_when(
                    Num_overdue_invoices == 0 ~ "Zero",
                    Num_overdue_invoices > 0 ~ "Non-Zero"
                    ))

invoicecheck <- data %>% count(CxType,overdueinvoices)
data %>% group_by(CxType,overdueinvoices) %>% summarise(sum(Num_overdue_invoices))


glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)

churntrain6b <- 
  data2 %>% select(
    CxType,
    Country,
    #VenueType,
    TrainingCompleted,
    #LicensePlan,
    #LicenseType,
    DaysSinceCloudBill,
    DaysSinceCheckin,
    contains("Resolve"),
    Num_overdue_invoices
  )

churntrain6b <- churntrain6b %>% 
  rename(
    CaseResolve_oneDay = `CaseResolve_<24hrs`,
    CaseResolve_lessthanoneWeek = `CaseResolve_<1wk`,
    CaseResolve_morethanoneWeek = `CaseResolve_>1wk`
  )

mod6b <- tree(
  CxType ~ 
    DaysSinceCloudBill +
    DaysSinceCheckin +
    TrainingCompleted +
    # VenueType + #added variables, but tree did not create new branches
    #LicensePlan +
    #LicenseType +
    CaseResolve_Immediate +
    CaseResolve_oneDay +
    CaseResolve_lessthanoneWeek +
    CaseResolve_morethanoneWeek,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain6b
)

plot(mod6b)
text(mod6b, pretty = 0)

summary(mod6b)
mod6b

pred6b <- predict(mod6b, type="class")
confusionMatrix(pred6b,data2$CxType)

############################
## Model 6 - Random Forest##
############################

churntrain6 <- 
  data %>% select(
    CxType,
    Country,
    VenueType,
    TrainingCompleted,
    LicensePlan,
    LicenseType,
    DaysSinceCloudBill,
    DaysSinceCheckin,
    contains("CaseResolve")
  )

churntrain6 <- churntrain6 %>% filter(is.na(LicenseType)==FALSE)

# churntrain6 %>% count(Country) %>% arrange(desc(n))
# churntrain6 <- churntrain6 %>%  #dummy var added to see if Tree would split.  It did.
#   mutate(p7dSales = case_when(
#     CxType=="CHURN" ~ 1000,
#     TRUE ~ 10000
#       ))

mod6 <- 
  randomForest(
    CxType ~ 
      DaysSinceCloudBill +
      DaysSinceCheckin +
      TrainingCompleted +
      # VenueType +
      LicensePlan +
      LicenseType +
      CaseResolve_Immediate +
      `CaseResolve_<24hrs` +
      `CaseResolve_<1wk` +
      `CaseResolve_>1wk`,
      # Country,
    data = churntrain6
)

# plot(mod6)
mod6
varImpPlot(mod6)


##########################################################################################
# Model 7 - tree 
### excludes venues with > 14 days since last Cloud bill that are marked as Cx
##########################################################################################

glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)

churntrain7 <- 
  data2 %>% select(
    CxType,
    Country,
    # VenueType, #contains NAs, cannot use for model
    TrainingCompleted,
    # LicensePlan, #contains NAs, cannot use for model
    # LicenseType, #contains NAs, cannot use for model
    #DaysSinceCloudBill, #removed to evaluate model that can predict early stage Churn
    DaysSinceCheckin,
    contains("Resolve"),
    Num_overdue_invoices,
    CasesTotal
  )

glimpse(churntrain7)

mod7 <- tree(
  CxType ~ 
    #DaysSinceCloudBill + #removed to evaluate model that can predict early stage Churn
    #DaysSinceCheckin +
    TrainingCompleted +
    # VenueType + #contains NAs, cannot use for model
    # LicensePlan + #contains NAs, cannot use for model
    # LicenseType + #contains NAs, cannot use for model
    CaseResolve_Immediate +
    CaseResolve_oneDay +
    CaseResolve_lessthanoneWeek +
    CaseResolve_morethanoneWeek +
    CasesTotal +
    Num_overdue_invoices,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain7
)
plot(mod7)
text(mod7, pretty = 0)

summary(mod7)
mod7

pred7 <- predict(mod7, type="class")
confusionMatrix(pred7,data2$CxType)

data2$ChurnPrediction <- pred7
glimpse(data2)
data2 <- data2 %>% mutate(CxTypeMatch = ifelse((ChurnPrediction == CxType),"Match","Not Match"))

data.output <- 
  select(data2,ID, AccountName, CxType,LastCloudBill,ChurnPrediction,CxTypeMatch,CasesTotal,Num_overdue_invoices) %>% 
  filter(CxTypeMatch=="Not Match")

view(data.output)

data2 %>% group_by(CxType) %>% summarise(avg=mean(CasesTotal))

data2 %>% 
  filter(CxType=="CHURN") %>% count(LatestCloudBillYr) %>% mutate(`% Total` = round(n / sum(n)*100,1))


##########################################################################################
# Model 7a - tree 
### excludes venues with > 14 days since last Cloud bill that are marked as Cx
### includes Sales data 7d, 14d, 30d, 60d, & 90d prior to Churn
##########################################################################################

glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)

churntrain7a <- 
  data2 %>% select(
    ID,
    AccountName,
    CxType,
    Country,
    VenueType, #contains NAs, cannot use for model
    TrainingCompleted,
    # LicensePlan, #contains NAs, cannot use for model
    # LicenseType, #contains NAs, cannot use for model
    #DaysSinceCloudBill, #removed to evaluate model that can predict early stage Churn
    DaysSinceCheckin,
    contains("Resolve"),
    Num_overdue_invoices,
    CasesTotal,
    P7d_90d_ratio,
    P14d_90d_ratio,
    P30d_90d_ratio,
    P60d_90d_ratio
  )

churntrain7a <- na.omit(churntrain7a)

glimpse(churntrain7a)

mod7a <- tree(
  CxType ~ 
    # DaysSinceCloudBill + #removed to evaluate model that can predict early stage Churn
    #DaysSinceCheckin +
    TrainingCompleted +
    VenueType + #contains NAs, cannot use for model
    # LicensePlan + #contains NAs, cannot use for model
    # LicenseType + #contains NAs, cannot use for model
    # CaseResolve_Immediate +
    # CaseResolve_oneDay +
    # CaseResolve_lessthanoneWeek +
    # CaseResolve_morethanoneWeek +
    # CasesTotal +
    Num_overdue_invoices +
    P7d_90d_ratio,
    # P14d_90d_ratio +
    # P30d_90d_ratio +
    # P60d_90d_ratio,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain7a
)
plot(mod7a)
text(mod7a, pretty = 0)

summary(mod7a)
mod7a

pred7a <- predict(mod7a, type="class")
confusionMatrix(pred7a,churntrain7a$CxType)

churntrain7a$ChurnPrediction <- pred7a
glimpse(churntrain7a)
data_output_raw <- 
  churntrain7a %>% 
  mutate(CxTypeMatch = ifelse((ChurnPrediction == CxType),"Match","Not Match"))

data_output_not_match <- data_output_raw %>% filter(CxTypeMatch=="Not Match")
view(data_output_not_match)
write.csv(data_output_not_match, file = "MyData.csv")

## validate odd data in tree branches
data_output_validation_1 <- 
  data_output_raw %>% 
    filter(P14d_90d_ratio>0.8) %>% 
    filter(P30d_90d_ratio>1) %>% 
    filter(P7d_90d_ratio>4.42) %>% 
    filter(P60d_90d_ratio<1.6)
          
view(data_output_validation_1)
write.csv(data_output_validation_1, file = "MyData.csv")

##########################################################################################
# Model 8 - tree 
### excludes venues with > 14 days since last Cloud bill that are marked as Cx
### includes Sales data 7d, 14d, 30d, 60d, & 90d prior to Churn
### added Col forto identify voluntary vs. involuntary Churn
### goal is to predict Involuntary Churn now
##########################################################################################

glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)
data2$Involuntary_Churn <- as.factor(data2$Involuntary_Churn)

churntrain8 <- 
  data2 %>% select(
    ID,
    AccountName,
    CxType,
    Country,
    VenueType, #contains NAs, cannot use for model
    TrainingCompleted,
    # LicensePlan, #contains NAs, cannot use for model
    # LicenseType, #contains NAs, cannot use for model
    #DaysSinceCloudBill, #removed to evaluate model that can predict early stage Churn
    DaysSinceCheckin,
    contains("Resolve"),
    Num_overdue_invoices,
    CasesTotal,
    P7d_90d_ratio,
    P14d_90d_ratio,
    P30d_90d_ratio,
    P60d_90d_ratio,
    Involuntary_Churn
  )

churntrain8 <- na.omit(churntrain8)

glimpse(churntrain8)

mod8 <- tree(
  Involuntary_Churn ~ 
    # DaysSinceCloudBill + #removed to evaluate model that can predict early stage Churn
    #DaysSinceCheckin +
    TrainingCompleted +
    VenueType + #contains NAs, cannot use for model
    # LicensePlan + #contains NAs, cannot use for model
    # LicenseType + #contains NAs, cannot use for model
    # CaseResolve_Immediate +
    # CaseResolve_oneDay +
    # CaseResolve_lessthanoneWeek +
    # CaseResolve_morethanoneWeek +
    # CasesTotal +
    Num_overdue_invoices +
    P7d_90d_ratio +
    P14d_90d_ratio +
    P30d_90d_ratio +
    P60d_90d_ratio,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain8
)
plot(mod8)
text(mod8, pretty = 0)

summary(mod8)
mod8

pred8 <- predict(mod8, type="class")
confusionMatrix(pred8,churntrain8$Involuntary_Churn)

churntrain8$ChurnPrediction <- pred8
glimpse(churntrain8)
data_output_raw <- 
  churntrain8 %>% 
  mutate(CxTypeMatch = ifelse((ChurnPrediction == Involuntary_Churn),"Match","Not Match"))

data_output_not_match <- data_output_raw %>% filter(CxTypeMatch=="Not Match")
view(data_output_not_match)
write.csv(data_output_not_match, file = "MyData.csv")

## validate odd data in tree branches
data_output_validation_1 <- 
  data_output_raw %>% 
  filter(P14d_90d_ratio>0.8) %>% 
  filter(P30d_90d_ratio>1) %>% 
  filter(P7d_90d_ratio>4.42) %>% 
  filter(P60d_90d_ratio<1.6)

view(data_output_validation_1)
write.csv(data_output_validation_1, file = "MyData.csv")


##########################################################################################
# Model 8a - tree 
### excludes venues with > 14 days since last Cloud bill that are marked as Cx
### includes Sales data 7d, 14d, 30d, 60d, & 90d prior to Churn
### added Col forto identify voluntary vs. involuntary Churn
### goal is to predict Involuntary Churn now
### remove venues with Last Cloud Bill date < Jan 1, 2018
##########################################################################################

glimpse(data2)

data2$Country <- as.factor(data2$Country)
data2$VenueType <- as.factor(data2$VenueType)
data2$LicensePlan <- as.factor(data2$LicensePlan)
data2$LicenseType <- as.factor(data2$LicenseType)
data2$Involuntary_Churn <- as.factor(data2$Involuntary_Churn)

data3 <- data2 %>% filter(LastCloudBill>"2018-01-01")
glimpse(data3)
data3 %>% count(LatestCloudBillYrMnth)
data3 %>% count(CxType,Involuntary_Churn)

churntrain8a <- 
  data3 %>% select(
    ID,
    AccountName,
    CxType,
    Country,
    VenueType, #contains NAs, cannot use for model
    TrainingCompleted,
    # LicensePlan, #contains NAs, cannot use for model
    # LicenseType, #contains NAs, cannot use for model
    #DaysSinceCloudBill, #removed to evaluate model that can predict early stage Churn
    # DaysSinceCheckin,
    contains("Resolve"),
    Num_overdue_invoices,
    CasesTotal,
    P7d_90d_ratio,
    P14d_90d_ratio,
    P30d_90d_ratio,
    P60d_90d_ratio,
    Involuntary_Churn
  )

churntrain8a <- na.omit(churntrain8a)

glimpse(churntrain8a)

mod8a <- tree(
  Involuntary_Churn ~ 
    # DaysSinceCloudBill + #removed to evaluate model that can predict early stage Churn
    #DaysSinceCheckin +
    TrainingCompleted +
    VenueType + #contains NAs, cannot use for model
    # LicensePlan + #contains NAs, cannot use for model
    # LicenseType + #contains NAs, cannot use for model
    # CaseResolve_Immediate +
    # CaseResolve_oneDay +
    # CaseResolve_lessthanoneWeek +
    # CaseResolve_morethanoneWeek +
    # CasesTotal +
    Num_overdue_invoices +
    P7d_90d_ratio +
    P14d_90d_ratio +
    P30d_90d_ratio +
    P60d_90d_ratio,
  # Country, #108 levels.  Model errored, need <= 32 levels.
  data = churntrain8a
)
plot(mod8a)
text(mod8a, pretty = 0)

summary(mod8a)
mod8a

pred8a <- predict(mod8a, type="class")
confusionMatrix(pred8a,churntrain8a$Involuntary_Churn)


churntrain8a$ChurnPrediction <- pred8a
glimpse(churntrain8a)
data_output_raw <- 
  churntrain8a %>% 
  mutate(CxTypeMatch = ifelse((ChurnPrediction == Involuntary_Churn),"Match","Not Match"))

data_output_not_match <- data_output_raw %>% filter(CxTypeMatch=="Not Match")
view(data_output_not_match)
write.csv(data_output_not_match, file = "MyData.csv")

data3_InvoluntaryChurn <- 
  data3 %>% filter(Involuntary_Churn=="Yes") %>% select(contains("_90d_ratio")) 

data3_VoluntaryChurn <-
  data3 %>% filter(Involuntary_Churn=="No",CxType=="CHURN") %>% select(contains("_90d_ratio")) 

data3_Not_InvoluntaryChurn <-
  data3 %>% filter(Involuntary_Churn=="No") %>% select(contains("_90d_ratio")) 

##############################
#Model 8a - data validation
##############################

 
data3_validation <-
  data3 %>% 
    mutate(CxType_final = case_when(
        CxType == "Customer" ~ "Customer",
        CxType == "CHURN" & Involuntary_Churn =="Yes" ~ "Involuntary Churn",
        CxType == "CHURN" & Involuntary_Churn =="No" ~ "Voluntary Churn"
    ))

data3_validation <-
  data3_validation %>% filter(CxAcqDate < max(CxAcqDate - as.difftime(90,unit="days")))

ggplot(data3_validation,aes(CxAcqDate,P14d_90d_ratio,color=CxType_final)) +
  geom_point(alpha=0.5) +
  ylim(-1,5)
  
data3 %>% 
  select(
      P7d_90d_ratio,
      P14d_90d_ratio,
      P30d_90d_ratio,
      P60d_90d_ratio,
      Involuntary_Churn
      ) %>% pairs()

############################
#### View Model Output ####
############################

#ifelse(((predict3 > 0.09)==TRUE),"CHURN","Customer")

data <- data %>% mutate(ChurnPrediction = ifelse(((predict3 > 0.09)==TRUE),"CHURN","Customer"))
data <- data %>% mutate(CxTypeMatch = ifelse((ChurnPrediction == CxType),"Match","Not Match"))
data.output <- 
  select(data,ID, AccountName, CxType,TrainingCompleted,LastCloudBill,ChurnPrediction,CxTypeMatch,contains("CaseResolve"), LastCheckinFilled) %>% 
    filter(CxTypeMatch=="Not Match")

data.output %>% count(CxType,ChurnPrediction)

view(data.output)
 


