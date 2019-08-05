ftj############import all the neccessary libraries##########
library(ggplot2)
library(caTools)
##################load the data##################################
general_data <- read.csv('general_data.csv',stringsAsFactors = FALSE)
employee_survey_data <-read.csv('employee_survey_data.csv',stringsAsFactors = FALSE)
manager_survey_data <- read.csv('manager_survey_data.csv',stringsAsFactors = FALSE)
in_time <- read.csv('in_time.csv',stringsAsFactors = FALSE)
out_time <- read.csv('out_time.csv',stringsAsFactors = FALSE)

#nrow(general_data)
#Info: all the data frames have  4410 no of rows

#merge the data frames
combined <- merge(x= general_data,y=employee_survey_data,by ="EmployeeID",all = TRUE)
combined <- merge(x= combined,y=manager_survey_data,by="EmployeeID",all = TRUE)

nrow(combined) #check row count after merge

combined$Attrition <- as.factor(combined$Attrition)
combined$BusinessTravel <- as.factor(combined$BusinessTravel)
combined$Department <-as.factor(combined$Department)
combined$Education <-as.factor(combined$Education)
combined$EducationField <-as.factor(combined$EducationField)
combined$Gender<-as.factor(combined$Gender)
combined$JobLevel <-as.factor(combined$JobLevel)
combined$JobRole <-as.factor(combined$JobRole)
combined$MaritalStatus <-as.factor(combined$MaritalStatus)
combined$StockOptionLevel <-as.factor(combined$StockOptionLevel)
combined$EnvironmentSatisfaction <-as.factor(combined$EnvironmentSatisfaction)
combined$JobSatisfaction <-as.factor(combined$JobSatisfaction)
combined$WorkLifeBalance <-as.factor(combined$WorkLifeBalance)
combined$JobInvolvement <-as.factor(combined$JobInvolvement)
combined$PerformanceRating <-as.factor(combined$PerformanceRating)
 
str(combined)
View(names(combined))


########################clean the data####################

sum( is.na(combined$EmployeeID))


#EmployeeCount
sum(is.na(combined$EmployeeCount)) 
sum((combined$EmployeeCount>1)) 
#not necessary can remove this column
combined <- combined[,-9]


 #Over18
 sum( is.na(combined$Over18))
 sum((combined$Over18!='Y'))
 #remove this column since adds no value
 combined = combined[,-15]
 
 
  #standard hours
 sum(is.na(combined$StandardHours!=8)) 
 combined = combined[,-16]

## TODO move all remove columns here 
############################################################
 #finding cloumsn with NA
 
 sum(is.na(combined))
 
 which(is.na(combined))
 
 
 colSums(is.na(combined))
 
 #NumCompaniesWorked,TotalWorkingYears(considered numberr of companies worked and years at a COMPANY.
                                        if No of comapnies in 1 or 0 then yearsAt campany else 11 as its the average)
 #NUmCompaniesWorked with Median value i.e 2
 
 combined$x_NumCompaniesWorked <- ifelse(is.na(combined$NumCompaniesWorked), median(combined$NumCompaniesWorked,na.rm=TRUE),
                                       combined$NumCompaniesWorked)
 
 combined$x_TotalWorkingYears <- ifelse(is.na(combined$TotalWorkingYears), ifelse(combined$NumCompaniesWorked == 0|combined$NumCompaniesWorked == 1,
                               combined$YearsAtCompany,ifelse(is.na(combined$TotalWorkingYears),11,combined$TotalWorkingYears))
                                      ,combined$TotalWorkingYears)
 
 #EmployeeID EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance 
 #0                      25                      20                      38 
 combined$x_wrklifebal <-ifelse(is.na(combined$WorkLifeBalance),median(combined$WorkLifeBalance,na.rm=TRUE),combined$WorkLifeBalance)
 combined$x_Job_Stf <-ifelse(is.na(combined$JobSatisfaction),median(combined$JobSatisfaction,na.rm=TRUE),combined$JobSatisfaction)
 combined$x_Env_Stf <-ifelse(is.na(combined$EnvironmentSatisfaction),median(combined$EnvironmentSatisfaction,na.rm=TRUE),combined$EnvironmentSatisfaction)
 

##############univariate analysis############################
 #NOTE: The graphs are commented out to remove the over head while re - running code all time
# #age
# mean(combined$Age)
# max(combined$Age)
# min(combined$Age)
# sum( is.na(combined$Age))
# ggplot(combined,aes(combined$Age)) +geom_histogram()
# 
# #age data is found to be consistent between 18 to 60 with an average of 36
# 
# #attrition
# 
# sum( is.na(combined$Attrition))
# ggplot(combined,aes(combined$Attrition)) +geom_bar(aes(fill=combined$Attrition))
# 
# #business travel
# sum( is.na(combined$BusinessTravel))
# ggplot(combined,aes(combined$BusinessTravel)) +geom_bar(aes(fill=combined$BusinessTravel))
# 
# #department
# sum( is.na(combined$Department))
# ggplot(combined,aes(combined$Department)) +geom_bar(aes(fill=combined$Department))
# 
# 
# #DistanceFromHome
# 
# mean(combined$DistanceFromHome)
# max(combined$DistanceFromHome)
# min(combined$DistanceFromHome)
# 
# sum( is.na(combined$DistanceFromHome))
# ggplot(combined,aes(combined$DistanceFromHome)) +geom_histogram(binwidth = 4)
# 
# 
# #Education
# sum(is.na(combined$Education)) 
# ggplot(combined,aes(combined$Education)) +geom_bar(aes(fill=combined$Education))+scale_fill_discrete(labels=c("Below college","College","Bachelors","Masters","Doctor"))
# #most of them are bachelor degree holders and masters 
# 
# #EducationField
# sum(is.na(combined$EducationField)) 
# ggplot(combined,aes(combined$EducationField)) +geom_bar(aes(fill=combined$EducationField))
# 
# 
# #Gender
#  sum(is.na(combined$Gender)) 
#  ggplot(combined,aes(combined$Gender)) +geom_bar(aes(fill=combined$Gender))
# 
#  #JobLevel
#  
#  sum(is.na(combined$JobLevel)) 
#  ggplot(combined,aes(combined$JobLevel)) +geom_bar(aes(fill=combined$JobLevel))
#  
#  #JobRole
#  
#  sum(is.na(combined$JobRole)) 
#  ggplot(combined,aes(combined$JobRole)) +geom_bar(aes(fill=combined$JobRole))
#  
#  #MaritalStatus
# 
#  sum(is.na(combined$MaritalStatus)) 
#  ggplot(combined,aes(combined$MaritalStatus)) +geom_bar(aes(fill=combined$MaritalStatus))
#  
# #quite a number of divorced people 
# 
#  #MonthlyIncome
#  mean(combined$MonthlyIncome)
#  max(combined$MonthlyIncome)
#  min(combined$MonthlyIncome)
#  
#  
#  sum( is.na(combined$MonthlyIncome))
#  ggplot(combined,aes(combined$MonthlyIncome)) +geom_histogram()
# 
#  #NumCompaniesWorked
#  sum( is.na(combined$NumCompaniesWorked))
#  ggplot(combined,aes(combined$NumCompaniesWorked)) +geom_histogram(binwidth = 1)
#  #TODO: remove na values. set to 0 ?
# 
#  
#  #PercentSalaryHike
#  mean(combined$PercentSalaryHike)
#  max(combined$PercentSalaryHike)
#  min(combined$PercentSalaryHike)
#  
#  #TODO get percentile info for percentage hike
#  sum( is.na(combined$PercentSalaryHike))
#  ggplot(combined,aes(combined$PercentSalaryHike)) +geom_histogram(binwidth = 1)
#  
#  #StockOptionLevel
#  str(combined)
#  sum(is.na(combined$StockOptionLevel)) 
#  ggplot(combined,aes(combined$StockOptionLevel)) +geom_bar(aes(fill=combined$StockOptionLevel))
#  
#  #TotalWorkingYears
# 
#  sum(is.na(combined$TotalWorkingYears)) 
#  #TODO see why total working years is na
#  ggplot(combined,aes(combined$TotalWorkingYears)) +geom_histogram()
#  
#  
#  #TrainingTimesLastYear
#  sum(is.na(combined$TrainingTimesLastYear)) 
#  ggplot(combined,aes(combined$TrainingTimesLastYear)) +geom_histogram(binwidth = 1)
#  
#  #YearsAtCompany
#  sum(is.na(combined$YearsAtCompany)) 
#  ggplot(combined,aes(combined$YearsAtCompany)) +geom_histogram()
#  
#  #YearsSinceLastPromotion
# 
#  sum(is.na(combined$YearsSinceLastPromotion)) 
#  ggplot(combined,aes(combined$YearsSinceLastPromotion)) +geom_histogram(binwidth = 1)
#  
#  #YearsWithCurrManager
#  sum(is.na(combined$YearsWithCurrManager)) 
#  ggplot(combined,aes(combined$YearsWithCurrManager)) +geom_histogram()
# 
#  
#  #EnvironmentSatisfaction
# 
#  sum(is.na(combined$EnvironmentSatisfaction)) 
#  ggplot(combined,aes(combined$EnvironmentSatisfaction)) +geom_bar(aes(fill=combined$EnvironmentSatisfaction))
#  #TODO remove na here and check for NA's
#  
#  #JobSatisfaction
# 
#  sum(is.na(combined$JobSatisfaction)) 
#  ggplot(combined,aes(combined$JobSatisfaction)) +geom_bar(aes(fill=combined$JobSatisfaction))
#  #TODO remove or check NA here
#  
#  #WorkLifeBalance
# 
#  sum(is.na(combined$WorkLifeBalance)) 
#  ggplot(combined,aes(combined$WorkLifeBalance)) +geom_bar(aes(fill=combined$WorkLifeBalance))
#  #TODO remove NA here 
#  
#  #JobInvolvement
# 
#  sum(is.na(combined$JobInvolvement)) 
#  ggplot(combined,aes(combined$JobInvolvement)) +geom_bar(aes(fill=combined$JobInvolvement))
#  
#  #PerformanceRating
#     
#  sum(is.na(combined$PerformanceRating)) 
#  ggplot(combined,aes(combined$PerformanceRating)) +geom_bar(aes(fill=combined$PerformanceRating))
 
 
 #TODO check if conversion of factors for ratings is required prior ?
 #because it might be required to convert again to numeric for glm function
 
 ############################################################
 
 #############bi variate analysis#############################

#TODO: correlation plot to identify which all variables for bi variate analysis
 

#hike vs manager and self rating
#years in company with attrition
 
 g <- ggplot(combined,aes(x=x_Job_Stf,..count..,fill=Attrition))+geom_bar(position = "stack")
 g + facet_wrap(~combined$JobRole)+labs(title="Job Satisfaction across Different Job Roles", x="Job Satisfaction")
 
 
 h <- ggplot(combined,aes(x=x_wrklifebal,..count..,fill=Attrition))+geom_bar(position = "dodge")
 h + facet_wrap(~combined$JobRole)+labs(title="Work-Life Balance With Different Job Roles", x="Work-Life balance")
 
 h + facet_wrap(~combined$Gender)+labs(title="Work-Life Balance in Gender", x="Work-Life balance")
 > ggsave("WorkLifeBal.PNG")
 
 e <- ggplot(combined,aes(x=MonthlyIncome,..count..,fill=Attrition))+geom_histogram(position="stack")
 > e + facet_wrap(~combined$JobRole)+labs(title="Monthly Income across Different Job Roles", x="Monthly Income")
 
 i <- ggplot(combined,aes(x=BusinessTravel,..count..,fill=Attrition))+geom_bar(position = "stack")
 > i + facet_wrap(~combined$JobRole)+labs(title="Business Travels for Different Job Roles", x="Business Travel")
 
 k <- ggplot(combined,aes(x=TotalWorkingYears,..count..,fill=Attrition))+geom_bar(position = "stack")
 > k + facet_wrap(~combined$Education)+labs(title="Education and Work Experience on Attrition", x="Working Experience")
 
 l <- ggplot(combined,aes(x=Education,..count..,fill=Attrition))+geom_bar(position = "dodge")
 > l + facet_wrap(~combined$JobRole)+labs(title="Education level and Job Role", x="Education Level")
  
 m <- ggplot(combined,aes(x=PercentSalaryHike,..count..,fill=Attrition))+geom_bar(position = "stack")
 m + facet_wrap(~combined$PerformanceRating)+labs(title="Hike per Rating Against Attrition", x="Performamce Rating")
 ggsave("Hike Per Rating.PNG")
 ############################################################
 
 
########derive from categorical variables and feature scaling#####################
 #TODO: create new variables based on in time and out time
 
 #derived variable
 
 #TODO: new variables either from binning or some other intution from data
 
 #data variables for all trabnsformations. Combined will remain as master data for future references

 data <-  combined[,-1] 
#remove employee id at last Just in case if it is used to identify records

#########feature scaling

#TODO is it right to do this?
data$Age<-scale(data$Age)

data$DistanceFromHome<-scale(data$DistanceFromHome)
data$JobLevel<-scale(data$JobLevel)
data$MonthlyIncome<-scale(data$MonthlyIncome)
data$NumCompaniesWorked<-scale(data$NumCompaniesWorked)
data$PercentSalaryHike<-scale(data$PercentSalaryHike)
data$TotalWorkingYears<-scale(data$TotalWorkingYears)
data$TrainingTimesLastYear<-scale(data$TrainingTimesLastYear)
data$YearsAtCompany<-scale(data$YearsAtCompany)
data$YearsSinceLastPromotion<-scale(data$YearsSinceLastPromotion)
data$YearsWithCurrManager<-scale(data$YearsWithCurrManager)


# categorical variables transform

data$Attrition <- ifelse(data$Attrition=="Yes",1,0)
data$Gender <- ifelse(data$Gender=="Male",1,0)
#TODO : why not this be a integer ?why is it factor?
data$PerformanceRating <- ifelse(data$PerformanceRating=="4",1,0)

str(data)
categorical_variables_index <- c(3,4,6,7,9,10,11,15,21,22,23)
data_numeric <- data[,-categorical_variables_index]
data_categorical <- data[,categorical_variables_index]
str(data_numeric)

#TODO: remove this. It's just for time being
data_categorical$EnvironmentSatisfaction <- ifelse(is.na(data_categorical$EnvironmentSatisfaction),1,data_categorical$EnvironmentSatisfaction)
data_categorical$JobSatisfaction <- ifelse(is.na(data_categorical$JobSatisfaction),1,data_categorical$JobSatisfaction)
data_categorical$WorkLifeBalance <- ifelse(is.na(data_categorical$WorkLifeBalance),3,data_categorical$WorkLifeBalance)
sum(is.na(data_categorical$EnvironmentSatisfaction))
sum(is.na(data_categorical$JobSatisfaction))
sum(is.na(data_categorical$WorkLifeBalance =="1"))
data_categorical$WorkLifeBalance
View(data_categorical)
abc <- sapply(data_categorical, function(x) data.frame(model.matrix(~x-1,data =data_categorical))[,-1])

View(abc)
dummies<- data.frame()
############################################################


##################model building##############################
# splitting the data between train and test
set.seed(100)

indices = sample.split(data$Attrition, SplitRatio = 0.7)
train = data[indices,]
test = data[!(indices),]

#intial model from step AIC function
#do vif and p-value iteratively to determine good model


############################################################



###################################model evaluation##########

# check parameters like Accuracy , sensitivity, specificity, Gain and lift charts , ks statistics etc
############################################################