install.packages('caret')
install.packages('corrplot')   # used for making correlation plot
install.packages('xgboost')    # used for building XGBoost model
install.packages('cowplot')    # used for combining multiple plots 
install.packages('stringr')
install.packages('DescTools')
install.packages('sqldf')
install.packages('stringi')
install.packages('gdata')
install.packages('ngram')      # used for concatinating strings
install.packages('pivottabler')
install.packages('dummies')
install.packages('xgboost')
install.packages('readr')
install.packages('car')
install.packages('gbm')
library(xgboost)
library(readr)
library(car)
library(gbm)

library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots 
library(stringr)
library(DescTools)
library(sqldf)
library(stringi)
library(gdata)
library(ngram)
library(pivottabler)
library(dummies)


setwd("D:/Vishal/IIMBAI/AI ML Training/Doctor Consultation")

DoctorTrain <- read.csv(file = 'Final_Train.csv')
head(DoctorTrain)
names(DoctorTrain)
str(DoctorTrain)
dim(DoctorTrain)

DoctorTest <- read.csv(file = 'Final_Test.csv')
head(DoctorTest)
names(DoctorTest)
str(DoctorTest)
dim(DoctorTest)

DoctorTest$Fees <- NA
dim(DoctorTest)

head(DoctorTest)

DoctorData = rbind(DoctorTrain, DoctorTest)
dim(DoctorData)
str(DoctorData)

########### Data cleansing #####
##### Experince
data.frame(DoctorData)

DoctorData$Experience1 <- str_replace(DoctorData$Experience, " years experience", " ")

head(DoctorData)
View(DoctorData)


##### Place

DoctorData$Place <- str_replace(DoctorData$Place, "Dwarka, Sector 5, Delhi", "Dwarka Sector 5, Delhi")

for (i in 1:7948) {
  a <- StrPos( x = as.character(DoctorData$Place[i]), pattern =',')
  DoctorData$Locality[i] <- substr(DoctorData$Place[i], 1, a-1)
  DoctorData$city[i] <- substr(DoctorData$Place[i], a+2, str_length(DoctorData$Place[i]))
}

a <- NA

unique(DoctorData$Locality)
unique(DoctorData$city)

## sqldf("select * from DoctorData where Place like '%,%Sector 5%'")


#### Qualification

View(DoctorData)

write.csv(x = DoctorData, "DoctorData.csv")

########################### Analyse Qualifications ###
### Remove unwanted ',' from Dermatology related qualifications

strDerma <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Dermatology , Venereology%'")
StrVener <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Venereology & Leprosy%'")
strSkin <- sqldf("select DIstinct Qualification from DoctorData where Qualification like 'MD - Skin%,%'")
#View(strSkin)
strUK <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%,UK%'")
#View(strUK)
strUSA <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%, USA%'")
#View(strUSA)
strGer <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%,%Germany%'")
#View(strGer)
strSGP <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%,%singapore%'")
#View(strSGP)

#View (strDerma)
#write.csv(x = unique(trim(StrVener)), 'Venerelogy.csv')
#write.csv(strDerma, file = 'Derma.csv')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Dermatology , Venereology', 'Dermatology Venereology')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Dermatology, Venereology', 'Dermatology Venereology')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'MD - Skin,VD & Leprosy', 'MD - Skin VD & Leprosy')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, ',UK', ' UK')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, ', USA', ' USA')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Medicine, USA', 'Medicine USA')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, ',germany', ' germany')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Medicine, Singapore', 'Medicine Singapore')

#strDerma <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Dermatology ,Venereology%'")
#StrMS <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%MS,%'")
#View(StrMS)

StrDiabetes <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Diabetes%'")
StrDiabetes <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Diabetes & Metabolism%'")
StrDiabetes <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Diploma in Diabetes%'")

View(StrDiabetes)

write.csv(StrDiabetes, file = 'Diabetes.csv')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Endocrinology, Diabetes, Metabolism',
                                        'Endocrinology & Diabetes & Metabolism')

DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Endocrinology, Diabetes & Metabolism',
                                        'Endocrinology & Diabetes & Metabolism')

DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Endocrinology, Diabetes & Metabolism',
                                        'Endocrinology & Diabetes & Metabolism')


## No of qualifications ###
for (i in 1:7948) {
  b <- str_count(as.character(DoctorData$Qualification[i]), ",")
  DoctorData$cntQual[i] <- b+1 
}

DoctorData$DocNo <- NA
View(DoctorData)

for (i in 1:7948) {
  DoctorData$DocNo[i] <- i
}

DocQual <- read.csv(file = 'DocQual.csv')

View(DocQual)

hist(DoctorData$cntQual)
boxplot(x = DoctorData$cntQual)

qualSplit <- trim(unlist(strsplit(as.character(DoctorData$Qualification), ',')))
qualSplit

doctor_uid_required=rep(DoctorData$DocNo, DoctorData$cntQual)
doctor_qual_df=data.frame(doctor_uid_required, qualSplit)
View(doctor_qual_df)

#write.csv(doctor_qual_df, 'DocQual.csv')

QualificationUnq = unique(trim(doctor_qual_df$qualSplit))
View(QualificationUnq)

write.csv(x = QualificationUnq, file = 'QualificationUnique.csv')

QualLevel <- read.csv(file = 'QualificationWithLevel01.csv')
View(QualLevel)

### Doctor to highest level of qualification

DoctorData$qualLevel <- NA
DoctorData$Seq1 <- NA

#View(doctor_qual_df)
#View(QualLevel)

k <- 0

for (k in 1:7948) {
  
  #print (concatenate('k == ', k))
  DocNo <- k
  print (concatenate('DocNo == ', DocNo))

  sqlStrQual <- concatenate('select * 
                            from doctor_qual_df,  QualLevel
                            on trim(QualLevel.Qualification) = trim(doctor_qual_df.qualSplit)
                            where doctor_qual_df.doctor_uid_required = ', DocNo ,' order by doctor_qual_df.doctor_uid_required,
                            QualLevel.Seq1 desc')
  #print (concatenate('sqlStrQual == ', sqlStrQual))
  
  a <- sqldf(sqlStrQual)
  
  #View(a)  
  #print(a)
  DoctorData$qualLevel [DocNo] <- as.character(a[1,4])
  DoctorData$Seq1 [DocNo] <- a[1,5]
  DoctorData[DocNo, ]
  a <- NA
}

View (DoctorData)
write.csv(x = DoctorData, 'DoctorData.csv')

View(sqldf("select * from DoctorData
                            where DoctorData.Seq1 = '0'"))

View(sqldf("select * from DoctorData
                            where DoctorData.Seq1 IS NULL"))

unique(DoctorData$Seq1)


##### Features for location cost of living

CostCity <- read.csv('CostOfLivingIndexMapping.csv')
unique(DoctorData$city)

DataDataWithCost <- sqldf('select DoctorData.*, CostCity.CostOfLivingIndex, CostCity.LocalPurchasingPowerIndex 
        from DoctorData left join CostCity
        on DoctorData.city = CostCity.CityName')

View(DataDataWithCost)

write.csv(DataDataWithCost, 'DocDataWithCost.csv')

############################### Split the data back into train and test

DoctorTrainWithFea <- sqldf("select * from DataDataWithCost
                            where DataDataWithCost.Fees != 'NA'")

View(DoctorTrainWithFea)

DoctorTestWithFea <- sqldf("select * from DataDataWithCost
                            where DataDataWithCost.Fees IS NULL")
View(DoctorTestWithFea)

############################# Data cleaning Train data only ####

unique(DoctorTrainWithFea$Seq1)

DoctorTrainWithFea$Qualification

View(sqldf("select * from DoctorTrainWithFea
                            where DoctorTrainWithFea.Seq1 = '0'"))

### Ignore the rows with degree as "Get inspired by remarkable stories of people like you"
#DoctorTrainWithFea01 <- sqldf("select * from DoctorTrainWithFea
#                            where DoctorTrainWithFea.Qualification 
#                              != 'Get inspired by remarkable stories of people like you'")


#View(sqldf("select * from DoctorTrainWithFea01
#                            where Seq1 = '0'"))

docpivot <- (sqldf("select * from DoctorTrainWithFea
                            where Profile = 'General Medicine'"))

qhpvt(docpivot, "Profile", "qualLevel", "n()")
### Most of the doctors on General Medicince are PG Degree, so map this one doctor to PG Degree

################ Create a matirx with relavant columns only 01

DoctorTrainWithFea02 <- (sqldf("select  Fees, Rating, Experience1, Profile,cntQual,
                                  Seq1,CostOfLivingIndex, LocalPurchasingPowerIndex
                                    from DoctorTrainWithFea"))

View(DoctorTrainWithFea02)

DoctorTrainWithFea02$Seq1 <- as.factor(DoctorTrainWithFea02$Seq1)

###### Make a corplot

# corrplot(DoctorTrainWithFea02)
# 
# cor_train = cor(DoctorTrainWithFea02,c("kendall"))
# 
# corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

############ Model Creation

linear_reg_mod = lm(Fees ~ Experience1 +
                           cntQual +
                           Seq1 + 
                           CostOfLivingIndex, 
                              data = DoctorTrainWithFea02)
summary(linear_reg_mod)

plot(linear_reg_mod$residuals)
hist(linear_reg_mod$residuals)
qqnorm(linear_reg_mod$residuals)
qqline(linear_reg_mod$residuals)

DoctorTestWithFea$Seq1 <- as.factor(DoctorTestWithFea$Seq1)

res <- predict(linear_reg_mod, newdata = DoctorTestWithFea)
View(res)

write.csv(res, "res.csv")


summary(res)

write.csv()

################ Create a matirx with relavant columns only 02


DoctorTrainWithFea03 <- (sqldf("select  Fees, Rating, Experience1, Profile,cntQual,
                                  Seq1,CostOfLivingIndex, LocalPurchasingPowerIndex
                                    from DoctorTrainWithFea"))

View(DoctorTrainWithFea)

DoctorTrainWithFea03$OnlinePres <- NA

# DoctorTrainWithFea03$OnlinePres <- with(DoctorTrainWithFea03, 
#                                        ifelse(is.na(trim(DoctorTrainWithFea03$Rating)), "N", "Y"))
# 
write.csv(DoctorTrainWithFea03, "DoctorTrainWithFea03.csv")
write.csv(DoctorTestWithFea, "DoctorTestWithFea03.csv")

DoctorTrainWithFea03 <- read.csv("DoctorTrainWithFea03.csv")
DoctorTestWithFea03 <- read.csv("DoctorTestWithFea03.csv")

DoctorTrainWithFea03.1 <- (sqldf("select  Fees, Experience1, Profile,cntQual,
                                    Seq1,CostOfLivingIndex, LocalPurchasingPowerIndex, OnlinePres
                                    from DoctorTrainWithFea03"))

DoctorTestWithFea03.1 <- (sqldf("select  Fees, Experience1, Profile,cntQual,
                                  Seq1,CostOfLivingIndex, LocalPurchasingPowerIndex, OnlinePres
                                  from DoctorTestWithFea03"))

str(DoctorTrainWithFea03.1)
str(DoctorTestWithFea03.1)
# for (i in 1: 5961) {
#  
#   if (is.null(trim(DoctorTrainWithFea03$Rating))) {
#     print (" Rating is Null")
#     DoctorTrainWithFea03$OnlinePres <- "N",csv
#   }
#   
#   else {
#     print (concatenate(" Rating is Not Null   ", DoctorTrainWithFea03$Rating, "end"))
#     DoctorTrainWithFea03$OnlinePres <- "Y"
#   }
# }
# 
# View (DoctorTrainWithFea03)

linear_reg_mod02 = lm(Fees ~ Experience1 +
                      cntQual +
                      Seq1 + 
                      CostOfLivingIndex + 
                      OnlinePres, 
                    data = DoctorTrainWithFea03)
summary(linear_reg_mod02)

plot(linear_reg_mod$residuals)
hist(linear_reg_mod$residuals)
qqnorm(linear_reg_mod$residuals)
qqline(linear_reg_mod$residuals)

res <- predict(linear_reg_mod02, newdata = DoctorTestWithFea03)
View(res)

write.csv(res, "res02.csv")

summary(res)

write.csv()


DoctorTrainWithFea031 <- dummy(DoctorTrainWithFea03$OnlinePres, sep = ".")

DoctorTrainWithFea032 <- dummy.data.frame(DoctorTrainWithFea03.1, sep = "." )
View(DoctorTrainWithFea032)

write.csv(x = DoctorTrainWithFea032, "DoctorTrainWithFea032.csv")

DoctorTrainWithFea032 <- read.csv("DoctorTrainWithFea032.csv")

str(DoctorTrainWithFea031)

DoctorTestWithFea032 <- dummy.data.frame(DoctorTestWithFea03.1, sep = "." )
View(DoctorTestWithFea032)

write.csv(x = DoctorTestWithFea032, "DoctorTestWithFea032.csv")

DoctorTestWithFea032 <- read.csv("DoctorTestWithFea032.csv")


#### Data Set
#### DoctorTrainWithFea032
#### DoctorTestWithFea032

linear_reg_mod03 = lm(Fees ~ Experience1 +
                        cntQual +
                        Seq1 + 
                        CostOfLivingIndex + 
                        OnlinePres_N +
                        OnlinePres_Y,
                        data = DoctorTrainWithFea032)
summary(linear_reg_mod03)


DoctorTestWithFea03.21 <- sqldf("Select Experience1, cntQual, Seq1, CostOfLivingIndex, OnlinePres.N, 
                              OnlinePres.Y from DoctorTestWithFea032")

                           #DoctorTestWithFea03.2

res <- predict(linear_reg_mod03, newdata = DoctorTestWithFea032)
View(res)

write.csv(res, "res3.csv")

###########################Model 4 ####################

DoctorAllWithFea4 <- read.csv("DoctorOverallWithFeaBase04.csv")
str(DoctorAllWithFea4)
unique(DoctorAllWithFea4$Fees)

View(DoctorAllWithFea4)

DoctorAllWithFea4$Cat_Rating_Feedback <- as.factor(DoctorAllWithFea4$Cat_Rating_Feedback)
DoctorAllWithFea4$Cat_Ayurveda <- as.factor(DoctorAllWithFea4$Cat_Ayurveda)  
DoctorAllWithFea4$Cat_Dentist              <- as.factor(DoctorAllWithFea4$Cat_Dentist)
DoctorAllWithFea4$Cat_Dermatologists       <- as.factor(DoctorAllWithFea4$Cat_Dermatologists)
DoctorAllWithFea4$Cat_ENT_Specialist       <- as.factor(DoctorAllWithFea4$Cat_ENT_Specialist)
DoctorAllWithFea4$Cat_General_Medicine     <- as.factor(DoctorAllWithFea4$Cat_General_Medicine)
DoctorAllWithFea4$Cat_Homeopath            <- as.factor(DoctorAllWithFea4$Cat_Homeopath)
DoctorAllWithFea4$Cat_Bangalore            <- as.factor(DoctorAllWithFea4$Cat_Bangalore)
DoctorAllWithFea4$Cat_Chennai              <- as.factor(DoctorAllWithFea4$Cat_Chennai)
DoctorAllWithFea4$Cat_Coimbatore           <- as.factor(DoctorAllWithFea4$Cat_Coimbatore)
DoctorAllWithFea4$Cat_Delhi                <- as.factor(DoctorAllWithFea4$Cat_Delhi)
DoctorAllWithFea4$Cat_Ernakulam            <- as.factor(DoctorAllWithFea4$Cat_Delhi)
DoctorAllWithFea4$Cat_Hyderabad            <- as.factor(DoctorAllWithFea4$Cat_Hyderabad)
DoctorAllWithFea4$Cat_Mumbai               <- as.factor(DoctorAllWithFea4$Cat_Mumbai)
DoctorAllWithFea4$Cat_NA                   <- as.factor(DoctorAllWithFea4$Cat_NA)
DoctorAllWithFea4$Cat_Thiruvananthapuram   <- as.factor(DoctorAllWithFea4$Cat_Thiruvananthapuram)
DoctorAllWithFea4$Cat_Seq_1                <- as.factor(DoctorAllWithFea4$Cat_Seq_1)
DoctorAllWithFea4$Cat_Seq_2                <- as.factor(DoctorAllWithFea4$Cat_Seq_2)
DoctorAllWithFea4$Cat_Seq_3                <- as.factor(DoctorAllWithFea4$Cat_Seq_3)
DoctorAllWithFea4$Cat_Seq_4                <- as.factor(DoctorAllWithFea4$Cat_Seq_4)
DoctorAllWithFea4$Cat_Seq_5                <- as.factor(DoctorAllWithFea4$Cat_Seq_5)
DoctorAllWithFea4$Cat_Seq_6                <- as.factor(DoctorAllWithFea4$Cat_Seq_6)
DoctorAllWithFea4$Cat_Seq_0                <- as.factor(DoctorAllWithFea4$Cat_Seq_0)
DoctorAllWithFea4$Rating <- as.numeric(DoctorAllWithFea4$Rating)
str(DoctorAllWithFea4)

View(DoctorAllWithFea4)

#### Split data into training and test

DoctorTrainWithFea4 <- sqldf("select Fees, Cat_Rating_Feedback,
                             Experience1,
                             cntQual,
                             CostOfLivingIndex,
                             Cat_Ayurveda,
                             Cat_Dentist,
                             Cat_Dermatologists,
                             Cat_ENT_Specialist,
                             Cat_General_Medicine,
                             Cat_Homeopath,
                             Cat_Bangalore,
                             Cat_Chennai, Cat_Coimbatore, Cat_Delhi, Cat_Ernakulam, 
                             Cat_Hyderabad, Cat_Mumbai, Cat_NA, Cat_Thiruvananthapuram,
                             Cat_Seq_1, Cat_Seq_2, Cat_Seq_3, Cat_Seq_4, Cat_Seq_5, Cat_Seq_6,
      
                              Competition
                                from DoctorAllWithFea4
                                where Fees is not NULL")

DoctorTestWithFea4 <- sqldf("select Cat_Rating_Feedback,
                             Experience1,
                             cntQual,
                             CostOfLivingIndex,
                             Cat_Ayurveda,
                             Cat_Dentist,
                             Cat_Dermatologists,
                             Cat_ENT_Specialist,
                             Cat_General_Medicine,
                             Cat_Homeopath,
                             Cat_Bangalore,
                             Cat_Chennai, Cat_Coimbatore, Cat_Delhi, Cat_Ernakulam, 
                             Cat_Hyderabad, Cat_Mumbai, Cat_NA, Cat_Thiruvananthapuram,
                             Cat_Seq_1, Cat_Seq_2, Cat_Seq_3, Cat_Seq_4, Cat_Seq_5, Cat_Seq_6,
                             Competition
                             from DoctorAllWithFea4
                             where Fees is NULL")

linear_reg_mod04 = lm(Fees ~ Experience1 +
                      cntQual +
                      CostOfLivingIndex +
                      Cat_Ayurveda +
                      Cat_Dentist +
                      Cat_Dermatologists +
                      Cat_ENT_Specialist +
                      Cat_General_Medicine +
                      Cat_Homeopath +
                      Cat_Bangalore +
                      Cat_Chennai + Cat_Coimbatore + Cat_Delhi + Cat_Ernakulam +
                      Cat_Hyderabad + Cat_Mumbai + Cat_Thiruvananthapuram + #Cat_NA + 
                      Cat_Seq_1 + Cat_Seq_2 + Cat_Seq_3 + Cat_Seq_4 +Cat_Seq_5 +Cat_Seq_6 +
                      Competition, 
                      data = DoctorTrainWithFea4)

summary(linear_reg_mod04)

res04 <- predict(linear_reg_mod04, newdata = DoctorTestWithFea4)
View(res04)

write.csv(res04, "res04.csv")

################ GBM ##############################
# for reproducibility
set.seed(123)

gbm.fit <- gbm(
  formula = Fees ~ .,
  distribution = "gaussian",
  data = DoctorTrainWithFea4,
  n.trees = 7000,
  interaction.depth = 4,
  shrinkage = 0.2,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

gbm.fit01 <- gbm(
  formula = Fees ~ .,
  distribution = "gaussian",
  data = DoctorTrainWithFea4,
  n.trees = 7000,
  interaction.depth = 4,
  shrinkage = 0.25,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


gbm.fit02 <- gbm(
  formula = Fees ~ .,
  distribution = "gaussian",
  data = DoctorTrainWithFea4,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.15,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

print(gbm.fit)
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")
# hyper_grid <- expand.grid(
#   shrinkage = c(.01, .1, .3),
#   interaction.depth = c(1, 3, 5),
#   n.minobsinnode = c(5, 10, 15),
#   bag.fraction = c(.65, .8, 1), 
#   optimal_trees = 0,               # a place to dump results
#   min_RMSE = 0                     # a place to dump results
# )
#nrow(hyper_grid)
summary(
  gbm.fit, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

pred <- predict(gbm.fit01, n.trees = gbm.fit$n.trees, DoctorTestWithFea4)
pred <- predict(gbm.fit02, n.trees = gbm.fit$n.trees, DoctorTestWithFea4)

View(pred)

write.csv(pred, "res05.csv")
write.csv(pred, "res06.csv")
write.csv(pred, "res07.csv")
write.csv(pred, "res08.csv")
write.csv(pred, "res09.csv")

gbm.tune <- gbm(
  formula = Fees ~ .,
  distribution = "gaussian",
  data = DoctorTrainWithFea4,
  n.trees = 5000,
  interaction.depth = hyper_grid$interaction.depth[i],
  shrinkage = hyper_grid$shrinkage[i],
  n.minobsinnode = hyper_grid$n.minobsinnode[i],
  bag.fraction = hyper_grid$bag.fraction[i],
  train.fraction = .75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

############## xgboost ####
param_list = list(
  objective = "reg:linear",
  eta=0.01,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)

dtrain = xgb.DMatrix(data = as.matrix(DoctorTrainWithFea4))

str(DoctorTrainWithFea4)

dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

########################################################################

DoctorTrainWithFea4 <- sqldf("select Fees, Cat_Rating_Feedback,
                             Experience1,
                             cntQual,
                             CostOfLivingIndex,
                             LocalPurchasingPowerIndex,
                             Cat_Ayurveda,
                             Cat_Dentist,
                             Cat_Dermatologists,
                             Cat_ENT_Specialist,
                             Cat_General_Medicine,
                             Cat_Homeopath,
                             Cat_Bangalore,
                             Cat_Chennai, Cat_Coimbatore, Cat_Delhi, Cat_Ernakulam, 
                             Cat_Hyderabad, Cat_Mumbai, Cat_NA, Cat_Thiruvananthapuram,
                             Cat_Seq_1, Cat_Seq_2, Cat_Seq_3, Cat_Seq_4, Cat_Seq_5, Cat_Seq_6,
                             
                             Competition
                             from DoctorAllWithFea4
                             where Fees is not NULL")

DoctorTestWithFea4 <- sqldf("select Cat_Rating_Feedback,
                            Experience1,
                            cntQual,
                            CostOfLivingIndex,
                             LocalPurchasingPowerIndex,
                            Cat_Ayurveda,
                            Cat_Dentist,
                            Cat_Dermatologists,
                            Cat_ENT_Specialist,
                            Cat_General_Medicine,
                            Cat_Homeopath,
                            Cat_Bangalore,
                            Cat_Chennai, Cat_Coimbatore, Cat_Delhi, Cat_Ernakulam, 
                            Cat_Hyderabad, Cat_Mumbai, Cat_NA, Cat_Thiruvananthapuram,
                            Cat_Seq_1, Cat_Seq_2, Cat_Seq_3, Cat_Seq_4, Cat_Seq_5, Cat_Seq_6,
                            Competition
                            from DoctorAllWithFea4
                            where Fees is NULL")

linear_reg_mod05 = lm(Fees ~ Experience1 +
                        cntQual +
                        CostOfLivingIndex +
                        LocalPurchasingPowerIndex +
                        Cat_Ayurveda +
                        Cat_Dentist +
                        Cat_Dermatologists +
                        Cat_ENT_Specialist +
                        Cat_General_Medicine +
                        Cat_Homeopath +
                        Cat_Bangalore +
                        Cat_Chennai + Cat_Coimbatore + Cat_Delhi + Cat_Ernakulam +
                        Cat_Hyderabad + Cat_Mumbai + Cat_Thiruvananthapuram + #Cat_NA + 
                        Cat_Seq_1 + Cat_Seq_2 + Cat_Seq_3 + Cat_Seq_4 +Cat_Seq_5 +Cat_Seq_6 +
                        Competition, 
                      data = DoctorTrainWithFea4)

summary(linear_reg_mod05)

res05 <- predict(linear_reg_mod05, newdata = DoctorTestWithFea4)
View(res05)

write.csv(res05, "res10.csv")

###############################################
gbm.fita <- gbm(
  formula = Fees ~ .,
  distribution = "gaussian",
  data = DoctorTrainWithFea4,
  n.trees = 7000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

print(gbm.fita)
sqrt(min(gbm.fita$cv.error))
gbm.perf(gbm.fita, method = "cv")

summary(
  gbm.fita, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

preda <- predict(gbm.fita, n.trees = gbm.fit$n.trees, DoctorTestWithFea4)

View(preda)
write.csv(preda, "res11.csv")
