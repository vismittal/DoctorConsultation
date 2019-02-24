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
View(strSkin)
strUK <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%,UK%'")
View(strUK)
strUSA <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%, USA%'")
View(strUSA)
strGer <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%,%Germany%'")
View(strGer)
strSGP <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%,%singapore%'")
View(strSGP)

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

strDerma <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Dermatology ,Venereology%'")

StrMS <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%MS,%'")

View(StrMS)

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

write.csv(x = QualificationUnq, file = 'QualificationUniq40.csv')

QualLevel <- read.csv(file = 'QualificationWithLevel.csv')
View(QualLevel)

### Doctor to highest level of qualification

DoctorData$qualLevel <- NA
DoctorData$Seq1 <- NA

#View(doctor_qual_df)
#View(QualLevel)

k <- 0

for (k in 1:7948) {
  
  print (concatenate('k == ', k))
  DocNo <- k
  print (concatenate('DocNo == ', DocNo))

  sqlStrQual <- concatenate('select * 
                            from doctor_qual_df,  QualLevel
                            on trim(QualLevel.Qualification) = trim(doctor_qual_df.qualSplit)
                            where doctor_qual_df.doctor_uid_required = ', DocNo ,' order by doctor_qual_df.doctor_uid_required,
                            QualLevel.Seq1 desc')
  
  print (concatenate('sqlStrQual == ', sqlStrQual))
  
  a <- sqldf(sqlStrQual)
  
  #View(a)  
  #print(a)
  
  DoctorData$qualLevel [DocNo] <- as.character(a[1,5])
  DoctorData$Seq1 [DocNo] <- a[1,6]
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

write.csv(DataDataWithCost, 'DataDataWithCost.csv')

############################### Split the data back into train and test

DoctorTrainWithFea <- sqldf("select * from DataDataWithCost
                            where DataDataWithCost.Fees != 'NA'")

View(DoctorTrainWithFea)

DoctorTestWithFea <- sqldf("select * from DataDataWithCost
                            where DataDataWithCost.Fees IS NULL")
View(DoctorTestWithFea)

############################# Data cleaning Train data only ####

unique(DoctorTrainWithFea$Seq1)

DoctorTrainWithFea$se

View(sqldf("select * from DoctorTrainWithFea
                            where DoctorTrainWithFea.Seq1 = '0'"))
