install.packages('caret')
install.packages('corrplot')   # used for making correlation plot
install.packages('xgboost')    # used for building XGBoost model
install.packages('cowplot')    # used for combining multiple plots 
install.packages('stringr')
install.packages('DescTools')
install.packages('sqldf')
install.packages('stringi')
install.packages('gdata')

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
for (i in 1:7948) {
  a <- StrPos( x = as.character(DoctorData$Place[i]), pattern =',')
  DoctorData$Locality[i] <- substr(DoctorData$Place[i], 1, a-1)
  DoctorData$city[i] <- substr(DoctorData$Place[i], a+2, str_length(DoctorData$Place[i]))
}


unique(DoctorData$Locality)
unique(DoctorData$city)


#### Qualification

View(DoctorData)

########################### Analyse Qualifications ###
### Remove unwanted ',' from Dermatology related qualifications

strDerma <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Dermatology , Venereology%'")

View (strDerma)

StrVener <- sqldf("select DIstinct Qualification from DoctorData where Qualification like '%Venereology & Leprosy%'")

write.csv(x = unique(trim(StrVener)), 'Venerelogy.csv')

write.csv(strDerma, file = 'Derma.csv')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Dermatology , Venereology', 'Dermatology Venereology')
DoctorData$Qualification <- str_replace(DoctorData$Qualification, 'Dermatology, Venereology', 'Dermatology Venereology')

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

###
# b <- 0
# c <- 0
# for (i in 1:7948) {
#   b <- str_count(as.character(DoctorData$Qualification[i]), ",")
# 
#     if (as.numeric(b) > as.numeric(c)) {
#     c <- b
#     d <- i
#     qual <- as.character(DoctorData$Qualification[i])
#   }
# }

DoctorData$DocNo <- NA
View(DoctorData)

for (i in 1:7948) {
  
  DoctorData$DocNo[i] <- i
  
}

DocQual <- read.csv(file = 'DocQual.csv')

View(DocQual)

hist(DoctorData$cntQual)
boxplot(x = DoctorData$cntQual)

qualSplit <- unlist(strsplit(as.character(DoctorData$Qualification), ','))
qualSplit

doctor_uid_required=rep(DoctorData$DocNo, DoctorData$cntQual)

doctor_qual_df=data.frame(doctor_uid_required, qualSplit)
doctor_qual_df

QualificationUnq = unique(trim(doctor_qual_df$qualSplit))
View(QualificationUnq)

write.csv(x = QualificationUnq, file = 'QualificationUniq.csv')

# sqldf("select Distinct  qualSplit from doctor_qual_df where qualSplit like '%Venereology & Leprosy%'")
# View(doctor_qual_df)
# 
# write.csv(x = unique(trim(doctor_qual_df$qualSplit)),file = 'Unique_Qual.csv')
# 
# 
# 
# DoctorData$
#   
#   sqldf("select Qualification from DoctorData where Qualification like '%Venereology & Leprosy%'")
