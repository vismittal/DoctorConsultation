install.packages('caret')
install.packages('corrplot')   # used for making correlation plot
install.packages('xgboost')    # used for building XGBoost model
install.packages('cowplot')    # used for combining multiple plots 
install.packages('stringr')
install.packages('DescTools')
install.packages('sqldf')

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


#### Qualification
## No of qualifications ###
for (i in 1:7948) {
  b <- str_count(as.character(DoctorData$Qualification[i]), ",")
  DoctorData$cntQual[i] <- b+1 
}

View(DoctorData)

##### Analyse Qualifications ###
b <- 0
c <- 0
for (i in 1:7948) {
  b <- str_count(as.character(DoctorData$Qualification[i]), ",")
  if (as.numeric(b) > as.numeric(c)) {
    c <- b
    d <- i
    qual <- as.character(DoctorData$Qualification[i])
  }
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



for (i in 1:7948) {
  b <- str_count(as.character(DoctorData$Qualification[i]), ",")
 
    for (j in 1:b){
        
    
    }
  
  
  }
}