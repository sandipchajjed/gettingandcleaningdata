## Getting and Cleaning Data : Course Project
# Created by Sandip Chajjed
# June 16, 2016

## Objective: You should create one R script called run_analysis.R that does the following.
# 1	Merges the training and the test sets to create one data set.
# 2	Extracts only the measurements on the mean and standard deviation for each measurement.
# 3	Uses descriptive activity names to name the activities in the data set
# 4	Appropriately labels the data set with descriptive variable names.
# 5	From the data set in step 4, creates a second, independent tidy data set with the average 
#      of each variable for each activity and each subject. 
	
# One of the most exciting areas in all of data science right now is wearable 
# computing - see for example this article . Companies like Fitbit, Nike, and 
# Jawbone Up are racing to develop the #most advanced algorithms to attract new users. 
# The data linked to from the course website represent data collected from the accelerometers
# from the Samsung Galaxy S smartphone. A full #description is available at the site where the data was obtained:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# Here are the data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

library(plyr)
library(dplyr)
library(data.table)
library(tidyr)


## Create data folder for this project and put the files in data folder. 
if(!file.exists("./data")){dir.create("./data")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="./data/Dataset.zip",method='curl')

##Unzip the dataset file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

path_DS <- file.path("./data", "UCI HAR Dataset")
files <- list.files(path_DS, recursive= TRUE)
files
## Read data from files into the variables 

# Read the Activity files
dataActivityTest <- read.table(file.path(path_DS,"test","y_test.txt"),header=FALSE)
dataActivityTrain <- read.table(file.path(path_DS,"train", "y_train.txt"),header=FALSE)

# Read the Subject files
dataSubjectTest <- read.table(file.path(path_DS,"test","subject_test.txt"), header = FALSE )
dataSubjectTrain <- read.table(file.path(path_DS,"train","subject_train.txt"), header =FALSE)

# Read the Features data  files
dataFeaturesTest <- read.table(file.path(path_DS,"test","X_test.txt"), header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_DS,"train","X_train.txt"), header = FALSE)


## Objective 1: Merge the Training and the Test sets to create one data set
dataActivity <- rbind(dataActivityTrain,dataActivityTest)
dataSubject <- rbind(dataSubjectTrain,dataSubjectTest)
dataFeatures <- rbind(dataFeaturesTrain,dataFeaturesTest)

names(dataSubject)<-c("subject")
names(dataActivity)<-c("activity")
dataFeaturesnames <- read.table(file.path(path_DS,"features.txt"), header=FALSE)
names(dataFeatures) <- dataFeaturesnames$V2

dataComb <- cbind(dataSubject,dataActivity)
AllData <- cbind(dataFeatures,dataComb)


## Objective 2: Extracts only the measurements on the mean and standard deviation for each measurement.
subdataFeaturesNames<-dataFeaturesnames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesnames$V2)]
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(AllData,select=selectedNames)
str(Data)


## Objective 3: Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table(file.path(path_DS,"activity_labels.txt"),header =FALSE)
Data$activity <-as.character(Data$activity)
for ( i in 1:6){
	Data$activity[Data$activity==i]<-as.character(activityLabels[i,2])
}

Data$activity <- as.factor(Data$activity)
head(Data$activity,30)


## Objective 4: Appropriately labels the data set with descriptive variable names.

names(Data) <- gsub("^t","time",names(Data))
names(Data) <- gsub("^f","frequency",names(Data))
names(Data) <-gsub("Acc","Accelerometer",names(Data))
names(Data) <-gsub("Gyro","Gyroscope",names(Data))
names(Data) <-gsub("Mag","Magnitude",names(Data))
names(Data) <-gsub("BodyBody","Body",names(Data))
head(names(Data),30)

## Objective 5: From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.


Data2 <- aggregate(. ~subject + activity,Data, mean)
Data2 <- Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file="tidydata.txt",row.name=FALSE)
head(Data2,3)











