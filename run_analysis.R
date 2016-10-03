##Load required packages
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(plyr)

##Create data directory, download and upzip dataset
  dir.create("Project")
  setwd("Project")
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile = "Dataset.zip")
  unzip(zipfile = "Dataset.zip")

##Read in the data, features and activities
  xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
  ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
  ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
  subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  features <- read.table("./UCI HAR Dataset/features.txt")
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
  
##Assign column names
  colnames(xtrain) <- features[,2]
  colnames(ytrain) <- "activityID"
  colnames(subjectTrain) <- "subjectID"
  
  colnames(xtest) <- features[,2]
  colnames(ytest) <- "activityID"
  colnames(subjectTest) <- "subjectID"
  colnames(activities) <- c('activityID', 'activityType')
  
##Merge training and test sets into one data set
  mergeTrain <- cbind(ytrain, subjectTrain, xtrain)
  mergeTest <- cbind(ytest, subjectTest, xtest)
  dataTable <- rbind(mergeTrain, mergeTest)
  
##Extract measurements on the mean and standard deviation for each measurement
  ##Read column names
  columnNames <- colnames(dataTable)
  
  ##Set-up a vector to keep IDs, means and standard deviations
  subFeatures <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
  
  ##Subset data by selected features and check
  selectFeatures <- c(as.character(subFeatures), "subjectID", "activityID")
  Data <- subset(dataTable, select = selectFeatures)
  str(Data)
  
  ##Name activities and check
  subActivityNames <- merge(Data, activities, by='activityID', all.x = TRUE)
  str(subActivityNames)
  
  ##Apply descriptive variable names, e.g. replace t with time
  names(Data) <- gsub("std()", "SD", names(Data))
  names(Data) <- gsub("mean()", "MEAN", names(Data))
  names(Data) <- gsub("^t", "time", names(Data))
  names(Data) <- gsub("^f", "frequency", names(Data))
  names(Data) <- gsub("Acc", "Accelerometer", names(Data))
  names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
  names(Data) <- gsub("Mag", "Magnitude", names(Data))
  names(Data) <- gsub("BOdyBody", "Body", names(Data))
  
  ##Create a tidy data setwith the average of each variable for each activity
  ##and each subject
  secondTidySet <- aggregate(. ~subjectID + activityID, Data, mean)
  secondTidySet <- secondTidySet[order(secondTidySet$subjectID, secondTidySet$activityID),]
  write.table(secondTidySet, file="averageData.txt", row.names = FALSE)
  
  