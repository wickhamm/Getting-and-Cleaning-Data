########################################################################################################################
#                                  Data Cleaning and Analysis - Final Project Script
#                                     Maeve Wickham, December 11 2018
########################################################################################################################

# Instructions: 
#   You should create one R script called run_analysis.R that does the following.
#       1. Merges the training and the test sets to create one data set.
#       2. Extracts only the measurements on the mean and standard deviation for each measurement.
#       3. Uses descriptive activity names to name the activities in the data set
#       4. Appropriately labels the data set with descriptive variable names.
#       5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#           for each activity and each subject.

# WORK setwd("E:/Maeve/Dropbox/Data Science/3. Getting and Cleaning Data/Project")
# HOME setwd("C:/Users/Maeve/Dropbox/Data Science/3. Getting and Cleaning Data/Project")
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

###### 1, MERGING THE TRAINING AND TEST SETS 

### 1. Getting and unzipping the Data 
if( !dir.exists("UCI HAR Dataset") ) {
  if( !file.exists("UCI HAR Dataset.zip") ) {
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, "./Data", method="curl")}
  unzip("./Data") #data were unzipped to "./UCI HAR Dataset" folder)
  }

# Load the labels
activitylabel <- read.table("./UCI HAR Dataset/activity_labels.txt")
feature <- read.table("./UCI HAR Dataset/features.txt")

# Load the test sets
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Load the training sets
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

### 2. Prepping the data for merging

## Rename the variables
colnames(activitylabel) <- c("activity", "activitytype")

colnames(x_test) <- feature[,2]
#tidy names up for future variable extraction
cleanNames <- as.character(feature$V2)
cleanNames <- make.names(cleanNames, unique=TRUE)
names(x_test) <- cleanNames

colnames(y_test) <- "activity"
colnames(subject_test) <- "id"

colnames(x_train) <- feature[,2]
#tidy names up for future variable extraction
cleanNames <- as.character(feature$V2)
cleanNames <- make.names(cleanNames, unique=TRUE)
names(x_train) <- cleanNames

colnames(y_train) <- "activity"
colnames(subject_train) <- "id"

## Binding the multiple files together
test <- cbind(y_test, x_test, subject_test)
train <- cbind(y_train, x_train, subject_train)
merged <- rbind(test, train)

##### 2. EXTRACT MEAN AND SD FROM MERGED DATASET

meanStd <- merged %>%
  select(id, activity, contains(".mean."), contains(".std."))

##### 3. USING DESCRIPTIVE ACTIVITY LABELS
meanStd <- merge(meanStd, activitylabel, by="activity", all.x=TRUE)

##### 4. CLEAN THE VARIABLE NAMES 

# rename the variables using gsub (first two columns are id/activity)
names(meanStd)[-c(1:2)] <- names(meanStd)[-c(1:2)] %>% 
  gsub("^t", "time", .) %>% gsub("^f", "freq", .) %>% gsub("Acc", "Accelerometer",.) %>% gsub("Gyro", "Gyroscope",.) %>%
  gsub("Mag", "Magnitude", .) %>% gsub("BodyBody", "body",.) %>% gsub(".mean.", "mean", .) %>% 
  gsub(".std.", "std",.) %>% gsub("[.][.]X","x", .) %>% gsub("[.][.]Y","y", .) %>% gsub("[.][.]Z","z", .) %>% 
  gsub("std[.]", "std", .) %>% gsub("mean[.]", "mean", .) %>% tolower

##### 5. TIDY, SECONDARY DATASET WITH THE MEAN VALUES FOR EACH ACTIVITY AND SUBJECT
hartidy <- aggregate(. ~id + activitytype, meanStd, mean)
hartidy <- hartidy[order(hartidy$id, hartidy$activity),]

write.table(hartidy, "hartidy.txt", row.name=FALSE)
