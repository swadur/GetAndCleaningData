## Week 4 - Tidy Data Assigment
## Suleman Wadur
## Class: Get and Cleaning Data

## Load needed libraries
library(dplyr)


## Set working directory
workdir <- "C:/Move 4/Coursera/DataScience/Course3-GettingData/Week4/assignment"
setwd(workdir)

## directory check and data download
if(!file.exists("./data")) {
   dir.create("./data")
 }

 fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(fileUrl, destfile="./data/Dataset.zip")

## Unzip data and save in same directory
unzip(zipfile="./data/Dataset.zip", exdir="./data")


## Load the data from the zipped file.
trainSubject <- tbl_df(read.table("./data/UCI HAR Dataset/train/subject_train.txt"))
trainActivity <- tbl_df(read.table("./data/UCI HAR Dataset/train/y_train.txt"))
trainData <- tbl_df(read.table("./data/UCI HAR Dataset/train/X_train.txt"))


testSubject <- tbl_df(read.table("./data/UCI HAR Dataset/test/subject_test.txt"))
testActivity <- tbl_df(read.table("./data/UCI HAR Dataset/test/y_test.txt"))
testData <- tbl_df(read.table("./data/UCI HAR Dataset/test/X_test.txt"))

features <- tbl_df(read.table("./data/UCI HAR Dataset/features.txt"))
activityLabels<- tbl_df(read.table("./data/UCI HAR Dataset/activity_labels.txt"))

subjects <- rbind(trainSubject, testSubject)
activities <- rbind(trainActivity, testActivity)

## Add labels to loaded data
names(subjects) <- c("Subjects")
names(activities) <- c("Activities")

names(features) <- c("FeaturesKey", "FeaturesName")
names(activityLabels) <- c("ActivityKey", "ActivityName")

## Merge train and test data along with activities and subjects
mergedData <- rbind(trainData, testData)
names(mergedData) <- features$FeaturesName
TidyData <- cbind(subjects, activities, mergedData)



######
## Get all mean or std columns and pull data related to those columns
mean_std_features <- grep("mean|std|Subjects|Activities", colnames(TidyData), value=TRUE)
mean_std_data <- TidyData[,mean_std_features]


######
## Descriptive activities names.
TidyData <- merge(TidyData, activityLabels, by.x='Activities', by.y='ActivityKey') #using merge to add the descriptive activity as a column
TidyData <- subset(TidyData, select = c(2,564, 3:563)) #reorders the columns and drops the activity key
TidyData <- tbl_df(arrange(TidyData, Subjects, ActivityName)) #orders the new data by Subject and Activity Name


######
## Descriptive labels for variable names using gsub to substitute global occurrences.
names(TidyData) <- gsub("^t", "time", names(TidyData))
names(TidyData) <- gsub("^f", "frequency", names(TidyData))
names(TidyData) <- gsub("mad", "mid", names(TidyData))
names(TidyData) <- gsub("Acc", "Acceleration", names(TidyData))
names(TidyData) <- gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData) <- gsub("Mag", "Magnitude", names(TidyData))
names(TidyData) <- gsub("BodyBody", "Body", names(TidyData))
names(TidyData) <- gsub("\\(|\\)", "", names(TidyData))

######
## Calculate the mean of all data grouping by Subject and Activity
##colMeans(TidyData[sapply(TidyData, is.numeric)])
##aggregate( TidyData[,3:563], TidyData[,1:2], FUN = mean )
meanSubjectActivity <- aggregate( . ~ Subjects + ActivityName, data = TidyData, FUN = mean )

write.table(meanSubjectActivity, "./TidyData.txt", row.name=FALSE)
