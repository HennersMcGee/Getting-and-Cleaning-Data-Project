## ---------------------------
##
## Script name: run_analysis
##
## Purpose of script: Collect, work with, and clean a dataset.
##
## Author: Henry Letton
##
## Date Created: 2019-06-20
##
## ---------------------------
##
## Notes:
##   Create a dataset defined in the Getting and Cleaning Data coursera assignment.
##
## ---------------------------

# 0 Read in the data sets

location <- "./data/UCI HAR Dataset/"
x_train <- read.delim(paste(location,"train/x_train.txt",sep=""), header=FALSE, sep="",stringsAsFactors = FALSE)
y_train <- read.delim(paste(location,"train/y_train.txt",sep=""), header=FALSE, sep="",stringsAsFactors = FALSE)
x_test <- read.delim(paste(location,"test/x_test.txt",sep=""), header=FALSE, sep="",stringsAsFactors = FALSE)
y_test <- read.delim(paste(location,"test/y_test.txt",sep=""), header=FALSE, sep="",stringsAsFactors = FALSE)
features <- read.delim(paste(location,"features.txt",sep=""), header=FALSE, sep="",stringsAsFactors = FALSE)

# 1 Merge the training and the test sets to create one data set.

x <- rbind(x_train,x_test)
y <- rbind(y_train,y_test)

names(y) <- c("ActivityNumber")
for (i in 1:length(features[,2])){
    names(x)[i] <- features[i,2]
}

FullData <- cbind(y,x)

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

FullData <- FullData[grepl("mean",names(FullData)) | grepl("std",names(FullData)) | grepl("ActivityNumber",names(FullData))]

# 3 Uses descriptive activity names to name the activities in the data set

#Create lookup data with the mapping from number to activity name
lookup <- data.frame(c(1,2,3,4,5,6),
                     c("Walking","Walking Upstairs","Walking Downstairs","Sitting","Standing","Laying"))
names(lookup) <- c("number","ActivityName")
#Merge onto main data and delete now un-needed activity number column
FullData <- merge(lookup, FullData, by.x="number", by.y ="ActivityNumber")
FullData$number <- NULL

# 4 Appropriately label the data set with descriptive variable names.

## All variables already have appropriate names that aren't too long as to be unweildy.

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#install.packages("dplyr")
library(dplyr)
SumData <- FullData %>% group_by(ActivityName)
SumData <- summarise_if(SumData, is.numeric, mean)

# 5b Output data as .txt file

write.table(SumData, file="./Activity_Dataset_Summarised.txt", row.name=FALSE)
