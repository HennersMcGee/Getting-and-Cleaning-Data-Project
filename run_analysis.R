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

#Define function to read in a named file from either the test or train folder
#Note: dowload zip file has been extracted into a "data" folder within directory
readInFiles <- function(file,location){
    locat <- paste("./data/UCI HAR Dataset/",location,"/",file,"_",location,".txt",sep="")
    data <- read.delim(locat, header=FALSE, sep="",stringsAsFactors = FALSE)
}
#Define a list of files to be extracted
DataList <- c("X","y")
#Extract files in list for both test and train
for (i in DataList) {
    for (j in c("train","test")) {
        assign(paste(i,"_",j,sep=""), readInFiles(i,j))
    }
}

# 1 Merge the training and the test sets to create one data set.

#Loop through the dataset list combining train with test
for (i in DataList) {
    assign(i, rbind(get(paste(i,"_train",sep="")),get(paste(i,"_test",sep=""))))
}

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

#Create empty dataset the same length with 3 columns ready for y and mean/sd of x data.
Act_SD_Mean <- X[,1:3]*0
colnames(Act_SD_Mean) <- c("ActivityNumber","sd","mean")
#Assign the 3 columns with relevant data
for (i in 1:length(X[,1])) {
    Act_SD_Mean$ActivityNumber[i] <- y[i,]
    Act_SD_Mean$sd[i] <- sd(X[i,],na.rm=TRUE)
    Act_SD_Mean$mean[i] <- mean(as.numeric(X[i,],na.rm=TRUE),na.rm=TRUE)
}

# 3 Uses descriptive activity names to name the activities in the data set

#Create lookup data with the mapping from number to activity name
lookup <- data.frame(c(1,2,3,4,5,6),
                     c("Walking","Walking Upstairs","Walking Downstairs","Sitting","Standing","Laying"))
names(lookup) <- c("number","name")
#Merge onto main data and delete now un-needed activity number column
Act_SD_Mean <- merge(Act_SD_Mean, lookup, by.x="ActivityNumber", by.y ="number")
Act_SD_Mean$ActivityNumber <- NULL

# 4 Appropriately label the data set with descriptive variable names.

#Tidy the order and naming of the dataset
Act_SD_Mean <- Act_SD_Mean[,c(3,2,1)]
names(Act_SD_Mean) <- c("Activity","Mean","SD")

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Create dataset with mean of mean data, split by activity
Activity_Dataset_Summarised1 <- as.data.frame(tapply(Act_SD_Mean$Mean, Act_SD_Mean$Activity, mean))
#Create dataset with mean of SD data, split by activity
Activity_Dataset_Summarised2 <- as.data.frame(tapply(Act_SD_Mean$SD, Act_SD_Mean$Activity, mean))
#Bind the two together
Activity_Dataset_Summarised <- cbind(Activity_Dataset_Summarised1,Activity_Dataset_Summarised2)
#Change row names to coulmn
library(data.table)
setDT(Activity_Dataset_Summarised, keep.rownames = TRUE)[]
#Name columns appropriately
names(Activity_Dataset_Summarised) <- c("Activity","Mean","SD")

# 5b Output data as .txt file

write.table(Activity_Dataset_Summarised, file="./data/Activity_Dataset_Summarised.txt", row.name=FALSE)
