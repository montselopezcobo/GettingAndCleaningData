#=============================================
# COURSE PROJECT: GETTING AND CLEANING DATA
#=============================================

# This script works with data collected from  the accelerometers of the
# Samsung Galaxy S smartphone as made available from the course website 
# and follows the next steps:
# 
# 1.	Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard 
#     deviation for each measurement. 
# 3.	Uses descriptive activity names to name the activities in the 
#     data set.
# 4.	Appropriately labels the data set with descriptive variable names. 
# 5.	From the data set in step 4, creates a second, independent tidy 
#     data set with the average of each variable for each activity and 
#     each subject.



##--- 0. Libraries
#
require(data.table)
require(dplyr)

##############
###
### STEP 1. Merge the training and test sets to create one dataset ----
###
##############################################################################

##--- 1. Download and unzip data
#
setwd("/home/NET1/lopemon/coursera/00_Assignments/03_GettingAndCleaningData/CourseProject")

if (!file.exists("Data")) {dir.create("Data")}
setwd("./Data")

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./samsung.zip", method = "curl")
dateDownloaded <- date()
unzip("samsung.zip", exdir="./")
setwd("./UCI HAR Dataset")

##--- 2. Loading labels of activities and features
labelActivity <- read.csv("./activity_labels.txt", 
                           header = FALSE, sep=" ", 
                           col.names = c("activityCode","activityName"))
str(labelActivity)

labelFeature <- read.csv("./features.txt", 
                           header = FALSE, sep=" ", 
                           col.names = c("featureCode","featureName"))
str(labelFeature)

##--- 3. Reading test and train sets and joining them together

# For each dataset (test and train), we first add information
# on subject and activity and then we label the dataset 
# (test or train) before merging them

#- Test dataset
X_test <- read.fwf("./test/X_test.txt", 
                  width = c(rep(16, 561)), sep="", 
                  col.names = labelFeature[,2], 
                  check.names = TRUE)
subject_test <- read.csv("./test/subject_test.txt", 
                         header = FALSE, 
                         col.names = "subject")
y_test <- read.csv("./test/y_test.txt", 
                        header = FALSE, 
                        col.names = "activity")
Xtest_all <- cbind(dataset="test", subject_test, y_test, X_test)
rm(list=c("X_test", "y_test", "subject_test"))

#- Train dataset
X_train <- read.fwf("./train/X_train.txt", 
                  width = c(rep(16, 561)), sep="", 
                  col.names = labelFeature[,2], 
                  check.names = TRUE)
subject_train <- read.csv("./train/subject_train.txt", 
                         header = FALSE, 
                         col.names = "subject")
y_train <- read.csv("./train/y_train.txt", 
                        header = FALSE, 
                        col.names = "activity")
Xtrain_all <- cbind(dataset="train", subject_train, y_train, X_train)
rm(list=c("X_train", "y_train", "subject_train"))

#- Joining Test and Train datasets

#- First I see if the names of variables in both datasets are equal
identical(names(Xtest_all), names(Xtrain_all))

Xall <- rbind(Xtest_all, Xtrain_all)
rm(list=c("Xtest_all","Xtrain_all"))
save(Xall, file = "Xall.RData")

##############
###
### STEP 2. Extract the mean and sd for each measurement ----
###
##################################################################

#- I get the positions of columns including "mean" or "std" in their names
cols_mean <- as.vector(grep("mean", names(Xall)))
cols_sd <- as.vector(grep("std", names(Xall)))
cols_meansd <- sort(c(cols_mean, cols_sd))

#- Select those columns to create a new dataset. Also keep the 3 first ID variables
Xall_meansd <- select(Xall, 1:3, cols_meansd)
length(Xall_meansd)
rm(Xall)

##############
###
### STEP 3. Use descriptive activity names  ----
###
##################################################################

Xall_meansd$activity <- as.factor(Xall_meansd$activity)
 
levels(Xall_meansd$activity) <- labelActivity$activityName


##############
###
### STEP 4. Label the dataset with descriptive variable names  ----
###
######################################################################

Xnames <- names(Xall_meansd)
Xnames <- sub("tBody", "time.Body", Xnames)
Xnames <- sub("fBody", "frequency.Body", Xnames)
Xnames <- sub("tGravity", "time.Gravity", Xnames)
Xnames <- sub("fGravity", "frequency.Gravity", Xnames)
Xnames <- sub("Acc", ".accelerometer", Xnames)
Xnames <- sub("Gyro", ".gyroscope", Xnames)
Xnames <- sub("Mag", ".magnitude", Xnames)
Xnames <- sub("Jerk", ".Jerk", Xnames)

#- Here it is also corrected this repetition that seems to be a typo
Xnames <- sub("BodyBody", "Body", Xnames)
names(Xall_meansd) <- Xnames


##############
###
###   STEP 5. Independent dataset with average of each variable/activity/subject  ----
###
######################################################################################

#--- Summarising
#-    Notes: 
#-    1. The variable "dataset" is kept in case of future interest)
  
tmp <- aggregate(. ~ subject+activity+dataset, data=Xall_meansd, FUN = mean)
Xaverage <- arrange(tmp, subject, activity)

#- New variable names to reflect that average has been computed
names(Xaverage)[4:82] <- paste0("Average_", names(Xaverage)[4:82])


#--- Writing to a file

write.table(Xaverage, file="../../HumanActivityRecognition.txt", 
            row.names = FALSE)


                       ### XXXXXXXXXXXX ###

