library(reshape2)
library(data.table)
library(readr)
library(dplyr)

## **********************************************************************************************
## Part 0.  Downloading and unzipping the file 
## **********************************************************************************************
if (!file.exists("UCI HAR Dataset.zip")){
    link = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(link,"UCI HAR Dataset.zip", method = "curl" )
    unzip("UCI HAR Dataset.zip")
    setwd("./UCI HAR Dataset")
}

## **********************************************************************************************
## Part 1. Merges the training and the test sets to create one data set
## **********************************************************************************************

## Binding train data with activity and subject labels
train_set = tbl_df(fread("./train/X_train.txt"))
train_labels = tbl_df(fread("./train/y_train.txt"))
train_subject = tbl_df(fread("./train/subject_train.txt"))
train_bind = bind_cols(train_subject,train_labels,train_set)


## Binding test data with activity and subject labels
test_set = tbl_df(fread("./test/X_test.txt"))
test_labels = tbl_df(fread("./test/y_test.txt"))
test_subject  = tbl_df(fread("./test/subject_test.txt"))
test_bind = bind_cols(test_subject,test_labels,test_set)

## combining training and testing datasets into one
data = bind_rows(train_bind,test_bind)

## Creating column names for the dataset using featurenames
features_names = read.table("features.txt",col.names = c("S/N", "Feature"))
valid_features_names <- make.names(names=features_names$Feature, unique=TRUE)
names(data) <- c("subject","activity",valid_features_names)


## **********************************************************************************************
## Part 2. Extracts only the measurements on the mean and standard deviation for each measurement
## **********************************************************************************************

subset_data <- select(data,subject,activity,contains(".mean."),contains(".std."))


## **********************************************************************************************
## Part 3. Uses descriptive activity names to name the activities in the data set
## **********************************************************************************************

activity_lab = tbl_df(fread("activity_labels.txt", col.names = c("Tag","Activity")))
subset_data$activity = activity_lab$Activity[match(subset_data$activity, activity_lab$Tag)]

## **********************************************************************************************
## Part 4. Appropriately labels the data set with descriptive variable names.
## **********************************************************************************************

names(subset_data)[3:68] = gsub("^t","Time_",names(subset_data)[3:68])
names(subset_data)[3:68] = gsub("^f","FFT_",names(subset_data)[3:68])
names(subset_data)[3:68] = gsub("Gyro","_Gyroscope",names(subset_data)[3:68])
names(subset_data)[3:68] = gsub("Acc","_Acceleration",names(subset_data)[3:68])
names(subset_data)[3:68] = gsub("Mag","_Magnitude",names(subset_data)[3:68])

## renaming the dataset
tidy_data = subset_data

## Writes the dataframe into a .txt file
tidy_data%>% write.table("tidy_data.txt",row.name=FALSE)

## **********************************************************************************************
## Part 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
## **********************************************************************************************

summary_data = tidy_data %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

## Writes the summarized dataframe into a .txt file
summary_data%>% write.table("summary_data.txt", row.names = FALSE)

names(tidy_data)
