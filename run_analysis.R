## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#load the required library, install if not present
if (!require("data.table")) {
    install.packages("data.table")
}
library(data.table)

#Download the zip file in the working directory and unzip it.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- getwd()
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, "Dataset.zip"))
unzip("Dataset.zip")

#set the files path for training and test folders
dataPath <- file.path(path, "UCI HAR Dataset")
dataPathTest <- file.path(path, "UCI HAR Dataset/test")
dataPathTraining <- file.path(path, "UCI HAR Dataset/train")

# 1. Merge the training and the test sets to create one data set.

#Read the activity and features files
activity <- fread(file.path(dataPath, "activity_labels.txt")) 
setnames(activity, names(activity), c("activityCode", "activityName"))
features <- fread(file.path(dataPath, "features.txt")) 

#Read the training files
subjectTraining <- fread(file.path(dataPathTraining, "subject_train.txt")) 
xTraining <- fread(file.path(dataPathTraining, "X_train.txt")) 
yTraining <- fread(file.path(dataPathTraining, "y_train.txt")) 

#Add variables to the training data
setnames(subjectTraining, names(subjectTraining), "subjectId")
setnames(xTraining, names(xTraining), features$V2) 
setnames(yTraining, names(yTraining), "activityCode")

#merge all the training data files
dtTraining <- cbind(subjectTraining, xTraining, yTraining) 

#Read the test files
subjectTest <- fread(file.path(dataPathTest, "subject_test.txt"))
xTest <- fread(file.path(dataPathTest, "X_test.txt")) 
yTest <- fread(file.path(dataPathTest, "y_test.txt"))

#Add variables to the test data
setnames(subjectTest, names(subjectTest), "subjectId")
setnames(xTest, names(xTest), features$V2) 
setnames(yTest, names(yTest), "activityCode")

#merge all the test data files
dtTest <- cbind(subjectTest, xTest, yTest)

# Combine both the training and test data sets
dtFinal <- rbind(dtTraining, dtTest)

# 2. Extract only the measurements on the mean and standard deviation for each measurement
# include subjectId and activityCode also. 
extract_features <- (grepl("subjectId", names(dtFinal)) | grepl("activityCode", names(dtFinal)) | grepl("-mean\\()", names(dtFinal)) | grepl("-std\\()", names(dtFinal)))

colMeanStd <- names(dtFinal)[extract_features == TRUE]
dtFinal <- subset(dtFinal, select=colMeanStd)

# 3. Uses descriptive activity names to name the activities in the data set
dtFinal <- merge(dtFinal, activity, by = "activityCode", all.x=TRUE)

# 4. Appropriately labels the data set with descriptive variable names.
columnNames <- names(dtFinal)
for (i in 1:length(columnNames)) 
{
    columnNames[i] = gsub("mean\\()","Mean",columnNames[i])
    columnNames[i] = gsub("^t", "time", columnNames[i])
    columnNames[i] = gsub("^f", "freq", columnNames[i])
    columnNames[i] = gsub("std\\()","StdDev",columnNames[i])
    columnNames[i] = gsub("[Bb]ody[Bb]ody|[Bb]ody","Body", columnNames[i])
    columnNames[i] = gsub("","", columnNames[i])
    columnNames[i] = gsub("Mag","Magnitude",columnNames[i])
}
setnames(dtFinal, names(dtFinal), columnNames)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
id <-c("subjectId","activityCode","activityName")
dtMelt <- melt(dtFinal, id=id, measure.vars = setdiff(names(dtFinal),id))

tidyData <- dcast(dtMelt, subjectId + activityName ~ variable, mean)

# Upload the tidy dataset as a txt file with write.table() using row.name=FALSE
write.table(tidyData, file = "./tidydata.txt", row.names = FALSE)



