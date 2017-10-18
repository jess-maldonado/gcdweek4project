library(tidyverse)

## Setting file names for all the relevant files
file_testdata <- "~/test/X_test.txt"
file_testnames <- "~/test/y_test.txt"
file_traindata <- "~/train/X_train.txt"
file_trainnames <- "~/train/y_train.txt"
file_activities <- "~/activity_labels.txt"
file_features <- "~/features.txt"
file_subject_test <- "~/test/subject_test.txt"
file_subject_train <- "~/train/subject_train.txt"

## Set your working directory to where the Samsung data exists
setwd ("~/DSAcceleratorRepos/GCDataFiles/ProjectFile")

## Reading in all of the files
x_test_data <- read.table(file_testdata, header = FALSE)
y_test_data <- read.table(file_testnames, header = FALSE)
x_train_data <- read.table(file_traindata, header = FALSE)
y_train_data <- read.table(file_trainnames, header = FALSE)
features <- read.table(file_features, header = FALSE)
activities <- read.table(file_activities, header = FALSE)
subject_test <- read.table(file_subject_test, header = FALSE)
subject_train <- read.table(file_subject_train, header = FALSE)


##TRANSFORMING THE TEST DATA

## Adding the features as column names, and adding the ActivityIDs and SubjectIDs on as new columns for the test data.
colnames(x_test_data) <- features$V2
x_test_data["ActivityID"] <- y_test_data
x_test_data["SubjectID"] <- subject_test

## Adding column names to the activity table to make joining and selecting more intuitive
colnames(activities) <- c("ActivityID", "Activity")

## Joining the activity name onto the test data by activityID.
## Selecting on the mean and std columns from the data
x_test_data2 <- left_join(x_test_data, activities, by = c("ActivityID" = "ActivityID")) %>%
  select(SubjectID, Activity, contains("mean()"), contains("std()"))


##TRANSFORMING THE TRAIN DATA (performing the same transformations as test)

## Adding the features as column names, and adding the ActivityIDs and SubjectIDs on as new columns for the train data.
colnames(x_train_data) <- features$V2
x_train_data["ActivityID"] <- y_train_data
x_train_data["SubjectID"] <- subject_train

x_train_data2 <- left_join(x_train_data, activities, by = c("ActivityID" = "ActivityID")) %>%
  select(SubjectID, Activity, contains("mean()"), contains("std()"))
x_train_data2 %>% glimpse

##unioning the two refined data sets into one with all observations.
data_combined <- rbind(x_test_data2, x_train_data2)

## Updating the column names to be more intuitive
colnames(data_combined) <- sub("^t", "AvgTime", colnames(data_combined))
colnames(data_combined) <- sub("^f", "AvgFreq", colnames(data_combined))
colnames(data_combined) <- sub("-mean\\(\\)","_Mean", colnames(data_combined))
colnames(data_combined) <- sub("-std\\(\\)", "_StdDev", colnames(data_combined))
colnames(data_combined) <- sub("([XYZ])$", "\\1-Axis", colnames(data_combined))

## Providing the second tidy data set with the mean for each mean() and std() observation by subject and activity type.
all_means <- data_combined %>%
  group_by(SubjectID, Activity) %>%
  summarise_all(., funs(mean(.)))

write.table(all_means, file = "~/DSAcceleratorRepos/GCDataFiles/FinalProjectFiles/Step5Data.txt", row.name = FALSE)
