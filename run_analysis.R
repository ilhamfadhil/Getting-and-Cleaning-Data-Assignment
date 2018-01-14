### ASSIGNMENT INSTRUCTION

#You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.

#Good luck!

#set Working directory
old.dir <- getwd()

if(dir.exists("./data_cleaning_proj")) {
  setwd("./data_cleaning_proj")
} else {
  dir.create(".data_cleaning_proj")
  setwd("./data_cleaning_proj")
}


# Read data file
x_train <- read.table(file = "./UCI Har Dataset/train/X_train.txt",header = FALSE)
y_train <- read.table(file = "./UCI Har Dataset/train/Y_train.txt",header = FALSE)
x_test <- read.table(file = "./UCI Har Dataset/test/X_test.txt",header = FALSE)
y_test <- read.table(file = "./UCI Har Dataset/test/Y_test.txt",header = FALSE)
features <- read.table(file = "./UCI Har Dataset/features.txt", header = FALSE)
subject_test <- read.table(file = "./UCI Har Dataset/test/subject_test.txt", header = FALSE)
subject_train <- read.table(file = "./UCI Har Dataset/train/subject_train.txt", header = FALSE)
subject <- rbind(subject_test, subject_train)

#1.Merge the training and the test sets to create one data set
x_full <- rbind(x_test, x_train) # merge x data set
y_full <- rbind(y_test, y_train) # merge y data set
colnames(x_full) <- features$V2 # 4. Appropriately labels the data set with descriptive variable names.
colnames(y_full) <- c("activity") # create column name for y data set
data_full <- cbind(y_full, x_full) # merge x and y data set

#2.Extract only mean and standard deviation
features$V2 <- as.character(features$V2) # convert factor to character
sub_data <- c(grep("mean()", features$V2, fixed = T)) # Extract row number for mean()
sub_data <- append(sub_data, c(grep("std()", features$V2, fixed = T))) # Extract row number for std()
sub_data <- sort(sub_data) # Sort the row number
sub_data <- sub_data + 1 # to adjust with activity row
ex_data_full <- data_full[, c(1,sub_data)]

#3.Uses descriptive activity names to name the activities in the data set 
ex_data_full$activity_desc <- ifelse(ex_data_full$activity == 1, "Walking", 
                                     ifelse(ex_data_full$activity == 2, "Walking Upstairs",
                                     ifelse(ex_data_full$activity == 3, "Walking Downstairs", 
                                     ifelse(ex_data_full$activity == 4, "Sitting", 
                                     ifelse(ex_data_full$activity == 5, "Standing", "Laying")))))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
total_data <- cbind(ex_data_full, subject)
colnames(total_data)[colnames(total_data) == "V1"] <- "subject"
library(dplyr)
total_data_mean <- total_data %>%
  group_by(activity, subject) %>%
  summarise_all(funs(mean))
write.table(total_data_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
  
#back to old dir
setwd(old.dir)