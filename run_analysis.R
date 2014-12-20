# Here's this week's programming assignment:
# You should create one R script called run_analysis.R that does the following.
#  * Merges the training and the test sets to create one data set (TASK 1)
#  * Extracts only the measurements on the mean and standard deviation for each measurement (TASK 2 )
#  * Uses descriptive activity names to name the activities in the data set (TASK 3)
#  * Appropriately labels the data set with descriptive variable names (TASK 4)
#  * From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject (TASK 5)
  
# First, let's specify the directory where we've downloaded the Samsung data and set this as our working directory

whereToLook <- "/Users/ad26693/Downloads/UCI HAR Dataset"
setwd(whereToLook)

# Now, let's read in the data files we need and solve TASK 1 (and TASK 4 at the same time)...

# the following code block reads in the subject, activity, and data files for the TRAINING data set and joins them together, adding the appropriate variable column names provided by the features.txt file
# it also adds a new column is added that identifies each record as coming from either the TRAINING or TEST data sets
# and the columns containing the data are named with the appropriate descriptive variable names

features <- read.table("features.txt",sep="")
features <- features[,2]

# this code block reads the TRAINING data set
subject_train <- read.table("train/subject_train.txt",sep="")
activity_train <- read.table("train/Y_train.txt",sep="")
data_train <- cbind(subject_train,"train",activity_train)
colnames(data_train) <- c("subject","data_set","activity")
values_train <- read.table("train/X_train.txt",sep="")
#x_train consists of 7352 observations of 561 variables

colnames(values_train) <- features
data_train <- cbind(data_train, values_train)

# this code block reads the TEST data set
subject_test <- read.table("test/subject_test.txt",sep="")
activity_test <- read.table("test/Y_test.txt",sep="")
data_test <- cbind(subject_test,"test",activity_test)
colnames(data_test) <- c("subject","data_set","activity")
values_test <- read.table("test/X_test.txt",sep="")
#x_test consists of 7352 observations of 561 variables

colnames(values_test) <- features
data_test <- cbind(data_test, values_test)

# and this bit merges the TRAINING and TEST data sets and then cleans the workspace of all but the combined "full" data set

full_data <- rbind(data_train, data_test) # full_data is the solution to TASK 1 (it also accomplishes TASK 4, as the descriptive variable names have been added)

full_data[1:10,1:9] # show a bit of the results

# the following lines clean up the workspace leaving only the solution to TASKS 1 and 4
toRemove <-ls()[ls()!="full_data"]
remove(list=toRemove)
remove(toRemove)

# Okay... now let's accomplish TASK 2, winnowing down the data set to keep only the columns/variables of interest. For this, we need the `plyr` package.

library(plyr)

# the following uses regex to select the columns/variables to keep
keep = grepl("subject|data_set|activity|mean\\(|std\\(", colnames(full_data))

# and then we subset the data keeping only the columns of interest
subset_data <- full_data[,keep] # subset_data is the solution to TASK 2

subset_data[1:10,1:7] # show a bit of the results... first 4 variables, with activity CODE rather than activity name

# cleanup
remove(keep)

# Now, let's solve TASK 3, adding descriptive activity names to the data set...

activity_names <- read.table("activity_labels.txt",sep="")
colnames(activity_names) <- c("activity","activity_name")
# adds descriptive activity names to name the activities in the data set
tidy_data <- merge(subset_data,activity_names,by="activity") # tidy_data is the solution to TASK 3

tidy_data[1:10,c("subject","activity_name","tBodyAcc-mean()-X")] # show a bit of the results... only one example variable

# Finally, let's create a new tidy data set with the average of each variable for each activity and each subject... this is easy to do with the `aggregate` function...

tidy_summarized_data <- aggregate(tidy_data, by=list(tidy_data$subject,tidy_data$activity_name), FUN=mean, na.rm=TRUE) # tidy_summarized_data is the solution to TASK 5...

# ... but we should clean up the dataframe a bit
keep = grepl("Group|mean\\(|std\\(", colnames(tidy_summarized_data))
tidy_summarized_data <- tidy_summarized_data[,keep]
# rename the two aggregating columns
names(tidy_summarized_data)[1]="subject"
names(tidy_summarized_data)[2]="activity"      
# now, tidy_summarized_data includes only the variables of interest

tidy_summarized_data[1:10,1:6] # show a bit of the results... only a few example variables

write.table(tidy_summarized_data, "~/Desktop/tidy_summarized_data.txt", col.names=TRUE,row.names = FALSE, sep =",") 

# finally, we can clean up the workspace a bit an keep only those dataframe of interest
toRemove <-ls()[ls()!="full_data" & ls()!="tidy_data" & ls()!="tidy_summarized_data"]

remove(list=toRemove)
remove(toRemove)