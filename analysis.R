library(httr)
library(dplyr)
library(tidyr)


###Step 1: download the data source

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

GET(url, write_disk("dataset.zip" , overwrite = TRUE))

unzip("dataset.zip")

###read headers and labels

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

###filter mean and standard deviation


featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted_names <- features[featuresWanted , 2]
featuresWanted_names <- gsub('-mean', 'Mean', featuresWanted_names) # better names
featuresWanted_names <- gsub('-std', 'Std', featuresWanted_names)# better names
featuresWanted_names <- gsub('[-()]', '', featuresWanted_names)


##lload training data set

train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

##load test data set

test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)


##Merge all data and add column names


All <- rbind(train, test)
names(All) <- c("subject", "activity", featuresWanted_names)

##Final link to acitivty names

All <- All %>%
        mutate( activity =  factor(activity, levels = activity_labels[,1], labels = activity_labels[,2])) %>%
        mutate( subject = as.factor(subject))

All.tidy <- All %>%
        gather(measure, value, -subject, -activity)

All.mean <- All.tidy %>%
        group_by(subject, activity, measure) %>%
        summarise(mean(value))

        
write.table(All.mean, "final_tidy.txt", row.names = FALSE, quote = FALSE)
