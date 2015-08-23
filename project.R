library(reshape2)


#Task 0. Download data file. 
#Here we create the working directory where all files will be saved. To avoid potential problems We check whetehr the directory exists. 
#In the created directory we download and unzip the dataset. 
projectDirectoryName = 'getdata_project' 
if (!file.exists(projectDirectoryName)){
  dir.create(projectDirectoryName)#create directory for the project

}
setwd(projectDirectoryName)

datasetURL = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
datasetFilename = 'getdata_project_dataset.zip'

if (!file.exists(datasetFilename)){
  download.file(datasetURL, datasetFilename, method="wininet") #in windows this method works fine. Other OS my require to change method to 'curl'.  
}

if(!file.exists('UCI HAR Dataset')){
  unzip(datasetFilename)  
}


#Firstly, we extract the names of activities and features to give names to the training and test set.
#Then, we merge sets using subset of features which contain information only about means or standard deviation.
activityLabels = read.table('UCI HAR Dataset/activity_labels.txt')  
activityLabels[,2] = as.character(activityLabels[,2]) #by default it is supposed to be of integer tye, however, we would to use them as characters. They are just names.
features = read.table('UCI HAR Dataset/features.txt')
features[,2] = as.character(features[,2])


#extract data with mean and standard deviation (all features which have mean and std in their names)
#to find wanted features we use regular expressions
featuresMeanStd = grep('.*mean.*|.*std.*', features[,2])
featuresMeanStd.names = features[featuresMeanStd, 2]
featuresMeanStd.names = gsub('-mean', 'Mean', featuresMeanStd.names)  #Give proper names for features with Mean
featuresMeanStd.names = gsub('-std', 'Std', featuresMeanStd.names)  #Give proper names for features with STD
featuresMeanStd.names <- gsub('[-()]', '', featuresMeanStd.names)

# Load the datasets using obtained subset of features
train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresMeanStd]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresMeanStd]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

# merge datasets and add labels
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresMeanStd.names)

#change some elements of names
names(allData) <- gsub('^t',"TimeDomain.",names(allData))
names(allData) <- gsub('^f',"FrequencyDomain.",names(allData))


# turn activities into factors
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

write.table(allData.mean, "tidy_dataset.txt", row.names = FALSE, quote = FALSE)

