#You should create one R script called run_analysis.R that does the following: 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive activity names. 
# 5.Creates a second, independent tidy data set with the average of 
#    each variable for each activity and each subject. 


library(data.table)
library(reshape2)

# 1.Merges the training and the test sets to create one data set.

# Load all data by using read.tables
trainsetX<-read.table("./UCI HAR Dataset/train/X_train.txt")
testsetX<-read.table("./UCI HAR Dataset/test/X_test.txt")
trainsetY<-read.table("./UCI HAR Dataset/train/y_train.txt")
testsetY<-read.table("./UCI HAR Dataset/test/y_test.txt")

Subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
Subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

features<-read.table("./UCI HAR Dataset/features.txt")
activity_lables<-read.table("./UCI HAR Dataset/activity_labels.txt")

# load column names
feature_names <- features[,2]
activity_names <- activity_lables[,2]

# add labels
 names(testsetX)<-feature_names
 names(trainsetX)<-feature_names


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

testsetX<-testsetX[,grepl("mean|std", feature_names)]
trainsetX<-trainsetX[,grepl("mean|std", feature_names)]

# combind the test and train data sets
alldata<-rbind(trainsetX,testsetX)

# 3. Uses descriptive activity names to name the activities in the data set

testsetY[,2]<-activity_names[testsetY[,1]]
trainsetY[,2]<-activity_names[trainsetY[,1]]

activities<-rbind(testsetY, trainsetY)


# 4. Appropriately labels the data set with descriptive variable names. 

names(activities)<-c("ActivityID", "ActivityLabel")

subject<-rbind(Subject_test, Subject_train)
colnames(subject)<-"SubjectNumber"

combinddata<-cbind(as.data.table(subject), activities, alldata)


# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#5a: Calculate average of each variable for each activity and each subject
id_labels<-c("SubjectNumber", "ActivityID", "ActivityLabel")
data_labels<-setdiff(colnames(combinddata), id_labels)
predata<-melt(combinddata, id = id_labels, measure.vars = data_labels)
result<-dcast(predata, SubjectNumber + ActivityLabel ~ variable, mean)
write.table(result, "Tidydataset.txt",sep="/")


