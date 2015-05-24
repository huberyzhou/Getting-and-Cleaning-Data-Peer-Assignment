# run_analysis.R
setwd("D:/Coursera/Getting and Cleaning Data")
if(!file.exists("data")){dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="data/UCI HAR Dataset.zip", mode = 'wb')

unzip(zipfile="data/UCI HAR Dataset.zip",exdir="data")

pathfile <- file.path("data" , "UCI HAR Dataset")
files<-list.files(pathfile, recursive=TRUE)
files # view the files' names

library(dplyr)
# First, handle paths of txt files
pathTrain<-file.path(pathfile, "train")
pathTest<-file.path(pathfile, "test")

xtest<-read.table(file.path(pathTest,"X_test.txt"))
ytest<-read.table(file.path(pathTest,"Y_test.txt"))
subject.test<-read.table(file.path(pathTest,"subject_test.txt"))

xtrain<-read.table(file.path(pathTrain,"X_train.txt"))
ytrain<-read.table(file.path(pathTrain,"Y_train.txt"))
subject.train<-read.table(file.path(pathTrain,"subject_train.txt"))

#Get activity labels 
activity.labels<-read.table(file.path(pathfile,
                                      "activity_labels.txt"),
                            col.names = c("Id", "Activity"))
head(activity.labels)

#Get features labels
feature.labels<-read.table(file.path(pathfile,
                                     "features.txt"),
                           colClasses = c("character"))
head(feature.labels)

#1.Merges the training and the test sets to create one data set.
Train<-cbind(cbind(xtrain, subject.train), ytrain)
Test<-cbind(cbind(xtest, subject.test), ytest)
Sensor<-rbind(Train, Test)

sensor.labels<-rbind(rbind(feature.labels, c(562, "Subject")), c(563, "Id"))[,2]
names(Sensor)<-sensor.labels
str(Sensor)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
Sensor.meanstd <- Sensor[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(Sensor))]

#3. Uses descriptive activity names to name the activities in the data set
Sensor.meanstd <- inner_join(Sensor.meanstd, activity.labels, by = "Id", match = "first")
Sensor.meanstd <- Sensor.meanstd[,-1] # Remove ID

#4. Appropriately labels the data set with descriptive names.
names(Sensor.meanstd) <- gsub("([()])","",names(Sensor.meanstd))
names(Sensor.meanstd) <- make.names(names(Sensor.meanstd))
names(Sensor.meanstd)

#5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject 
Final<-ddply(Sensor.meanstd, c("Subject","Activity"), numcolwise(mean))
# improve column names
Final.headers<-names(Final)

addSuffix<- function(x, suffix) {
  if (!(x %in% c("Subject","Activity"))) {
    paste(x,suffix, sep="")
  }
  else{
    x
  }
}

Final.headers<-sapply(Final.headers, addSuffix, ".mean")
names(Final)<-Final.headers

write.table(Final, file = "data/UCI HAR Dataset/Sensor_avg_by_subject.csv", 
            sep = ',',
            row.name=FALSE)
