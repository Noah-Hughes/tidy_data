## run_analysis.R
## Written by Noah Hughes
## This program assumes that the "UCI HAR Dataset" Folder is in the current directory
## This program will not run correctly if the "UCI HAR Dataset" is not in the correct directory



run_analysis <- function()
  
{
  library(reshape2)
  library(plyr)
  
  #Read labels
  Labels <- read.csv("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
  ActivityLabels <- read.csv("./UCI HAR Dataset/activity_labels.txt",header = FALSE, sep = "")
  
  #reads files into datasets
  Subtrain <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "", col.names = c("Subject"))
  Xtrain <- read.csv("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", col.names = C(Labels[,2]))
  Ytrain <- read.csv("./UCI HAR Dataset/train/Y_train.txt", header = FALSE, sep = "", col.names = c("Activity_Label"))
  Subtest <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "",  col.names = c("Subject"))
  Xtest <- read.csv("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", col.names = C(Labels[,2]))
  Ytest <- read.csv("./UCI HAR Dataset/test/Y_test.txt", header = FALSE, sep = "", col.names = c("Activity_Label"))
  
  #combine train datasets
  train <- cbind(Subtrain,Ytrain,Xtrain)
  
  #combine test datasets
  test <- cbind(Subtest,Ytest,Xtest)

  # Combines the test and train datasets
  total <- rbind(test,train)
  
  #gets the mean and std data with Activity_Label and Subject
  meanstdtotal <- total[,names(total)[grep("mean|std|Activity_Label|Subject", names(total))]]
 
  #Apply Activity Labels
  dataset2 = mutate(meanstdtotal, Activity_Label=factor(meanstdtotal$Activity_Label,labels=ActivityLabels$V2))
  
  #tidy dataset
  datamelt <- melt(dataset2,id=c(names(dataset2[,1:2])),measure.vars=c(names(dataset2[,3:81])))
  
  #data table by Subject and Activity with Averages for each measurement
  dataData <- dcast(datamelt, Subject + Activity_Label ~ variable,mean)
  
  #write tidy data
  write.table(dataData, file = "tidydata.txt")
  
}


