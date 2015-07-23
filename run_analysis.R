new file mode 100644
@@ -0,0 +1,54 @@
+run_analysis <- function(){
+  
+  #Read in data
+  xtest <- read.table("./X_test.txt", header=FALSE, sep="")
+  xtrain <- read.table("./x_train.txt", header=FALSE, sep="")
+  ytest <- read.table("./y_test.txt", header=FALSE, sep="")
+  ytrain <- read.table("./y_train.txt", header=FALSE, sep="")
+  subtest <- read.table("./subject_test.txt", header=FALSE, sep="")
+  subtrain <- read.table("./subject_train.txt", header=FALSE, sep="")
+  
+  #Combine all the data into one table
+  test <- cbind(subtest, ytest, xtest)
+  train <- cbind(subtrain, ytrain, xtrain)
+  alldata <- rbind(test, train)
+  
+  #Read features
+  features <- read.table("./features.txt", header=FALSE, sep="")
+  features[,2] <- as.character(features[,2])
+  
+  #Header names based on features vector
+  colnames(alldata) <- c("Subject", "Activity", features[,2])
+  
+  #Extract the mean and std deviation data
+  subdata <- alldata[, c(1, 2, grep(".mean\\(\\)|.std\\(\\)", names(alldata)))]
+  
+  #Read activity labels
+  activities <- read.table("./activity_labels.txt", header=FALSE, sep="")
+  activities[, 2] <- gsub("_", " ", tolower(activities[, 2])) #Removes underscore & makes it lowercase
+  
+  #Adds descriptive names for activties
+  subdata$Activity <- activities[subdata$Activity, 2]
+  
+  #Changing labels to be more meaningful
+  names(subdata) <- tolower(names(subdata))
+  names(subdata) <- sub("^t", "time_", names(subdata))
+  names(subdata) <- sub("^f", "freq_", names(subdata))
+  names(subdata) <- sub("acc", "_acc_", names(subdata))
+  names(subdata) <- sub("gyro", "_gyro_", names(subdata))
+  names(subdata) <- sub("-std\\(\\)", "std", names(subdata))
+  names(subdata) <- sub("-mean\\(\\)", "mean", names(subdata))
+  names(subdata) <- sub("jerk", "jerk_", names(subdata))
+  names(subdata) <- sub("mag", "mag_", names(subdata))
+  names(subdata) <- sub("-x", "_x", names(subdata))
+  names(subdata) <- sub("-y", "_y", names(subdata))
+  names(subdata) <- sub("-z", "_z", names(subdata))
+  
+  #Creating second tidy data with average of each variable for each subject and each activity
+  tidydata <- melt(subdata, id=c("subject", "activity"), measure.vars=names(data)[3:length(names(data))])
+  tidydata <- dcast(tidydata, subject + activity ~ variable, mean)
+  
+  #Outputs tidy data to a txt file in the working directory
+  write.table(tidydata, file="tidy_data.txt", row.names=FALSE)
+  
+}
\ No newline at end of file

