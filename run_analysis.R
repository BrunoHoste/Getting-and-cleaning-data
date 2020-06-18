library(tidyr)
setwd ("C:/Users/bruno/Desktop/coursera/3_Getting and cleaning data/quiz")

## This first part combines step 1 through 4 of the assignment


        ## Read the features file to set them as variable name for the dataset 

features <- read.table("../quiz/features.txt")
collabels <- as.vector(features$V2)

        ## Read the test set and train set, add variable names, defined above 
        ## subset all the variables with "mean" or "std" in it (for question 2) 
        ## add a variable called 'set' to identify testing (set = 0) or training (set = 1) 

x_test <- read.table("../quiz/test/X_test.txt",col.names=collabels)
x_test_mean_std <- x_test[,grepl("mean|std",names(x_test))]
x_test_mean_std <- cbind(set = 0,x_test_mean_std)
x_train <- read.table("../quiz/train/X_train.txt",col.names=collabels)
x_train_mean_std <- x_train[,grepl("mean|std",names(x_train))]
x_train_mean_std <- cbind(set = 1,x_train_mean_std)

        ## Read the activity number (y_test/y_train) and the activity label (activity)
        ## merge them together (sort=FALSE to keep the current order when merging)
        ## add variable names 'activity.code' and 'activity'

y_test <- read.table("../quiz/test/y_test.txt")
y_train <- read.table("../quiz/train/y_train.txt")
activity <- read.table("../quiz/activity_labels.txt")

testrowactivity <- merge(y_test,activity,by="V1",sort=FALSE)
colnames(testrowactivity) = c("activity.code","activity")
trainrowactivity <- merge(y_train,activity,by="V1",sort=FALSE)
colnames(trainrowactivity) = c("activity.code","activity")

        ## Read the subject table, give it a variable name "subject.number" 

subject_test <- read.table("../quiz/test/subject_test.txt")
colnames(subject_test) <- "subject.number"

subject_train <- read.table("../quiz/train/subject_train.txt")
colnames(subject_train) <- "subject.number"

        ## Bind the subjects, the activity and the test/train set

test1 <- cbind(subject_test,testrowactivity)
test <- cbind(test1,x_test_mean_std)
train1 <- cbind(subject_train,trainrowactivity)
train <- cbind(train1,x_train_mean_std)

        ## Bind the test and train files together in one clean dataset 

data <- rbind(test,train)
  

## Create a dataframe with the avarages of each variable for each activity and each subject
        
        ## split the data according activity and subject number (when there are no values we leave them out (drop))
s <- split(data,list(data$activity,data$subject.number),drop=TRUE)
        
        ## take the mean for each column 
summ <- lapply(s,function(data)colMeans((data[,5:83]),na.rm=TRUE))
        
        ## make a dataframe from the outcome 
df  <- do.call("rbind",summ)
rownames <- rownames(df)
df <- cbind(rownames,df)
df <- data.frame(df)
dff <- separate(df,rownames,into = c("activity","subject"),sep="\\.")
result <- dff[,c(2,1,3:81)]
        
        ## write it in a table
write.table(result,file = "result.txt",row.names=FALSE)

