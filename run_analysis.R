getCleanData <- function() {
    
    library(reshape2)
    
    ##merging train and test sets
    ##readng x and y data of train/test set
    xtrain <- read.table("train/X_train.txt")
    ytrain <- read.table("train/y_train.txt")
    xtest <- read.table("test/X_test.txt")
    ytest <- read.table("test/y_test.txt")
    
    subjecttest <- read.table("test/subject_test.txt")
    subjecttrain <- read.table("train/subject_train.txt")
    
    xData <- rbind(xtest,xtrain)
    yData <- rbind(ytest,ytrain)
    
    
    subjectData <- rbind(subjecttrain,subjecttest)
    
    features <- read.table("features.txt")[,"V2"]
    
    colnames(xData) <- features
    colnames(yData) <- "activity_id"
    colnames(subjectData) <- "subject"
    
    means <- grep("-mean\\(\\)",features,value=TRUE)
    stds <- grep("-std\\(\\)",features,value=TRUE)
    
    newFeatures <- c(means,stds)
    
    xDataNew <- xData[,newFeatures] 
    
    activities <- read.table("activity_labels.txt")
    colnames(activities) <- c("activity_id","activity")
       
    yDataNew <- merge(yData,activities)
    
    data <- cbind(xDataNew,yDataNew["activity"])
    
    
    
    ## write first result data set to csv
    write.table(data, file="measurements_mean_std.txt",sep="\t", row.names=FALSE)
    
    ## join subject ids
    data2 <- cbind(data, subjectData)
    
    ## summarize means per unique (activity, subject) pair 
    data2melt <- melt(data2, id=c("subject", "activity"))
    data3 <- dcast(data2melt, activity + subject ~ variable, mean)
    
    ## write second result data set to csv
    write.table(data3,file="activity_subject_means.txt",sep="\t",row.names = FALSE)
    
}
