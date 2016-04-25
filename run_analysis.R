# 1. Merge the training and the test data to create one data set.

# Read in the data from files
#imports features.txt
features = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\features.txt",header=FALSE) 
#imports activity_labels.txt
activityType = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\activity_labels.txt",header=FALSE) 
#imports subject_train.txt
subjectTrain = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\train\\subject_train.txt",header=FALSE) 
#imports x_train.txt
xTrain = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\train\\x_train.txt",header=FALSE) 
#imports y_train.txt
yTrain = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\train\\y_train.txt",header=FALSE) 

# Assigin column names to the imported data
colnames(activityType) = c('activityId','activityType')
colnames(subjectTrain) = 'subjectId'
colnames(xTrain) = features[,2] 
colnames(yTrain) = 'activityId'

# Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)


# Read in the test data
#imports subject_test.txt
subjectTest = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\test\\subject_test.txt",header=FALSE)
#imports x_test.txt
xTest = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\test\\x_test.txt",header=FALSE)
#imports y_test.txt
yTest = read.table("C:\\Users\\Andrew\\Desktop\\UCI HAR Dataset\\test\\y_test.txt",header=FALSE) 

# Assign column names to the test data 
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2] 
colnames(yTest) = "activityId"


# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest)


# Combine training and test data to create the full data set
FullData = rbind(trainingData,testData)


# Create a vector for the column names from the full data, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(FullData)

# 2. Extract only the mean and standard deviation for each measurement. 

# Create a logical picking vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
PickVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData table based on the logicalVector to keep only desired columns
FullData = FullData[PickVector==TRUE]

# 3. Use descriptive activity names for the variables in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
FullData = merge(FullData,activityType,by='activityId',all.x=TRUE)

# Update the colNames vector to include the new column names after merge
colNames  = colnames(FullData)

# 4. Label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(FullData) = colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, FullDataNoActivityType without the activityType column
FullDataNoActivityType = FullData[,names(FullData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(FullDataNoActivityType[,names(FullDataNoActivityType) != c('activityId','subjectId')],
                    by=list(activityId=FullDataNoActivityType$activityId,subjectId = FullDataNoActivityType$subjectId),mean)

# Merging the tidy data with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export the tidy data set 
write.table(tidyData, 'C:\\Users\\Andrew\\Desktop\\tidyData.txt',row.names=TRUE,sep='\t')

