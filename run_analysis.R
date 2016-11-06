# Coursera Assignment: Course 3, week 4
# by Mario Giesel, nov 2016

rm(list=ls())
setwd('Q:/Organisation/Privat/Giesel_Mario/XXX_R/Coursera/Kurs3/week4')

# ------------------------------------------------------------
# 1. Merge the training and the test sets to create one data set.
# ------------------------------------------------------------

library(dplyr) # wanna use functions dplyr::bind_rows() and dplyr::bind_cols()

train1 <- read.table("./UCI_Dataset/train/subject_train.txt", col.names = "id") # subject id of training dataset
train2 <- read.table("./UCI_Dataset/train/y_train.txt", col.names = "activity") # activity of training dataset
train3 <- read.table("./UCI_Dataset/train/X_train.txt") # measurements of training dataset
train  <- bind_cols(train1, train2, train3) # putting them together
train  <- mutate(train, status = "Training") # mark the experimental status of the respondents
rm(train1, train2, train3) # clean the house

test1 <- read.table("./UCI_Dataset/test/subject_test.txt", col.names = "id") # subject id of test dataset
test2 <- read.table("./UCI_Dataset/test/y_test.txt", col.names = "activity") # activity of test dataset
test3 <- read.table("./UCI_Dataset/test/X_test.txt") # measurements of test dataset
test  <- bind_cols(test1, test2, test3) # putting them together
test  <- mutate(test, status = "Test") # mark the experimental status of the respondents
rm(test1, test2, test3) # clean the house

mydata <- bind_rows(test, train)
rm(test, train) # clean the house

# ------------------------------------------------------------
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# ------------------------------------------------------------
# from features.txt choose mydata variables that contain mean() or std() and write results to mydata2 
mydata2 <- select(mydata, id, status, activity,
                  V1  : V6,
                  V41 : V46 ,
                  V81 : V86 ,
                  V121: V126,
                  V161: V166,
                  V201: V202,
                  V214: V215,
                  V227: V228,
                  V240: V241,
                  V253: V254,
                  V266: V271,
                  V345: V350,
                  V424: V429,
                  V503: V504,
                  V516: V517,
                  V529: V530,
                  V542: V543)

# ------------------------------------------------------------
# 3. Use descriptive activity names to name the activities in the data set
# ------------------------------------------------------------

table(mydata2$activity) # check variable before we start
# label values according to "activity_labels.txt"
mydata2$activity <- factor(mydata$activity,
    levels = c(1,2,3,4,5,6),
    labels = c("Walking", "Walking upstairs", "Walking downstairs", 
               "Sitting", "Standing", "Laying")) 
table(mydata2$activity) # recheck variable

# ------------------------------------------------------------
# 4. Appropriately label the data set with descriptive variable names.
# ------------------------------------------------------------
# descriptively renaming variables using the descriptive labels from features.txt
mydata3 <- rename(mydata2, 
       tBodyAcc_mean_X = V1, 
       tBodyAcc_mean_Y = V2,
       tBodyAcc_mean_Z = V3, 
       tBodyAcc_std_X = V4,
       tBodyAcc_std_Y = V5, 
       tBodyAcc_std_Z = V6,
       tGravityAcc_mean_X = V41,
       tGravityAcc_mean_Y = V42,
       tGravityAcc_mean_Z = V43,
       tGravityAcc_std_X = V44,
       tGravityAcc_std_Y = V45,
       tGravityAcc_std_Z = V46,
       tBodyAccJerk_mean_X = V81,
       tBodyAccJerk_mean_Y = V82,
       tBodyAccJerk_mean_Z = V83,
       tBodyAccJerk_std_X = V84,
       tBodyAccJerk_std_Y = V85,
       tBodyAccJerk_std_Z = V86,
       tBodyGyro_mean_X = V121,
       tBodyGyro_mean_Y = V122,
       tBodyGyro_mean_Z = V123,
       tBodyGyro_std_X = V124,
       tBodyGyro_std_Y = V125,
       tBodyGyro_std_Z = V126,
       tBodyGyroJerk_mean_X = V161,
       tBodyGyroJerk_mean_Y = V162,
       tBodyGyroJerk_mean_Z = V163,
       tBodyGyroJerk_std_X = V164,
       tBodyGyroJerk_std_Y = V165,
       tBodyGyroJerk_std_Z = V166,
       tBodyAccMag_mean = V201,
       tBodyAccMag_std = V202,
       tGravityAccMag_mean = V214,
       tGravityAccMag_std = V215,
       tBodyAccJerkMag_mean = V227,
       tBodyAccJerkMag_std = V228,
       tBodyGyroMag_mean = V240,
       tBodyGyroMag_std = V241,
       tBodyGyroJerkMag_mean = V253,
       tBodyGyroJerkMag_std = V254,
       fBodyAcc_mean_X = V266,
       fBodyAcc_mean_Y = V267,
       fBodyAcc_mean_Z = V268,
       fBodyAcc_std_X = V269,
       fBodyAcc_std_Y = V270,
       fBodyAcc_std_Z = V271,
       fBodyAccJerk_mean_X = V345,
       fBodyAccJerk_mean_Y = V346,
       fBodyAccJerk_mean_Z = V347,
       fBodyAccJerk_std_X = V348,
       fBodyAccJerk_std_Y = V349,
       fBodyAccJerk_std_Z = V350,
       fBodyGyro_mean_X = V424,
       fBodyGyro_mean_Y = V425,
       fBodyGyro_mean_Z = V426,
       fBodyGyro_std_X = V427,
       fBodyGyro_std_Y = V428,
       fBodyGyro_std_Z = V429,
       fBodyAccMag_mean = V503,
       fBodyAccMag_std = V504,
       fBodyBodyAccJerkMag_mean = V516,
       fBodyBodyAccJerkMag_std = V517,
       fBodyBodyGyroMag_mean = V529,
       fBodyBodyGyroMag_std = V530,
       fBodyBodyGyroJerkMag_mean = V542,
       fBodyBodyGyroJerkMag_std = V543)

# ------------------------------------------------------------
# From the data set in step 4, 
# create a second, independent tidy data set with the average of each variable for each activity and each subject.
# ------------------------------------------------------------ 

library(tidyr)

#data melting for aggregation and reorganisation
mydata4 <- gather(data=mydata3, key=measure, value=value, -(id:activity)) 

# variant 1: means for id and activity
mygroup <- group_by(mydata4, id, activity) # define aggregation groups id * activity
mydata5 <- summarise(mygroup, mean = mean(value)) # mean aggregation
mydata6 <- spread(mydata5, key=activity, value = mean) # making ist tidy
mydata6 # tidy data: each id is a row, each activity is a column
 
# variant 2: means for id and activity and measure
mygroup2 <- group_by(mydata4, id, activity, measure) # define aggregation groups id * activity * measure
mydata7 <- summarise(mygroup2, mean = mean(value)) # mean aggregation
mydata8 <- spread(mydata7, key=measure, value = mean) # making ist tidy
mydata8 # tidy data: each id*activity unit is a row, each variable is a column

#export
write.table(mydata6, file = 'tidy1.txt', row.names = FALSE)
write.table(mydata8, file = 'tidy2.txt', row.names = FALSE)
