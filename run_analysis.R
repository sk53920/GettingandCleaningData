library(tidyr)
library(dplyr)

#read features table for column names of x data set
feat <- read.table("features.txt")

path <- getwd()

#read in training set
x_train <- read.table(paste(path,"/train/X_train.txt", sep=""))
y_train <- read.table(paste(path,"/train/y_train.txt", sep=""))
s_train <- read.table(paste(path,"/train/subject_train.txt", sep=""))

#read in test set
x_test <- read.table(paste(path,"/test/X_test.txt", sep=""))
y_test <- read.table(paste(path,"/test/y_test.txt", sep=""))
s_test <- read.table(paste(path,"/test/subject_test.txt", sep=""))

#assign descriptive labels to variables of columns
x_trainnames <- setNames(x_train, feat$V2)
x_testnames <- setNames(x_test, feat$V2)

#Combine x, y, and subject of training data and create new column to assign data source
combtrain <- bind_cols(s_train, y_train, x_trainnames)
combtrain <- combtrain %>% rename(Subject = V1, Activity = V11) %>% as_tibble() %>% mutate(DataSource = c("Training"))

#Combine x, y, and subject of test data and create new column to assign data source
combtest <- bind_cols(s_test, y_test, x_testnames)
combtest <- combtest %>% rename(Subject = V1, Activity = V11) %>% as_tibble() %>% mutate(DataSource = c("Training"))

#concatenate both data sets into new merged data table
merged <- bind_rows(combtrain, combtest)

#Select only measurements containing mean or standard deviation in the column names
mergeds <- merged %>% select(Subject, Activity, matches('mean|std'))

#label activity coulmn with descriptive names instead of numerical values
mergedsa <- mergeds %>%  mutate(Activity = recode(Activity, '1' = "Walking", 
                           '2' = "WalkingUpstairs",
                           '3'= "WalkingDownstairs",
                           '4'= "Sitting", 
                           '5' = "Standing", 
                           '6'="Laying")
         )


#Create second, independent data set with average of each variable for each activity and subject
avg <- mergedsa %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

write.table(avg, file="avgtable.txt", row.name = FALSE)

