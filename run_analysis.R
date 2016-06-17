#run_analysis.R
#assignment Getting & Cleaning data

#Read test and train set
testset<-read.table("./test/X_test.txt")
trainset<-read.table("./train/X_train.txt")

#Read variables en rename duplicate rows
variables<-read.table("features.txt")
variables<-variables[,2]
variables<-as.character(variables)
while (anyDuplicated(variables)!=0){
  i<-anyDuplicated(variables)
  variables[i]<-paste(variables[i],"a")
}

#Read subjects
subjecttrain<-read.table("./train/subject_train.txt")
subjecttest<-read.table("./test/subject_test.txt")

#Read activities
activitytrain<-read.table("./train/y_train.txt")
activitytest<-read.table("./test/y_test.txt")
activitynames<-read.table("activity_labels.txt")
activitytrain<-left_join(activitytrain, activitynames, by="V1")
activitytrain<-activitytrain[,2]
activitytest<-left_join(activitytest, activitynames, by="V1")
activitytest<-activitytest[,2]

#combine test en train set and ad variables, subject and activity(names)
testset2<-cbind(subjecttest, activitytest, testset)
trainset2<-cbind(subjecttrain, activitytrain, trainset)
colnames(trainset2)[1:2]<-c("subject", "activity")
colnames(trainset2)[3:563]<-variables
colnames(testset2)[1:2]<-c("subject", "activity")
colnames(testset2)[3:563]<-variables
alldata<-rbind(testset2, trainset2)


#extract measurements on the mean and standarddeviation
library(dplyr)
mean_std<-select(alldata, subject, activity, contains("mean"), contains("std"))

#create new dataset with average per subject and activity
new_data<-data.frame()
for (i in 1:30) {
  temp<-filter(mean_std, subject==i)
  res<-temp %>% group_by(activity) %>% summarise_each(funs(mean))
  new_data<-rbind(new_data, res)
}

# create text file
write.table(new_data, "results_assignment.txt", row.name=FALSE)

