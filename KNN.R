library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
#View(Insurance_Dataset_)
Insurance_Dataset_ <- Insurance_Dataset_[,-c(1:5,7,11,14:19,21,26:27,31:32)]
sum(is.na(Insurance_Dataset_))
summary(Insurance_Dataset_)
library(Hmisc)
Insurance_Dataset_["Description_illness"] <- with(Insurance_Dataset_,impute(Insurance_Dataset_$Description_illness,mode))
Insurance_Dataset_["Mortality_risk"] <- with(Insurance_Dataset_,impute(Insurance_Dataset_$Mortality_risk,mode))
data_factor <- as.data.frame(lapply(Insurance_Dataset_[,-c(12,15)],factor))
str(data_factor)
library(correlationfunnel)
dummy_data <- binarize(data_factor[,-14])
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(Insurance_Dataset_[,c(12,15)],FUN=normalize))


final_data <- data.frame(data_factor["Result"],dummy_data,data_norm)
sum(is.na(final_data))

set.seed(1)
final_data_1 <- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.70*nrow(final_data)),]
test <- final_data_1[-c(1:as.integer(0.70*nrow(final_data))),]
data_train <- train[,-1]
data_test <- test[,-1]

#Get labels for training and test datasets

data_train_labels <- train[,1]
data_test_labels <- test[,1]

# Build a KNN model on taining dataset
library("class")
train_data_pred <- knn(train=data_train,test=data_train,cl=data_train_labels,k=1)
train_acc <- mean(train_data_pred==data_train_labels)
test_data_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k=1)
test_acc <- mean(test_data_pred==data_test_labels)
library(caret)
confusionMatrix(test_data_pred,test$Result)
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in 1:30)
{
  train_data_pred <- knn(train=data_train,test=data_train,cl=data_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_data_pred==data_train_labels))
  test_data_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_data_pred==data_test_labels))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(1:30,train_acc,type="l",main="Train_accuracy",col="blue")
plot(1:30,test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=1:30))
# Plotting 2 different graphs on same co-ordinate axis
#install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


data_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k=5)
