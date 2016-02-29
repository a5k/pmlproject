##load libraries
library(caret)

##read in datasets
training <- read.csv("./pml/pml_training.csv", header = TRUE, 
                     colClasses = NA, na.strings = c("NA", "#DIV/0", ""))

testing <-  read.csv("./pml/pml_testing.csv", header = TRUE, 
                     colClasses = NA, na.strings = c("NA", "#DIV/0", ""))

## eliminate unncessary varaiables from training and testing set
## columns to remove are columns that contain study metadata and
## columns that don't contain values in every observation in the training data

## remove all columns that contain an NA value since we cannot use this for prediction
training <- training[,which(colSums(is.na(training)) == 0)]
testing <- testing[,which(colSums(is.na(testing)) == 0)]

## the first 7 metadata olumns upon observation are not useful for prediction 
training <- training[,8:ncol(training)]
testing <- testing[,8:ncol(testing)]

## test to see if we have the same columns in both datasets
compareCols <- names(training) == names(testing)
print(compareCols)

## now that an appropriate set of predictors has been chosen split
## the training set into a train and test set so that out of sample 
## error can be computed

inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
trainsubset <- training[inTrain,]
testsubset<- training[-inTrain,]

## apply a default random forest model to the training suset
## predict on the testing subset 
## and compute the resulting confusion matrix

model1 <- train(classe ~ ., data = trainsubset, method = "rf")

save(model1, file = "./pml/model1.rda")


