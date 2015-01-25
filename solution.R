#To execute this R script from R command line:
#source("D:/NTU/Coursera/Practical Machine Learning/project/solution.R")
#Load Libraries: 
library(caret) #Caret package
library(rattle) #Graphical user interface for data mining in R
library(rpart) #Recursive Partitioning and Regression Trees
library(randomForest) #For the prediction using RF

#Set seed for reproducibility
set.seed(11111)

#train and test files URL:
trainDataUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testDataUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#Preprocess step 1: handle missing values by replacing "NA", "#DIV/0!", and blank "". 
#Useful reference: http://www.ats.ucla.edu/stat/r/faq/missing.htm
total_training <- read.csv(url(trainDataUrl), na.strings=c("NA","#DIV/0!",""))
final_test_set <- read.csv(url(testDataUrl), na.strings=c("NA","#DIV/0!",""))

#Data slicing: partition training data into train set and test set with ratio of 60:40
inTrain <- createDataPartition(y=total_training$classe, p=0.6, list=FALSE)
training <- total_training[inTrain,] #This set is to build the model
testing <- total_training[-inTrain,] #This set is to verify the model

#Verify the partition ratio
#nrow(training) #result in 11776
#nrow(testing) # result in 7846 
#A more comprehensive method: dim(training) and dim(testing)

#########################################################
# Cleaning the data
#########################################################
#Compute and view Near Zero Value variables. Reference: Lecture 15covariateCreation.pdf
myDataNZV <- nearZeroVar(training, saveMetrics=TRUE)
myDataNZV #view the set of NZV
#Find the set of nzv == true in myDataNZV
logical_index <- myDataNZV[,"nzv"] #30 variables results in nzv
nzv_set <-rownames(myDataNZV)[logical_index] #To view the list of variables that have NZV
#Remove nzv columns in the training data
trainingV1 <- training[!logical_index]
#Dim(trainingV1) #Results in 1170x130

#Remove first column which is ID
trainingV2 <- trainingV1[c(-1)]

#Cleaning Variables with too many NAs (60% threshold)
trainingV3 <- trainingV2 #creating another subset to iterate in loop
for(i in 1:length(trainingV2)) { #for every column in the training dataset
        if( sum( is.na( trainingV2[, i] ) ) /nrow(trainingV2) >= .6 ) { #if nº NAs > 60% of total observations
		for(j in 1:length(trainingV3)) {
			if( length( grep(names(trainingV2[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
				trainingV3 <- trainingV3[ , -j] #Remove that column
			}	
		} 
	}
}
#> dim(trainingV3)
#[1] 11776    58
#Base on this final result of the cleaning process, clean other two sets: test set from partition and test set for verification
testing<-testing[colnames(trainingV3)]
final_test_set<-final_test_set[colnames(trainingV3[,-match("classe", colnames(trainingV3))])] #Remove "classe"
#Coerced into same type
for (i in 1:length(final_test_set) ) {
        for(j in 1:length(trainingV3)) {
		if( length( grep(names(trainingV3[i]), names(final_test_set)[j]) ) ==1)  {
			class(final_test_set[j]) <- class(trainingV3[i])
		}      
	}      
}
final_test_set <- rbind(trainingV3[2, -58] , final_test_set) 
final_test_set <- final_test_set[-1,]
#########################################################
# Decision Tree
#########################################################
decisionTreeMod <- rpart(classe ~ ., data=trainingV3, method="class")
fancyRpartPlot(decisionTreeMod)
#Prediction based on the decision tree model
decisionTreePred <- predict(decisionTreeMod, testing, type = "class")
confusionMatrix(decisionTreePred, testing$classe)

#########################################################
# Random Forest
#########################################################

randForestMod <- randomForest(classe ~. , data=trainingV3)
#Prediction
randForestPred1 <- predict(randForestMod, testing, type = "class")
confusionMatrix(randForestPred1, testing$classe)
randForestPred2 <- predict(randForestMod, final_test_set, type = "class")
#Function to generate files with predictions to submit for assignment:

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0(i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(randForestPred2)
