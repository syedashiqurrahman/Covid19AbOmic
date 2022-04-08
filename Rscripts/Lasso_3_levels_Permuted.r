
rm(list = ls())
cat("\014")

#3 groups?

# 0 <- healthy
# .5 <- survived
# 1 <- died

library(glmnetUtils) #cv.glmnet()
library(pROC) #auc()
library(caret) #createDataPartition(), createFolds()
library(e1071) #svm()
library(randomForest)
library(kernlab) #tuning svm train

setwd("/Users/sar210/Box/covid19/FCprofiling/")

#dataSet <- read.csv("fcProfile_Canonical_nonCanonical_Updated.csv")
# dataSet <- read.csv("fcProfile_nonCanonical_Healthy_Covid_Updated.csv")
# dataSet <- read.csv("fcProfile_Healthy_Covid_C_NC_Updated.csv")
# dataSet <- read.csv("fcProfile_Healthy_Covid_C_NC_without_FCRs_Glycans.csv")
# dataSet <- read.csv("fcProfile_Healthy_Covid_3_Flu_Updated.csv")

dataSet <- read.csv("fcProfile_Healthy_Covid_2_Flu_Updated.csv")

#healthy <- 2  survived <- 0    died <- 1
#healthy <- 0  survived <- 0.5  died <- 1

#all 0's to 0.5    then all 2's to 0
dataSet["Y"][dataSet["Y"] == 0] <- 0.5
dataSet["Y"][dataSet["Y"] == 2] <- 0


Y <- dataSet$Y
# Normalize using z score
scaled <- scale(dataSet[,-c(1,2)])
dataSet <- cbind(dataSet[,c(1,2)], scaled)


aucSVM <- c()
aucRF <- c()
replicateCV <- 10
for(cv in 1:replicateCV) 
{
  permutedY <- sample(Y)
  dataSet$Y
  permutedY
  dataSet$Y <- permutedY
  
  numk <- 10
  
  folds <- createFolds(dataSet[,"Y"], k=numk, list=FALSE, returnTrain = FALSE) #n folds with row indexes to be included in fold
  trainFolds <- cbind(folds, dataSet)
  
  repl.Y <- c()
  repl.svm.Y <- c()
  repl.rf.Y <- c()
  numVarsUsed <- c()
  
  #Each fold will have different set of variables
  
  for(n in 1:numk)
  {
    tempTest <- trainFolds[which(trainFolds$folds==n), ] #nth fold #! for names & - for col indexes
    tempTrain <- trainFolds[which(trainFolds$folds!=n),] #all folds except nth fold #need matrix format
    
    cv.lasso <- cv.glmnet(x=as.matrix(tempTrain[,!colnames(tempTrain) %in% c("folds","SampleID","Y")]), y=tempTrain[,"Y"],alpha=1, nfolds=10)
    lambda <- cv.lasso$lambda.min   #lambda <- cv.lasso$lambda.1se 
    lasso <- glmnet(x=as.matrix(tempTrain[,!colnames(tempTrain) %in% c("folds","SampleID","Y")]), y=tempTrain[,"Y"], alpha=1, lambda = lambda*.85)
    #6/7/10 features lambda 0.7  
    
    
    allVars <- data.frame(data.matrix(lasso[["beta"]]))
    selectedVars <- row.names(subset(allVars, s0!=0.000000e+00))
    vars <- c()
  
    if (length(selectedVars)==1) ##If no variables
      {
      data <- tempTrain[,!colnames(tempTrain) %in% c("folds","SampleID","Y")]
      randomSelect <- sample(ncol(data), 3) ##
      vars <- sapply(randomSelect, function(x){vars <- append(colnames(data)[x], vars) }) ##
      selectedVars <- vars
     }
    
    numVarsUsed <- append(numVarsUsed, length(selectedVars))
    
    tempVarsTrain <- data.frame(cbind(tempTrain[,c("Y")],tempTrain[,colnames(tempTrain) %in% selectedVars]))
    tempVarsTest <- data.frame(cbind(tempTest[,c("Y")] ,tempTest[,colnames(tempTest) %in% selectedVars]))
    
    colnames(tempVarsTrain)[1] <- "Y"
    colnames(tempVarsTest)[1] <- "Y"
    
    repl.Y <- append(repl.Y, tempVarsTest$Y)
    
    #svm
  
    #beforehand tune; cost=10/100; kernel: radial/linear; make scale true if not normalized
    
    svm.model <- svm(Y ~., data=tempVarsTrain, kernal = "linear", cost=10, scale=FALSE)
    svm.predY <- predict(svm.model, newdata = tempVarsTest[,-1])
    repl.svm.Y <- append(repl.svm.Y, svm.predY)
    
    #rf
    rf.model <- randomForest(Y~.,data=tempVarsTrain, ntree=100, importance = TRUE)
    rf.predY <- predict(rf.model, newdata = tempVarsTest[,-1])
    repl.rf.Y <- append(repl.rf.Y, rf.predY)
  
  }

#aucSVM[cv] <- auc(repl.Y, repl.svm.Y)
aucSVM[cv] <- (multiclass.roc(repl.Y, repl.svm.Y, levels=c(0, .5, 1)))$auc
aucRF[cv] <- (multiclass.roc(repl.Y, repl.rf.Y, levels=c(0, .5, 1)))$auc
#aucRF[cv] <- auc(repl.Y, repl.rf.Y)

}

# numVarsUsed
aucSVM
aucRF

median(aucSVM)
median(aucRF)

# write.csv(aucRF, "AUC_Lasso_RF_ETP_fcProfile_3levels_Permuted.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_ETP_fcProfile_3levels_Permuted.csv")

# write.csv(aucRF, "AUC_Lasso_RF_fcProfile_3levels_without_FCRs_Glycans_Permuted.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_ETP_fcProfile_3levels_without_FCRs_Glycans_Permuted.csv")

# write.csv(aucRF, "AUC_Lasso_RF_fcProfile_3levels_3Flu_Permuted.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_fcProfile_3levels_3Flu_Permuted.csv")

# write.csv(aucRF, "AUC_Lasso_RF_fcProfile_3levels_2Flu_Permuted.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_fcProfile_3levels_2Flu_Permuted.csv")
