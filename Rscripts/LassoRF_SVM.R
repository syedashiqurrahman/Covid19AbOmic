
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/")
# setwd("/Users/syedashiqurrahman/Box/covid19/FCprofiling/")


library(e1071)
library(caret)
library(glmnet)
library(randomForest)
library(gbm)
library(matrixStats)
library(readxl)
library(cvAUC)
library(pROC)
library(DMwR2)
library(tidyverse)

# df <- read.csv("fcProfile_Clinical_C_NC_combined.csv")
# data <- df[, c(6:166)]
# imputedDF <- knnImputation(data[,-1], k=2)
# for(i in 1:ncol(data)){
#   data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
# }

data <- read.csv("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/ReviewResponse/fcProfile_Clinical_matched.csv")
data <- data[, -c(1:5, 27:74)]

calc_mode <- function(x){
  distinct_values <- unique(x)
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}
data %>% 
  mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

Y <- data[,1]
# threshold <- 50
# data <- data[, !sapply(data, function(x) mean(x)) < threshold] 
# data <- cbind.data.frame(Y, data)


scaledData <- scale(data[,-1])

Y <- data[,1]
data <- cbind.data.frame(Y, scaledData)
k <- 10
# k <- nrow(data) - 1
ACC <- c()
aucRF <- c()
auctree <- c()
aucSVM <- c()

dF <- c(1:nrow(data))

# replicateCV <- 1
for (replicateCV in 1:10)
{
  
    folds <- createFolds(y = data$Y, k=k, list = FALSE, returnTrain = FALSE)
    myData <- cbind(data, folds)
    # FOLDS[replicateCV, ] <- folds
    
    Y <- c()
    append.RF <- c()
    append.SVM <- c()
    append.tree <- c()
    append.lasso <- c()
    selectedVars <- c()
    
    # NoF <- 1
    for (NoF in 1:k)
    {
      
      fold = which(folds == NoF)
      
      train <- myData[-fold, ]
      test <- myData[fold, ]
      
      train <- train[, -ncol(train)]
      test <- test[, -ncol(test)]
      
      
      X = as.matrix(train[, -1])
      y = train$Y
  
      glmnet1 <- cv.glmnet(X, y=train$Y, alpha=1, nfolds = k)
      
      lambda <- glmnet1$lambda.min
      # lambda <- glmnet1$lambda.1se
      lambda <- lambda*0.75
      
      glmnet2 <- glmnet(X, y=train$Y, alpha=1, lambda = lambda)
      c <- coef(glmnet2)
      
      inds<-which(c!=0)
      variables<-row.names(c)[inds]
      len <- length(variables)
      
      if (len == 1)
      {
        randomSelect <- sample(ncol(data), 3)
        variables <- row.names(c)[randomSelect]
      } else
      {
        variables<-row.names(c)[inds]
        variables <- variables[2:len]
      }
      selectedVars <- append(selectedVars, len)
        
      tempTr <- train[, (names(train) %in% variables)]
      tempTrain <- cbind.data.frame(train$Y, tempTr)
      tempTr <- test[, (names(test) %in% variables)]
      tempTest <- cbind.data.frame(test$Y, tempTr)
      
      colnames(tempTrain)[1] <- "Y"
      colnames(tempTest)[1] <- "Y"
          
      Y <- append(Y, tempTest$Y)
      
      # svm
      svmfit = svm(Y~ ., data = tempTrain , kernel="linear", cost=10, scale=FALSE)
      yhat.SVM = predict(svmfit, newdata = tempTest[, -1])
      append.SVM <- append(append.SVM, yhat.SVM)

      # RF
      RFfit <- randomForest(Y~ ., data = tempTrain, importance=TRUE, ntree = 100)
      yhat.RF = predict(RFfit, newdata = tempTest[, -1])
      append.RF <- append(append.RF, yhat.RF)
      
      
    }
    
    aucRF[replicateCV] <- auc(Y, append.RF)
    aucSVM[replicateCV] <- auc(Y, append.SVM)
    
    ACC[replicateCV] <- length(which(Y == append.SVM))/nrow(data)
    
}
selectedVars

aucRF
median(aucRF)
aucSVM
median(aucSVM)


# write.csv(aucRF, "AUC_Lasso_RF_Clinical.csv")
# write.csv(aucSVM, "AUC_Lasso_SVM_Clinical.csv")


