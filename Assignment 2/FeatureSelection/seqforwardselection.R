#Calculate accuracy using stratified 5 fold cross validation
stratifiedAccuracyFunc <- function(fold1,fold2,fold3,fold4,fold5, bestFeatureSet) {
  #model designed for each set
  model1 <- randomForest(diagnosis ~ ., data = fold1[,c(bestFeatureSet,length(fold1))], importance = TRUE, ntree = 500)
  model2 <- randomForest(diagnosis ~ ., data = fold2[,c(bestFeatureSet,length(fold2))], importance = TRUE, ntree = 500)
  model3 <- randomForest(diagnosis ~ ., data = fold3[,c(bestFeatureSet,length(fold3))], importance = TRUE, ntree = 500)
  model4 <- randomForest(diagnosis ~ ., data = fold4[,c(bestFeatureSet,length(fold4))], importance = TRUE, ntree = 500)
  model5 <- randomForest(diagnosis ~ ., data = fold5[,c(bestFeatureSet,length(fold5))], importance = TRUE, ntree = 500)
  #predictions on models 
  
  #1st set as test set, start modelling 2to5th sets in a fold
  accuracyFold1 <- c()
  prediction1 <- predict(model2, fold1[,c(bestFeatureSet,length(fold1))], type = 'class')
  accuracyFold1 <- cbind(accuracyFold1, mean(prediction1 == fold1$diagnosis))
  prediction2 <- predict(model3, fold1[,c(bestFeatureSet,length(fold1))], type = 'class')
  accuracyFold1 <- cbind(accuracyFold1, mean(prediction2 == fold1$diagnosis))
  prediction3 <- predict(model4, fold1[,c(bestFeatureSet,length(fold1))], type = 'class')
  accuracyFold1 <- cbind(accuracyFold1, mean(prediction3 == fold1$diagnosis))
  prediction4 <- predict(model5, fold1[,c(bestFeatureSet,length(fold1))], type = 'class')
  accuracyFold1 <- cbind(accuracyFold1, mean(prediction4 == fold1$diagnosis))
  accuracyFold1 <- mean(accuracyFold1)
  
  #2nd set as test set, start modelling 1,3,4,5th sets in a fold
  accuracyFold2 <- c()
  prediction1 <- predict(model1, fold2[,c(bestFeatureSet,length(fold2))], type = 'class')
  accuracyFold2 <- cbind(accuracyFold2, mean(prediction1 == fold2$diagnosis))
  prediction2 <- predict(model3, fold2[,c(bestFeatureSet,length(fold2))], type = 'class')
  accuracyFold2 <- cbind(accuracyFold2, mean(prediction2 == fold2$diagnosis))
  prediction3 <- predict(model4, fold2[,c(bestFeatureSet,length(fold2))], type = 'class')
  accuracyFold2 <- cbind(accuracyFold2, mean(prediction3 == fold2$diagnosis))
  prediction4 <- predict(model5, fold2[,c(bestFeatureSet,length(fold2))], type = 'class')
  accuracyFold2 <- cbind(accuracyFold2, mean(prediction4 == fold2$diagnosis))
  accuracyFold2 <- mean(accuracyFold2)
  
  #3rd set as test set, start modelling 1,2,4,5th sets in a fold
  accuracyFold3 <- c()
  prediction1 <- predict(model1, fold3[,c(bestFeatureSet,length(fold3))], type = 'class')
  accuracyFold3 <- cbind(accuracyFold3, mean(prediction1 == fold3$diagnosis))
  prediction2 <- predict(model2, fold3[,c(bestFeatureSet,length(fold3))], type = 'class')
  accuracyFold3 <- cbind(accuracyFold3, mean(prediction2 == fold3$diagnosis))
  prediction3 <- predict(model4, fold3[,c(bestFeatureSet,length(fold3))], type = 'class')
  accuracyFold3 <- cbind(accuracyFold3, mean(prediction3 == fold3$diagnosis))
  prediction4 <- predict(model5, fold3[,c(bestFeatureSet,length(fold3))], type = 'class')
  accuracyFold3 <- cbind(accuracyFold3, mean(prediction4 == fold3$diagnosis))
  accuracyFold3 <- mean(accuracyFold3)
  
  #4th set as test set, start modelling 1,2,3,5th sets in a fold
  accuracyFold4 <- c()
  prediction1 <- predict(model1, fold4[,c(bestFeatureSet,length(fold4))], type = 'class')
  accuracyFold4 <- cbind(accuracyFold4, mean(prediction1 == fold4$diagnosis))
  prediction2 <- predict(model2, fold4[,c(bestFeatureSet,length(fold4))], type = 'class')
  accuracyFold4 <- cbind(accuracyFold4, mean(prediction2 == fold4$diagnosis))
  prediction3 <- predict(model3, fold4[,c(bestFeatureSet,length(fold4))], type = 'class')
  accuracyFold4 <- cbind(accuracyFold4, mean(prediction3 == fold4$diagnosis))
  prediction4 <- predict(model5, fold4[,c(bestFeatureSet,length(fold4))], type = 'class')
  accuracyFold4 <- cbind(accuracyFold4, mean(prediction4 == fold4$diagnosis))
  accuracyFold4 <- mean(accuracyFold4)
  
  #5th set as test set, start modelling 1,2,3,4th sets in a fold
  accuracyFold5 <- c()
  prediction1 <- predict(model1, fold5[,c(bestFeatureSet,length(fold5))], type = 'class')
  accuracyFold5 <- cbind(accuracyFold5, mean(prediction1 == fold5$diagnosis))
  prediction2 <- predict(model2, fold5[,c(bestFeatureSet,length(fold5))], type = 'class')
  accuracyFold5 <- cbind(accuracyFold5, mean(prediction2 == fold5$diagnosis))
  prediction3 <- predict(model3, fold5[,c(bestFeatureSet,length(fold5))], type = 'class')
  accuracyFold5 <- cbind(accuracyFold5, mean(prediction3 == fold5$diagnosis))
  prediction4 <- predict(model4, fold5[,c(bestFeatureSet,length(fold5))], type = 'class')
  accuracyFold5 <- cbind(accuracyFold5, mean(prediction4 == fold5$diagnosis))
  accuracyFold5 <- mean(accuracyFold5)
  
  FinalAccuracy <- max(accuracyFold1,accuracyFold2,accuracyFold3,accuracyFold4,accuracyFold5)
  return(FinalAccuracy)
  
}
#Sequential forward selection function
#dataX : input dataset
seqforwardselection <- function(dataX)
{
  data = read.csv(dataX)
  #for 5 fold cross validation make k as 5
  k = 5
  dataclassM = c()
  dataclassB = c()
  #get the data according to class labels(last column in dataset)
  for(i in 1:length(data[,1])){
    stringlabel = as.character(data[i,ncol(x = data)])
    if(stringlabel == 'M'){
      dataclassM = rbind(dataclassM, data[i,])
    } else if(stringlabel == 'B'){
      dataclassB = rbind(dataclassB, data[i,])
    }
  }
  # for 5 fold cross validation lets get the total instances per fold
  total_instancesPerfold = length(data[,1])/k
  #to get the stratified folds, we will calculate the percentage of each class labels present
  #in whole dataset, and we will accordingly put that ratio in each fold, 
  #which will be maintained across all 5 folds.
  classMpercentage = length(dataclassM[,1])/k
  classBprecentage = length(dataclassB[,1])/k
  #creating sets in for each fold without replacement
  set.seed(1234)
  #stratified classification with equal proportion of class labels across all folds
  #1st set in a fold
  fold1M = dataclassM[sample(x = nrow(dataclassM), size = classMpercentage, replace = FALSE),]
  dataclassM <- dataclassM[!(dataclassM %in% fold1M)]
  fold1B = dataclassB[sample(x = nrow(dataclassB), replace = FALSE, size = classBprecentage),]
  dataclassB <- dataclassB[!(dataclassB %in% fold1B)]
  fold1 <- rbind(fold1M,fold1B)
  #2nd set in a fold
  fold2M = dataclassM[sample(x = nrow(dataclassM), size = classMpercentage, replace = FALSE),]
  dataclassM <- dataclassM[!(dataclassM %in% fold2M)]
  fold2B = dataclassB[sample(x = nrow(dataclassB), replace = FALSE, size = classBprecentage),]
  dataclassB <- dataclassB[!(dataclassB %in% fold2B)]
  fold2 <- rbind(fold2M,fold2B)
  #3rd set in a fold 
  fold3M = dataclassM[sample(x = nrow(dataclassM), size = classMpercentage, replace = FALSE),]
  dataclassM <- dataclassM[!(dataclassM %in% fold3M)]
  fold3B = dataclassB[sample(x = nrow(dataclassB), replace = FALSE, size = classBprecentage),]
  dataclassB <- dataclassB[!(dataclassB %in% fold3B)]
  fold3 <- rbind(fold3M,fold3B)
  #4th set in a fold
  fold4M = dataclassM[sample(x = nrow(dataclassM), size = classMpercentage, replace = FALSE),]
  dataclassM <- dataclassM[!(dataclassM %in% fold4M)]
  fold4B = dataclassB[sample(x = nrow(dataclassB), replace = FALSE, size = classBprecentage),]
  dataclassB <- dataclassB[!(dataclassB %in% fold4B)]
  fold4 <- rbind(fold4M,fold4B)
  #5th set in a fold
  fold5M = dataclassM[sample(x = nrow(dataclassM), size = classMpercentage, replace = FALSE),]
  dataclassM <- dataclassM[!(dataclassM %in% fold5M)]
  fold5B = dataclassB[sample(x = nrow(dataclassB), replace = FALSE, size = classBprecentage),]
  dataclassB <- dataclassB[!(dataclassB %in% fold5B)]
  fold5 <- rbind(fold5M,fold5B)
  #supervised learning algorithm
  library(randomForest)
  #create models using random forest
  #hyperparameters used : number of trees to be 500
  #feature set combination to be used in random forest 
  bestFeatureSet <- c()
  bestFeature <- c()
  #accuracy on whole dataset
  accuracyBestonWholeData <- 0
  #accuracy of previousloop
  previousAccuracy <- 0
  numberOfColumnsInData <- length(data[1,])-1
  #remaining features that are still not selected as best feature
  remainingFeatures <- c(1:numberOfColumnsInData)
  #final feature set that gives the maximum performance on dataset
  bestFeatureSetFinal <- c()
  i <- 1
  #if previous loop's accuracy is greater than accuracy in current loop, break the while loop
  while(accuracyBestonWholeData >= previousAccuracy){
    #features to be selected from remaining feature set
      for(j in remainingFeatures) {
        #i is the feature column for selection
        bestFeature <- j
        bestFeatureSet[i] <- bestFeature
        #accuracy calculation through stratified cross fold validation
        accuracyBestonWholeData = stratifiedAccuracyFunc(fold1,fold2,fold3,fold4,fold5, bestFeatureSet)
        accuracyBestonWholeData = accuracyBestonWholeData*100
        if(accuracyBestonWholeData > previousAccuracy){
          bestFeatureSetFinal[i] <- j
          previousAccuracy = accuracyBestonWholeData
        }
      }
      i <- i + 1
      bestFeatureSet <- bestFeatureSetFinal
      lastFeatureSelected <- bestFeatureSetFinal[length(bestFeatureSetFinal)]
      #if last feature selected is not there in remaining feature set, 
      # that means there is no feature has been selected in this loop
      if(lastFeatureSelected %in% remainingFeatures){
        remainingFeatures <- remainingFeatures[!remainingFeatures %in% lastFeatureSelected]
        accuracyBestonWholeData <- previousAccuracy
      }
  }
  #print the best selected features
  print("Feature selected are as follows: ")
  print(bestFeatureSetFinal)
}

#Pass the dataset as an input argument to the function
seqforwardselection('/Users/Komal Barge/Documents/R/assignment2/cancerdata.csv')
