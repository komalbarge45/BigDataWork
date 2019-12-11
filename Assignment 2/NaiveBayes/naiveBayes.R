#Naive Bayes algorithm

#prediction for test dataset
predictFunction <- function(featureArray, testset) {
  #Total number of positive and negative instances in test dataset
  Total_positive_instances = featureArray[[9]]$positive[2]
  Total_negative_instances = featureArray[[9]]$negative[1]
  probabilityArray <- c()
  for( i in 1:length(testset[,1])){
    prob_pos_perfeature = Total_positive_instances/(Total_positive_instances+Total_negative_instances)
    prob_neg_perfeature = Total_negative_instances/(Total_positive_instances+Total_negative_instances)
    probabilityOfInstance = 0
    for(j in 1:length(featureArray)){
      ##length of feature array is 9
      featurewiseProbability <- testset[i,j]
      #m is the number of possible values of a feature
      m = length(unique(featureArray[[j]]$Var1))
      p = 1/m
      #count of each feature value on positive target
      index_Positive = featureArray[[j]]$positive[which(featureArray[[j]]$Var1 == featurewiseProbability)] + (m*p)
      #count of each feature value on negative target
      index_negative = featureArray[[j]]$negative[which(featureArray[[j]]$Var1 == featurewiseProbability)] + (m*p)
      #probability of each feature value on positive target
      prob_pos_perfeature = prob_pos_perfeature * index_Positive/(Total_positive_instances + m)
      #probability of each feature value on negative target
      prob_neg_perfeature = prob_neg_perfeature * index_negative/(Total_negative_instances + m)
    }
    #whichever target probability is maximum, 
    #assign that target value as an output to a test instance
    if(prob_neg_perfeature > prob_pos_perfeature){
      probabilityOfInstance = 'Negative'
    } else{
      probabilityOfInstance = 'Positive'
    }
    #predictions array for test dataset
    probabilityArray <- rbind(probabilityArray, probabilityOfInstance,deparse.level = 0)
  }
  return(probabilityArray)
}

# Naive bayes function
naiveBayes <- function(train,test){
  trainingDataset = read.csv(train)
  testingDataset = read.csv(test)
  #target values are 0 and 1 - negative and positive respectively
  trainingDataset$target = as.character(trainingDataset$target)
  testingDataset$target = as.character(testingDataset$target)
  #Replace the target variables with 'Positive' and 'Negative
  #Training dataset
  for(i in 1:nrow(trainingDataset)){
    if(trainingDataset[i,9] == '0')
      trainingDataset[i,9] = 'Negative'
    else
      trainingDataset[i,9] = 'Positive'
  }
  #Testing dataset
  for(i in 1:nrow(testingDataset)){
    if(testingDataset[i,9] == '0'){
      testingDataset[i,9] = 'Negative'
    }
    else {
      testingDataset[i,9] = 'Positive'
    }
  }
  #loop with columns
  featureArray <- c()
  for(i in 1:length(trainingDataset[1,])){
    #featureArray is a list of column attributes with their unique feature values 
    #each element of a list is a data frame of attribute values, its count in a given feature, 
    #and its count per positive and negative target
    featureArray[[i]] <- as.data.frame(table(trainingDataset[,i]))
    index <- 1
    for (j in featureArray[[i]]$Var1){
      # for each element in the list count positives and negatives for each of them
      countNegative = 0
      countPositive = 0
      for(k in 1:length(trainingDataset[,1])){
        if(trainingDataset[k,i] == j && trainingDataset[k,9] == 'Positive'){
          countPositive = countPositive + 1
        }
        else if(trainingDataset[k,i] == j && trainingDataset[k,9] == 'Negative'){
          countNegative = countNegative + 1
        }
      }
      featureArray[[i]]$positive[index] = countPositive
      featureArray[[i]]$negative[index] = countNegative
      index <- index + 1
    }
  }
  pred = predictFunction(featureArray, testingDataset)
  fileName = "predictions.csv"
  filedata = cbind(testingDataset, pred)
  write.table(filedata, file=fileName, sep=",", append=FALSE, col.names = FALSE)
  
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  total_instances <- length(testingDataset[,1])
  for(z in 1:total_instances){
    if(testingDataset[z,9] == pred[z,1] && pred[z,1] == 'Positive'){
      #TRUE POSITIVE
      tp <- tp +1
    } else if(testingDataset[z,9] == pred[z,1] && pred[z,1] == 'Negative') {
      #TRUE NEGATIVE
      tn <- tn + 1
    } else if(testingDataset[z,9] != pred[z,1] && testingDataset[z,9] == 'Positive'){
      #FALSE NEGATIVE
      fn <- fn + 1
    } else if(testingDataset[z,9] != pred[z,1] && testingDataset[z,9] == 'Negative') {
      #FALSE POSITIVE
      fp <- fp + 1
    }
  }
  #calculation of accuracy
  accuracy <- (tp + tn)/total_instances
  print("Accuracy:")
  print(accuracy)
  #calculation of sensitivity - true positive rate
  sensitivity <- tp/(tp+fn)
  print("Sensitivity:")
  print(sensitivity)
  #calculation of specificity - true negative rate
  specificity <- tn/(tn+fp)
  print("Specificity:")
  print(specificity)
}

#Pass training and testing datasets as an input argument to a program
naiveBayes('/Users/Komal Barge/Documents/R/assignment2/heartTrainingDataset.csv','/Users/Komal Barge/Documents/R/assignment2/heartTestingDataset.csv')
