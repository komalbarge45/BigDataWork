#Kindly Enter an absolute file path for downloaded csv file.
# To run the program : kindly run each line till the end
# which can be done by using ctrl+enter on each line, 
# You don't have to change your cursor position while using this shortcut.
# Just keep on pressing Ctrl+Enter at the sam time.

HeartDisease <- readxl::read_excel("C:/Masters/Big Data/Assignments/Assignment1/HeartDisease.xlsx", col_names = FALSE)
HeartOriginalData <- HeartDisease[1:100,]

#Missing value generator function with parameters : 
#HeartData : data, percentageMissValue: percentage missingness that needs
#to be generated in data, featureNo: feature that needs missingness
createMissingValues<-function(HeartData, percentageMissValue, featureNo){
  is.na(HeartData[sample(1:nrow(HeartData),percentageMissValue*nrow(HeartData)),featureNo])<-TRUE
  col_NA = which(is.na(HeartData[,featureNo]))
  return(col_NA)
}
#To find Euclidean distance :
#Paramters: HeartData: data, j: missing value row number, i : other data point row number
#f1,f2: other feature column numbers from where distance needs to be calculated.
euclidean_dist<-function(HeartData,j,i, f1, f2){
  return(sqrt( (HeartData[j,f1] - HeartData[i,f1])^2 + 
                 (HeartData[j,f2] - HeartData[i,f2])^2 ))
}
#To find Manhattan distance :
#Paramters: HeartData: data, j: missing value row number, i : other data point row number
#f1,f2: other feature column numbers from where distance needs to be calculated.
manhattan_dist<-function(HeartData,j,i, f1, f2){
  return(abs(HeartData[j,f1] - HeartData[i,f1]) + 
           abs(HeartData[j,f2] - HeartData[i,f2]))
}
#To find Minkowski distance :
#Paramters: HeartData: data, j: missing value row number, i : other data point row number
#f1,f2: other feature column numbers from where distance needs to be calculated.
minkowski_dist<-function(HeartData,j,i, f1, f2){
  return(( abs(HeartData[j,f1] - HeartData[i,f1])^3 + 
             abs(HeartData[j,f2] - HeartData[i,f2])^3 )^(1/3))
}
# One NN imputation method :
#Parameters: HeartData: data, col_NA : row number with missing value,
#distanceMeasure: which which distance measure you wnat to calculate the distance
#distanceMeasure values can be Euclidean - "euc", Manhattan - "man", Minkowski - "mink"
#fOp:column number with missing value,
#f1,f2: other feature column numbers from where distance needs to be calculated.
oneNN<-function(HeartData, col_NA, distanceMeasure, fOp, f1, f2){
  mainDataVector <- c()
  for(j in col_NA){
    dist <- 10000;
    for (i in 1:100) {
      if(!(i %in% col_NA)) {
        delta <- 10000;
        if(distanceMeasure == "euc") {
          delta <- euclidean_dist(HeartData, j, i, f1, f2)
        } else if(distanceMeasure == "man") {
          delta <- manhattan_dist(HeartData, j, i, f1, f2)
        } else {
          delta <- minkowski_dist(HeartData, j, i, f1, f2)
        }
        if(delta <= dist) {
          #column 1 is for data value to be replaced
          value = HeartOriginalData[i,fOp]
          dist <- delta
        }
      }
    }
    mainDataVector <- c(mainDataVector, value)
  }
  return(mainDataVector)
}
#To calculate mode
#x - a vector in which mode needs to be found
calculateMode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
# To find distances method :
#Parameters: HeartData: data, j: row number of missing value,
#col_NA : row number with missing value,
#distanceMeasure: which which distance measure you wnat to calculate the distance
#distanceMeasure values can be Euclidean - "euc", Manhattan - "man", Minkowski - "mink"
#f1,f2: other feature column numbers from where distance needs to be calculated.
getDistances<-function(HeartData, j, col_NA, distanceMeasure, f1, f2){
  distances<-data.frame()
  distances <- unname(distances)
  dist <- 10000;
  for (i in 1:100) {
    if(!(i %in% col_NA)) {
      if(distanceMeasure == "euc") {
        delta <- euclidean_dist(HeartData, j, i, f1, f2)
      } else if(distanceMeasure == "man") {
        delta <- manhattan_dist(HeartData, j, i, f1, f2)
      } else {
        delta <- minkowski_dist(HeartData, j, i, f1, f2)
      }
      vectorbinding = c(i,delta)
      vectorbinding<-unname(vectorbinding)
      distances <- rbind(distances, vectorbinding)
    }
  }
  return(distances)
}
# k-NN imputation method :
#Parameters: HeartData: data, col_1_NA : row number with missing value,
#distanceMeasure: which which distance measure you wnat to calculate the distance
#distanceMeasure values can be Euclidean - "euc", Manhattan - "man", Minkowski - "mink"
#fOp:column number with missing value,
#f1,f2: other feature column numbers from where distance needs to be calculated.
kNN<-function(HeartData, col_1_NA, distanceMeasure, k, fOp, f1, f2){
  knnPredictedMatrix <- c()
  for(j in col_1_NA){
    distances_Dataset <- data.frame()
    distances_Dataset <- getDistances(HeartData, j, col_1_NA, distanceMeasure, f1, f2)
    names(distances_Dataset)[1] <- "c1"
    names(distances_Dataset)[2] <- "c2"
    distances_Dataset <- distances_Dataset[order(distances_Dataset$c2),]
    distances_Dataset <- distances_Dataset[1:k,]
    if(fOp != 2){
      dataValue=0
      for(counter in 1:k){
        #featureno
        dataValue <- dataValue + HeartOriginalData[distances_Dataset[counter,1],fOp]
      }
      knnPredictedMatrix <- c(knnPredictedMatrix, dataValue/k)
    }
    else { ##for categorical values
      dataValue <- c()
      for(counter in 1:k){
        dataValue <- c(dataValue, HeartOriginalData[distances_Dataset[counter,1],fOp])
      }
      knnPredictedMatrix<-c(knnPredictedMatrix, calculateMode(dataValue))
    }
  }
  return(knnPredictedMatrix)
}

#Weighted k-NN imputation method :
#Parameters: HeartData: data, col_1_NA : row number with missing value,
#distanceMeasure: which which distance measure you wnat to calculate the distance
#distanceMeasure values can be Euclidean - "euc", Manhattan - "man", Minkowski - "mink"
#fOp:column number with missing value,
#f1,f2: other feature column numbers from where distance needs to be calculated.
weight_kNN<-function(HeartData, col_1_NA, distanceMeasure, k,fOp, f1, f2){
  weightknnPredict <- c()
  weightknnPredict <- unname(weightknnPredict)
  for(j in col_1_NA){
    distances_Dataset <- data.frame()
    ##index and distance values
    distances_Dataset <- getDistances(HeartData, j, col_1_NA, distanceMeasure, f1, f2)
    names(distances_Dataset)[1] <- "c1"
    names(distances_Dataset)[2] <- "c2"
    distances_Dataset <- distances_Dataset[order(distances_Dataset$c2),]
    distances_Dataset <- distances_Dataset[1:k,]
    #datavalue consists of wieghted values
    if(fOp != 2){ ##for continuous values
      dataValue <-0
      for(counter in 1:k){
        #featureno
        if (round(distances_Dataset[counter,2]) != 0){
          dataValue = dataValue + HeartOriginalData[distances_Dataset[counter,1],fOp]/distances_Dataset[counter,2]
        }  else {
          dataValue = dataValue + HeartOriginalData[distances_Dataset[counter,1],fOp]
        }
      }
      weightknnPredict <- c(weightknnPredict, dataValue/k)
    } else { ##for categorical values
      dataValue <- c()
      for(counter in 1:k){
        #dataValue <- c(dataValue, HeartOriginalData[distances_Dataset[counter,1],fOp])
        data0 <- 0
        data1 <- 0
        if(HeartOriginalData[distances_Dataset[counter,1],fOp] == 0){
          data0 = data0 + distances_Dataset[counter,2]
        } else {
          data1 = data1 + distances_Dataset[counter,2]
        }
      }
      if(data0 > data1) {
        weightknnPredict<-c(weightknnPredict, 0)
      } else {
        weightknnPredict<-c(weightknnPredict, 1)
      }
    }
  }
  return(weightknnPredict)
}
# Min-Max Normalization feature scaling method
#xval - a vector which needs to be normalized
normalize <- function(xval) {
  return ((xval - min(xval)) / (max(xval) - min(xval)))
}
# Mean Normalization feature scaling method
#xval - a vector which needs to be normalized
MeanNormalize <- function(xval) {
  return ((xval - sum(xval)/length(xval)) / (max(xval) - min(xval)))
}
#Main feature operation performing method
#missingValuePercentage - missing percentage value that needs to be generated
#featureOperated - feature in which missing values will be generated
#f2,f3 - other feature column numbers from where distance needs to be calculated.
#scalingFactor - whether data needs to be scaled or not
# scalingFactor will have values - NoScale: no scaling, 
#Min_MaxScale: Min-Max Normalization scaling measure, 
#MeanNormScale: Mean Normalization scaling measure 
featureFunction <- function(missingValuePercentage, featureOperated, f2, f3, scalingFactor){
  #create missing values in feature 1
  
  HeartUpdatedData_0.05per <- HeartDisease[1:100,]
  if(scalingFactor != "Min_MaxScale") {
    HeartUpdatedData_0.05per[,f3] <- normalize(HeartUpdatedData_0.05per[,f3])
  } else if(scalingFactor == "MeanNormScale") {
    HeartUpdatedData_0.05per[,f3] <- MeanNormalize(HeartUpdatedData_0.05per[,f3])
  }
  vector <- c()
  #Generate NA missing values in columns
  feature1ColumnIndex_NA <- createMissingValues(HeartUpdatedData_0.05per,missingValuePercentage,featureOperated)
  #put original values in data frame for accuracy comparison
  for(val in feature1ColumnIndex_NA) {
    vector <- c(vector, HeartOriginalData[val,featureOperated])
  }
  
  kval <- 5
  df <- data.frame()
  df <- unname(df)
  vector <- unname(vector)
  df <- rbind(df, vector)
  
  if(featureOperated != 2){
    oneNN_Index_Euclidean <- oneNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA,"euc",featureOperated, f2, f3)
    oneNN_Index_Manhattan <- oneNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA,"man",featureOperated, f2, f3)
    
    knn_Index_Euclidean <- kNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA,"euc",kval, featureOperated, f2, f3)
    knn_Index_Manhattan <- kNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA,"man",kval, featureOperated, f2, f3)
    
    weighted_knn_Euclidean <- weight_kNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA, "euc",kval, featureOperated, f2, f3)
    weighted_knn_Manhattan <- weight_kNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA, "man",kval, featureOperated, f2, f3)
    
    oneNN_Index_Euclidean <- unname(oneNN_Index_Euclidean)
    oneNN_Index_Manhattan <- unname(oneNN_Index_Manhattan)
    knn_Index_Euclidean <- unname(knn_Index_Euclidean)
    knn_Index_Manhattan <- unname(knn_Index_Manhattan)
    weighted_knn_Euclidean <- unname(weighted_knn_Euclidean)
    weighted_knn_Manhattan <- unname(weighted_knn_Manhattan)
    df <- rbind(df, oneNN_Index_Euclidean)
    df <- rbind(df, oneNN_Index_Manhattan)
    df <- rbind(df, knn_Index_Euclidean)
    df <- rbind(df, knn_Index_Manhattan)
    df <- rbind(df, weighted_knn_Euclidean)
    df <- rbind(df, weighted_knn_Manhattan)
  } else {
    oneNN_Index_Minkowski_2 <- oneNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA,"mink", featureOperated, f2, f3)
    knn_Index_Minkowski_2 <- kNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA,"mink",kval, featureOperated, f2, f3)
    weighted_knn_Minkowski_2 <- weight_kNN(HeartUpdatedData_0.05per, feature1ColumnIndex_NA, "mink",kval, featureOperated, f2, f3)
    
    oneNN_Index_Minkowski_2 <- unname(oneNN_Index_Minkowski_2)
    knn_Index_Minkowski_2 <- unname(knn_Index_Minkowski_2)
    weighted_knn_Minkowski_2 <- unname(weighted_knn_Minkowski_2)
    df <- rbind(df, oneNN_Index_Minkowski_2)
    df <- rbind(df, knn_Index_Minkowski_2)
    df <- rbind(df, weighted_knn_Minkowski_2)
    
  }
  
  return(df)
}
#calculate accuracy for continuous feature
#df-dataframe for original and predicted values that are generated for all imputation methods
calculateAccuracyContinuous<-function(df){
  output <- data.frame()
  output <- unname(output)
  for(i in 1:length(df[,1])-1){
    output <- rbind(output, abs(sum(1-(abs(df[i+1,]-df[1,])/df[1,]))/length(df[1,])))
  }
  return(output)
}

#calculate accuracy for categorical feature
#df-dataframe for original and predicted values that are generated for all imputation methods
calculateAccuracyCategorical<-function(df){
  output <- data.frame()
  output <- unname(output)
  for(i in 1:length(df[,1])-1){
    output <- rbind(output, abs(1-sum(abs(df[i+1,]-df[1,]))/length(df[1,])))
  }
  return(output)
}
#Main function performing all function calls 
#to calculate the accuracy over all missingness and all imputation method
#accuracy results will be appeneded to results.csv file which will be generated under Documents/ folder
#results file path: C:/Users/<Your Username>/Documents
accuracyMeasure <- function(){
  outputAccuracy1 <- data.frame()
  dataframe1 <- featureFunction(0.05,1,2,3,"NoScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataframe1)
  rownames(outputAccuracy1)<-c("TargetAccuracy_1","oneNN_Euclidean_0.05_1","oneNN_Manhattan_0.05_1",
                               "kNN_Euclidean_0.05_1", "kNN_Manhattan_0.05_1",
                               "weightedkNN_Euclidean_0.05_1","weightedkNN_Manhattan_0.05_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=FALSE,col.names = FALSE)
  
  #For feature 2 with 0.05 percentage
  dataframe2 <- featureFunction(0.05,2,1,3,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe2)
  rownames(outputAccuracy1)<-c("TargetAccuracy_2","oneNN_minkowski_0.05_2",
                               "kNN_minkowski_0.05_2",
                               "weightedkNN_minkowski_0.05_2")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataframe3 <- featureFunction(0.05,3,1,2,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe3)
  rownames(outputAccuracy1)<-c("TargetAccuracy_3","oneNN_Euclidean_0.05_3","oneNN_Manhattan_0.05_3",
                               "kNN_Euclidean_0.05_3", "kNN_Manhattan_0.05_3",
                               "weightedkNN_Euclidean_0.05_3","weightedkNN_Manhattan_0.05_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataframe4 <- featureFunction(0.1,1,2,3,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe4)
  rownames(outputAccuracy1)<-c("TargetAccuracy_4","oneNN_Euclidean_0.1_1","oneNN_Manhattan_0.1_1",
                               "kNN_Euclidean_0.1_1", "kNN_Manhattan_0.1_1",
                               "weightedkNN_Euclidean_0.1_1","weightedkNN_Manhattan_0.1_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataframe5 <- featureFunction(0.1,2,1,3,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe5)
  rownames(outputAccuracy1)<-c("TargetAccuracy_5","oneNN_minkowski_0.1_2",
                               "kNN_minkowski_0.1_2",
                               "weightedkNN_minkowski_0.1_2")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataframe6 <- featureFunction(0.1,3,1,2,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe6)
  rownames(outputAccuracy1)<-c("TargetAccuracy_6","oneNN_Euclidean_0.1_3","oneNN_Manhattan_0.1_3",
                               "kNN_Euclidean_0.1_3", "kNN_Manhattan_0.1_3",
                               "weightedkNN_Euclidean_0.1_3","weightedkNN_Manhattan_0.1_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  
  dataframe7 <- featureFunction(0.2,1,2,3,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe7)
  rownames(outputAccuracy1)<-c("TargetAccuracy_7","oneNN_Euclidean_0.2_1","oneNN_Manhattan_0.2_1",
                               "kNN_Euclidean_0.2_1", "kNN_Manhattan_0.2_1",
                               "weightedkNN_Euclidean_0.2_1","weightedkNN_Manhattan_0.2_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataframe8 <- featureFunction(0.2,2,1,3,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe8)
  rownames(outputAccuracy1)<-c("TargetAccuracy_8","oneNN_minkowski_0.2_2",
                               "kNN_minkowski_0.2_2",
                               "weightedkNN_minkowski_0.2_2")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataframe9 <- featureFunction(0.2,3,1,2,"NoScale")
  outputAccuracy1 <- calculateAccuracyCategorical(dataframe9)
  rownames(outputAccuracy1)<-c("TargetAccuracy_9","oneNN_Euclidean_0.2_3","oneNN_Manhattan_0.2_3",
                               "kNN_Euclidean_0.2_3", "kNN_Manhattan_0.2_3",
                               "weightedkNN_Euclidean_0.2_3","weightedkNN_Manhattan_0.2_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale10 <- featureFunction(0.05,1,2,3,"Min_MaxScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale10)
  rownames(outputAccuracy1)<-c("TargetAccuracy_10","oneNN_Euclidean_0.05_mm_sc_1","oneNN_Manhattan_0.05_mm_sc_1",
                               "kNN_Euclidean_0.05_mm_sc_1", "kNN_Manhattan_0.05_mm_sc_1",
                               "weightedkNN_Euclidean_0.05_mm_sc_1","weightedkNN_Manhattan_0.05_mm_sc_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale11 <- featureFunction(0.05,3,2,1,"Min_MaxScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale11)
  rownames(outputAccuracy1)<-c("TargetAccuracy_11","oneNN_Euclidean_0.05_mm_Sc_3","oneNN_Manhattan_0.05_mm_sc_3",
                               "kNN_Euclidean_0.05_mm_sc_3", "kNN_Manhattan_0.05_mm_sc_3",
                               "weightedkNN_Euclidean_0.05_mm_sc_3","weightedkNN_Manhattan_0.05_mm_sc_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale12 <- featureFunction(0.1,1,2,3,"Min_MaxScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale12)
  rownames(outputAccuracy1)<-c("TargetAccuracy_12","oneNN_Euclidean_0.1_mm_sc_1","oneNN_Manhattan_0.1_mm_sc_1",
                               "kNN_Euclidean_0.1_mm_sc_1", "kNN_Manhattan_0.1_mm_sc_1",
                               "weightedkNN_Euclidean_0.1_mm_sc_1","weightedkNN_Manhattan_0.1_mm_sc_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale13 <- featureFunction(0.1,3,2,1,"Min_MaxScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale13)
  rownames(outputAccuracy1)<-c("TargetAccuracy_13","oneNN_Euclidean_0.1_mm_Sc_3","oneNN_Manhattan_0.1_mm_sc_3",
                               "kNN_Euclidean_0.1_mm_sc_3", "kNN_Manhattan_0.1_mm_sc_3",
                               "weightedkNN_Euclidean_0.1_mm_sc_3","weightedkNN_Manhattan_0.1_mm_sc_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale14 <- featureFunction(0.2,1,2,3,"Min_MaxScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale14)
  rownames(outputAccuracy1)<-c("TargetAccuracy_14","oneNN_Euclidean_0.2_mm_sc_1","oneNN_Manhattan_0.2_mm_sc_1",
                               "kNN_Euclidean_0.2_mm_sc_1", "kNN_Manhattan_0.2_mm_sc_1",
                               "weightedkNN_Euclidean_0.2_mm_sc_1","weightedkNN_Manhattan_0.2_mm_sc_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale15 <- featureFunction(0.2,3,2,1,"Min_MaxScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale15)
  rownames(outputAccuracy1)<-c("TargetAccuracy_15","oneNN_Euclidean_0.2_mm_Sc_3","oneNN_Manhattan_0.2_mm_sc_3",
                               "kNN_Euclidean_0.2_mm_sc_3", "kNN_Manhattan_0.2_mm_sc_3",
                               "weightedkNN_Euclidean_0.2_mm_sc_3","weightedkNN_Manhattan_0.2_mm_sc_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale16 <- featureFunction(0.05,1,2,3,"MeanNormScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale16)
  rownames(outputAccuracy1)<-c("TargetAccuracy_16","oneNN_Euclidean_0.05_mn_sc_1","oneNN_Manhattan_0.05_mn_sc_1",
                               "kNN_Euclidean_0.05_mn_sc_1", "kNN_Manhattan_0.05_mn_sc_1",
                               "weightedkNN_Euclidean_0.05_mn_sc_1","weightedkNN_Manhattan_0.05_mn_sc_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale17 <- featureFunction(0.05,3,2,1,"MeanNormScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale17)
  rownames(outputAccuracy1)<-c("TargetAccuracy_17","oneNN_Euclidean_0.05_mn_Sc_3","oneNN_Manhattan_0.05_mn_sc_3",
                               "kNN_Euclidean_0.05_mn_sc_3", "kNN_Manhattan_0.05_mn_sc_3",
                               "weightedkNN_Euclidean_0.05_mn_sc_3","weightedkNN_Manhattan_0.05_mn_sc_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale18 <- featureFunction(0.1,1,2,3,"MeanNormScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale18)
  rownames(outputAccuracy1)<-c("TargetAccuracy_18","oneNN_Euclidean_0.1_mn_sc_1","oneNN_Manhattan_0.1_mn_sc_1",
                               "kNN_Euclidean_0.1_mn_sc_1", "kNN_Manhattan_0.1_mn_sc_1",
                               "weightedkNN_Euclidean_0.1_mn_sc_1","weightedkNN_Manhattan_0.1_mn_sc_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale19 <- featureFunction(0.1,3,2,1,"MeanNormScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale19)
  rownames(outputAccuracy1)<-c("TargetAccuracy_19","oneNN_Euclidean_0.1_mn_Sc_3","oneNN_Manhattan_0.1_mn_sc_3",
                               "kNN_Euclidean_0.1_mn_sc_3", "kNN_Manhattan_0.1_mn_sc_3",
                               "weightedkNN_Euclidean_0.1_mn_sc_3","weightedkNN_Manhattan_0.1_mn_sc_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale20 <- featureFunction(0.2,1,2,3,"MeanNormScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale20)
  rownames(outputAccuracy1)<-c("TargetAccuracy_20","oneNN_Euclidean_0.2_mn_sc_1","oneNN_Manhattan_0.2_mn_sc_1",
                               "kNN_Euclidean_0.2_mn_sc_1", "kNN_Manhattan_0.2_mn_sc_1",
                               "weightedkNN_Euclidean_0.2_mn_sc_1","weightedkNN_Manhattan_0.2_mn_sc_1")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  dataScale21 <- featureFunction(0.2,3,2,1,"MeanNormScale")
  outputAccuracy1 <- calculateAccuracyContinuous(dataScale21)
  rownames(outputAccuracy1)<-c("TargetAccuracy_21","oneNN_Euclidean_0.2_mn_Sc_3","oneNN_Manhattan_0.2_mn_sc_3",
                               "kNN_Euclidean_0.2_mn_sc_3", "kNN_Manhattan_0.2_mn_sc_3",
                               "weightedkNN_Euclidean_0.2_mn_sc_3","weightedkNN_Manhattan_0.2_mn_sc_3")
  write.table(outputAccuracy1,file="results.csv",sep=",",append=TRUE,col.names = FALSE)
  
  return(outputAccuracy1)
}
#results may vary based on missing values, as those are randonly generated everytime.
#acc - it is the last output accuracy dataframe
acc <- accuracyMeasure()
view(acc)

