#Calculate silhoutte width
sil_width <- function(newData, finalDistancesFromMedoids, finalMedoids, disMatrix,k){
  #newData: data with the cluster information for each point
  #finalDistancesFromMedoids: matrix of distances from medoids for all the points
  #finalMedoids: medoid points indices
  #disMatrix: dissimilarity matrix
  #k - number of clusters
  clusterPointsArray <- c()
  silhouttePerCluster <- c()
  #find a value of b : distance between each point and its next nearest cluster centroid
  # find a value of a : avg distance between point and all other points in the cluster
  for(i in 1:k){
    clusterPointsArray <- which(newData[,14] %in% i)
    silhouttePerPoint <- c()
    for(j in clusterPointsArray){
      b <- min(finalDistancesFromMedoids[j,-i])
      a <- mean(disMatrix[j, clusterPointsArray[-j]])
      silhouttePerPoint <- cbind(silhouttePerPoint, (b-a)/max(a,b))
    }
    silhouttePerCluster <- cbind(silhouttePerCluster, mean(silhouttePerPoint))
  }
  #return the average silhouetter width for the final clustering solution
  return(mean(silhouttePerCluster))
}

#algorithm for k medoids function
#dataset file and k number of clusters
kmedoids <- function(datafile, k){
  # kmedoids functions takes dataset as a datafile and 
  # number of clusters to be formed as a value of k
  set.seed(1234)
  dataset <- read.csv(datafile)
  #calculate dissimilarity matrix
  dismatrix <- as.matrix(daisy(x = dataset))
  #random medoids to be chosen
  medoids <- dataset[sample(nrow(dataset), k),]
  #row numbers of the random medoids
  rowNumMedoids <- as.numeric(rownames(medoids))
  distancesFromMedoids = c()
  #1st iteration forming cluster formation with random medoids
  #form clusters with minimum distance
  for(i in rowNumMedoids){
    distancesFromMedoids <- rbind(distancesFromMedoids, dismatrix[i,])
  }
  #take a transpose of matrix, each column will be a medoid.
  distancesFromMedoids <- t(distancesFromMedoids)
  minimumVal <- c()
  #create a column with cluster value to which it has minimum distance to
  clusterMatrix <- cbind(1:nrow(distancesFromMedoids), max.col(-distancesFromMedoids))
  for(i in 1:length(dataset[,1])){
    minimumVal <- rbind(minimumVal, min(distancesFromMedoids[i,]))
  }
  #3rd column with minimum distance values with the medoids
  newClusterMatrix <- cbind(clusterMatrix, minimumVal)
  #cost of cluster formation
  costSum <- sum(minimumVal[,1])
  thershold <- costSum;
  oldClusterMatrix <- matrix(data=1,nrow = length(newClusterMatrix[,1]), ncol = 3)
  maxiter<-1
  finalCluster <- newClusterMatrix
  finalMedoids <- rowNumMedoids
  finalDistancesFromMedoids <- distancesFromMedoids
  # Break the clustering formation if max 100 iterations
  #(99 iterations because one iteration of clusters formation happened already in above steps) 
  #are reached or there are no new cluster assignments.
  # Old and new ClusterMatrix's 3rd column has distances from medoids, if that doesn't change,
  # meaning there is no new cluster assignments.
  while(sum(oldClusterMatrix[,3] == newClusterMatrix[,3]) == 0 && maxiter <= 99) {
    # thershold becomes new updated cost
    oldClusterMatrix <- newClusterMatrix
    #random medoids to be chosen
    medoids <- dataset[sample(nrow(dataset), k),]
    #rownumbers of the random medoids
    rowNumMedoids <- as.numeric(rownames(medoids))
    distancesFromMedoids = c()
    #form clusters with minimum distance
    for(i in rowNumMedoids){
      distancesFromMedoids <- rbind(distancesFromMedoids, dismatrix[i,])
    }
    #take a transpose of matrix, each column will be a medoid.
    distancesFromMedoids <- t(distancesFromMedoids)
    minimumVal <- c()
    #create a column with cluster value to which it has minimum distance to
    clusterMatrix <- cbind(1:nrow(distancesFromMedoids), max.col(-distancesFromMedoids))
    for(i in 1:length(dataset[,1])){
      minimumVal <- rbind(minimumVal, min(distancesFromMedoids[i,]))
    }
    #3rd column with minimum distance values with the medoids
    newClusterMatrix <- cbind(clusterMatrix, minimumVal)
    costSum <- sum(minimumVal[,1])
    if(costSum < thershold) {
      # final cluster with the no more assignments and with lowest cost
      finalCluster <- newClusterMatrix
      finalMedoids <- rowNumMedoids
      finalDistancesFromMedoids <- distancesFromMedoids
      thershold <- costSum
    }
    maxiter = maxiter + 1
  }
  
  fileName = paste0("clusters_", k ,".csv")
  newDataWithClusterInfo = cbind(dataset, finalCluster[ , 2])
  # Write a cluster assignment to a file named cluster_k.csv with a input dataset 
  write.table(newDataWithClusterInfo, file=fileName, sep=",", append=FALSE, col.names = FALSE)
  ## Calculate average silhouette width
  average_silhouette = sil_width(newDataWithClusterInfo, finalDistancesFromMedoids, finalMedoids, dismatrix,k)
  print("Average silhouette width for the final clustering solution:")
  print(average_silhouette)
}
#Including cluster library for using daisy function for calculating dissimilarity matrix
library(cluster)

#Pass input as a csv dataset file and 
#number of clusters(k) to be formed
kmedoids('/Users/Komal Barge/Documents/R/assignment2/Wine.csv', 3)
