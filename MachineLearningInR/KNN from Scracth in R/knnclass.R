train.split <- function(xtrain, ytrain, sample.p = .8){
  nrow <- dim(xtrain)[1]
  split <- sample(nrow,sample.p*nrow)
  return(list(xtrain = xtrain[split,], ytrain = ytrain[split],
              xval = xtrain[-split,], yval = ytrain[-split]))
} 
#calculate euclid distance
euclid <- function(y1,y2){
  return(sqrt(sum((y1-y2)^2)))
}
#get the mode of a vector
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

knnclass <- function(xtrain, xtest, ytrain){
  df <- train.split(xtrain, ytrain)
  xtrain <- df$xtrain
  ytrain <- df$ytrain
  xval <- df$xval
  yval <- df$yval
  
  # scale
  xtrain <- scale(xtrain)
  xval = scale(xval, center=attr(xtrain, "scaled:center"), 
               scale=attr(xtrain, "scaled:scale"))
  # calculate the classification result for different k each observation of val output has dimension k *
  knn_matrix <- data.frame(sapply(1:length(yval), function(j){
    x <- xval[j,]
    ecludian <- data.frame(distance = sapply(1:length(ytrain),
                                             function(i){euclid(x,xtrain[i,])}))
    ecludian$label <- ytrain
    ecludian <- ecludian%>%arrange(distance)
    knn<- c()
    for(k in 1:sqrt(dim(xtrain)[1])){
      knn_k <- ecludian$label[1:k]
      result <- getmode(knn_k)
      knn <- cbind(knn,result)
    }
    knn
  }))
  # calculate the classification accuracy for each k
  acur <- apply(knn_matrix, 1, function(x){sum(x==yval)/length(yval)})
  k_optimal <- which.max(acur)
  
  #predict result
  xtest <- scale(xtest, center=attr(xtrain, "scaled:center"), 
                 scale=attr(xtrain, "scaled:scale"))   
  ypred <- apply(xtest,1,function(x){
    ecludian <- data.frame(distance =sapply(1:length(ytrain),
                                            function(i){euclid(x,xtrain[i,])}),
                           label = ytrain)%>%arrange(distance)
    knn_k <- getmode(ecludian$label[1:k_optimal])
    knn_k})
  return(ypred)
}