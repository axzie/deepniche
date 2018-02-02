extractCentroids <- function(df)
{
  return(df[,c(5,6)][!duplicated(df[,c(5,6)]),])
}

extractCentroidsList <- function(the_list)
{
  lapply(the_list, extractCentroids)
}

extractCentroidImage <- function(df1,df2){
  test <- cbind(df1,df2)
  size<-sqrt(length(test[,1]))
  dim1 <- ceiling(size)
  dim2 <- floor(size)
  
  orderTest <- as.data.frame(test[order(test[,1]),]) 
  seq <- 1:dim1
  seq <- rep(seq,each=dim2)
  result <- array(0,c(dim1,dim2,30))
  orderTest$id <- seq[1:length(orderTest[,1])]

  for(i in unique(seq))
  {
    cCol <- orderTest[which(orderTest$id==i),]
    cCol <- as.data.frame(cCol[order(cCol[,2]),]) 
    
    fill <- matrix(rep(0,dim2*30),nrow = dim2)
    we <- as.matrix(cCol[,3:(length(cCol[1,])-1)])
    xwe <- 1:dim(we)[1]  
    ywe <- 1:dim(we)[2]
    fill[xwe,ywe] <- we
    
    result[i,1:dim2,] <- fill
  }
  return(result)
}

extractCentroidImageList <- function(the_list1,the_list2){
  result <- mapply(FUN=extractCentroidImage,the_list1,the_list2,SIMPLIFY = F)
}

createImageArray <- function(the_list){
  l <- length(the_list)
  dims <- dim(the_list[[1]])
  result <- array(0,c(l,dims))
  for(i in 1:l){
    result[i,,,] <- the_list[[i]]
  }
  return(result)
}



neutralCentroids <- extractCentroidsList(sim.neutral[1:10])


imageList <- extractCentroidImageList(neutralCentroids[1:10],neutral[1:10])
imageStack <- createImageArray(test)

