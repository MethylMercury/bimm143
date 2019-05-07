#Q1.

population <- array(data = NA, dim = c(60, 80))

View(population)

#Q2.

compareArray <- function(x, y){
  flag <- "check your arrays again. something is wrong"
  for(i in 1:length(x)){
    if(is.na(x[i]) | is.na(y[i])){
      flag <- "NAs are present in your array"
      break
    }
    if(x[i] == y[i]){
      flag <- "Your array looks fine"
    }
  }
  print(flag)
}



#Q3

detect <- function (array, enter){

  gridArray <- array(0, dim = c(4,2))
  gridArray[1,] <- c(array[enter[1] - 1], array[enter[2]])
  gridArray[2,] <- c(array[enter[1] + 1], array[enter[2]])
  gridArray[3,] <- c(array[enter[1]], array[enter[2] + 1])
  gridArray[4,] <- c(array[enter[1]], array[enter[2] - 1])
  row.names(gridArray) <-  c("North", "South", "East", "West")
  colnames(gridArray) <- c("x coordinates", "y coordinates")
  
  for(i in 4:1){
    if(gridArray[i,1] > nrow(array)){
      gridArray <- gridArray[-i,]
    }
  }
  for(j in nrow(gridArray):1){
    if(gridArray[j, 2] > ncol(array)){
      gridArray <- gridArray[-j,]
    }
  }
  
  return(gridArray)
}