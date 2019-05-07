# BEFORE YOU TURN THIS IN, DO THE FOLLOWING:
# 1) CLEAR THE GLOBAL ENVIRONMENT 
#   - AFTER THAT NOTHING SHOULD BE IN YOUR GLOBAL ENVIRONMENT
# 2) SOURCE THIS FILE
#   - IF YOU GET AN ERROR, THEN SARAH WILL GET AN ERROR AND YOU WON'T GET A GOOD GRADE


############## Week 5 Homework ##############
############### Epidemiology ################

# Write your functions in the order of the handout.
# Use the test code provided to ensure that they work.
# Make sure you name each function as it is in the test code.
# DO NOT MODIFY THE PROVIDED TEST CODE. It will be used to grade your assignment.
# You should also try some test code of your own
# to make sure that the functions work for many cases.

############# Activity 1 ###################
# Function to set up the grid
print("Activity 1")

createGrid <- function(numRow, numCol, r){
  populatioN <- numeric(numRow * numCol)
  vacc <- (numRow * numCol) * r
  populatioN[1:vacc] <- 2
  populatioN[vacc+1:(length(populatioN)-vacc)] <- 1
  
  people <- sample(populatioN, length(populatioN), F, prob = c(numeric(length(populatioN)) + 1))
  x <- array(people, dim = c(numRow, numCol))
  return(x)
}

# Test code
# Should create a 6x4 grid with 18 2s and six 1s randomly distributed
createGrid(6,4 , 0.75)


########### Activity 2 ########################
# Function to check whether two arrays are identical
print("Activity 2")

compareArray <- function(x, y){
  flag <- "Your arrays are fine"
  
  for(i in 1:length(dim(x))){
    if(is.na(x[i]) | is.na(y[i])){
      flag <- "NAs are present in your array"
      break
    }
    if(ncol(x) != ncol(y) | nrow(x) != nrow(y)){
      flag <- "Your dimensions are not of the same size."
      break
    }
    for(j in 1:length(x)){
      if(x[j] != y[j]){
        flag <- "Your arrays do not have the same value"
        break
      }
    }
    
  }
  return(flag)
}



# Test code
testGrid1 <- array(data = c(numeric(23), 1), dim = c(6, 4))
testGrid2 <- testGrid1
print(compareArray(testGrid1, testGrid2)) # Should print TRUE
testGrid2 <- array(data = c(numeric(10), 1, numeric(13)), dim = c(6, 4))
print(compareArray(testGrid1, testGrid2)) # Should print FALSE
testGrid3 <- array(data=1:6, dim = c(3,2))
testGrid4 <- array(data=1:6, dim = c(2,3))
print(compareArray(testGrid3, testGrid4)) # Should print FALSE
testGrid5 <- array(data=c(3,2,4,4,5,6), dim = c(2,3))
print(compareArray(testGrid5, testGrid4)) # Should print FALSE



############# Activity 3 ######################
# Function to find the coordinates of all neighbors
# of a given spot in an array
print("Activity 3")

detect <- function (array, enter){
  
  gridArray <- array(0, dim = c(4,2))
  gridArray[1,] <- c(enter[1] - 1, enter[2])
  gridArray[2,] <- c(enter[1] + 1, enter[2])
  gridArray[3,] <- c(enter[1], enter[2] + 1)
  gridArray[4,] <- c(enter[1], enter[2] - 1)
  row.names(gridArray) <-  c("North", "South", "East", "West")
  colnames(gridArray) <- c("x coordinates", "y coordinates")
  
  for(i in 4:1){
    if(gridArray[i,1] > nrow(array) | gridArray[i,1] <= 0){
      gridArray <- gridArray[-i,]
    }
  }
  for(j in nrow(gridArray):1){
    if(gridArray[j, 2] > ncol(array) | gridArray[j, 2] <= 0){
      gridArray <- gridArray[-j,]
    }
  }
  
  return(gridArray)
}
# Test code
detect(testGrid1, c(2, 2))

# Should print the following:
# 1 2
# 2 3
# 3 2
# 2 1
# Not necessarily in that order

print(detect(testGrid1, c(6, 3)))

# Should print the following:
# 6 2
# 5 3
# 6 4
# Not necessarily in that order


################ Activity 4 ################
# Put a 0 in a 60x80 grid that you make with createGrid().
# No test code for this one.
print("Activity 4")

popGrid <- createGrid(60, 80, 1/(60*80))
# v <- 0
# for(i in 1:4800){
#   v <- v+1
#   if(popGrid[i] == 0){
#     print(v)
#   }
# }
# for loop I made to check where the infected individual is present in
# the population grid. When I ran this, it was at 787. It should print a
# different value for you.

View(popGrid)




############## Activity 5 #################
# Function to decide whether an individual should be infected
print("Activity 5")

getsInfected <- function( Array, position  ){
  # flag <- F
  
  Array[position[1], position[2]] <- 0
  
  # if(Array[position[1], position[2]] != 1){
  #   return(flag)
  # }else{ The function works. I'm gonna commment these out
  # (array[position[1], position[2]]) # prints one when c(5,4) is ran. works
  
  gridArray <- array(0, dim = c(4,2))
  gridArray[1,] <- c(position[1] - 1, position[2])
  gridArray[2,] <- c(position[1] + 1, position[2])
  gridArray[3,] <- c(position[1], position[2] + 1)
  gridArray[4,] <- c(position[1], position[2] - 1)
  row.names(gridArray) <-  c("North", "South", "East", "West")
  colnames(gridArray) <- c("x coordinates", "y coordinates")
  
  for(i in 4:1){
    if(gridArray[i,1] > nrow(Array) | gridArray[i,1] <= 0){
      gridArray <- gridArray[-i,]
    }
  }
  for(j in nrow(gridArray):1){
    if(gridArray[j, 2] > ncol(Array) | gridArray[j, 2] <= 0){
      gridArray <- gridArray[-j,]
    }
  }
  
  for(i1 in nrow(gridArray):1){
    
    if(Array[gridArray[i1, 1],gridArray[i1 ,2]] == 1){
      Array[gridArray[i1, 1], gridArray[i1, 2]] <- 0
      
      # flag <- T
    }
  }
  return(Array)
}


# Test code
testGrid3 <- array(data = c(numeric(23) + 1, 0), dim = c(6, 4))
print(getsInfected(testGrid3, c(5, 4))) # Should print TRUE
print(getsInfected(testGrid3, c(1, 1))) # Should print FALSE


############### Activity 6 ################
# Code to calculate the fraction of infected individuals at the end of the simulation
# This doesn't have to be a function.
# Either way, store the fraction in a variable called fractionInfected.
print("Activity 6")

testGrid4 <- array(data = (c(numeric(9), numeric(15) + 1)), dim = c(6, 4))
# Write code to calculate the fraction of infected individuals (0s) in testGrid4
# and store that fraction in fractionInfected.
Infect <- 0
Uninf <- 0

for(patients in 1:length(testGrid4)){
  if(testGrid4[patients] == 0){
    Infect <- Infect + 1
  }else{
    Uninf <- Uninf + 1
  }
}

fractionInfected <- (Infect / (Infect+Uninf))

# Test code
print(fractionInfected) # Should print 0.375


############### Activity 7 #################
# Putting it all together
# Now write code to run the simulation once.
# Use the base model and 10% vaccination.
# Make sure you store fractionInfected at the end so that it prints to the console.
# To plot, look at activity 7b.  [For activity 8-14 you may want to comment it out]
print("Activity 7")

my.grid <- createGrid(60, 80, .10)



# Test code
Infect <- 0
Uninf <- 0

for(patients in 1:length(my.grid)){
  if(my.grid[patients] == 1){
    Infect <- Infect + 1
  }else{
    Uninf <- Uninf + 1
  }
}

fractionInfected <- (Infect / (Infect+Uninf))

# my.grid[ceiling(runif(1, 0, 60)), ceiling(runif(1, 0, 80))] <- 0 # Starts the infection on 3,1 on my.grid



getsInfected2 <- function( Array, position){
  Array[position[1], position[2]] <- 0
  
  gridArray <- array(0, dim = c(4,2))
  gridArray[1,] <- c(position[1] - 1, position[2])
  gridArray[2,] <- c(position[1] + 1, position[2])
  gridArray[3,] <- c(position[1], position[2] + 1)
  gridArray[4,] <- c(position[1], position[2] - 1)
  
  for(i in 4:1){
    if(gridArray[i,1] > nrow(Array) | gridArray[i,1] <= 0){
      gridArray <- gridArray[-i,]
    }
  }
  for(j in nrow(gridArray):1){
    if(gridArray[j, 2] > ncol(Array) | gridArray[j, 2] <= 0){
      gridArray <- gridArray[-j,]
    }
  }
  print(gridArray)
  

  for(i1 in nrow(gridArray):1){
    position <- gridArray[i1]
    getsInfected.return.positions(Array, position)
    
    

    if(Array[gridArray[i1, 1], gridArray[i1 ,2]] == 1){
      Array[gridArray[i1, 1], gridArray[i1, 2]] <- 0
    }
  }
  ###### SAME AS FUNCTION getsInfected.
  
  return(Array)
}


print(fractionInfected) # Output may vary, but should be about 0.9

# getsInfected2(my.grid, c(ceiling(runif(1, 0, 60)), ceiling(runif(1, 0, 80))))
getsInfected2(my.grid, c(3,1))

View(my.grid)







################ Activity 8 ###############
# Varying the vaccination rate
# Make the plot described in the handout for #8.
# Increase the vaccination rate from 2% to 98%, in increments of 2%.
# Each time, store the final infected fraction,
# and plot the infected fraction vs. vaccination rate.




################ Activity 9 ###############
# Make the vaccine less than perfect
# Vary the vaccination rate
# Make a plot like in Activity 8.
# Increase the vaccination rate from 2% to 98%, in increments of 2%.
# Each time, store the final infected fraction,
# and plot the infected fraction vs. vaccination rate.





############### Activities 10-14 ##############
# Write any other code for making additional plots or modifying the model here.
# You need to do at least one of 10-14
# Comments or print statements describing what your code does are helpful!

getsInfected.return.positions <- function( Array, position  ){
  # flag <- F
  
  Array[position[1], position[2]] <- 0
  
  # if(Array[position[1], position[2]] != 1){
  #   return(flag)
  # }else{ The function works. I'm gonna commment these out
  # (array[position[1], position[2]]) # prints one when c(5,4) is ran. works
  
  gridArray <- array(0, dim = c(4,2))
  gridArray[1,] <- c(position[1] - 1, position[2])
  gridArray[2,] <- c(position[1] + 1, position[2])
  gridArray[3,] <- c(position[1], position[2] + 1)
  gridArray[4,] <- c(position[1], position[2] - 1)
  row.names(gridArray) <-  c("North", "South", "East", "West")
  colnames(gridArray) <- c("x coordinates", "y coordinates")
  
  for(i in 4:1){
    if(gridArray[i,1] > nrow(Array) | gridArray[i,1] <= 0){
      gridArray <- gridArray[-i,]
    }
  }
  for(j in nrow(gridArray):1){
    if(gridArray[j, 2] > ncol(Array) | gridArray[j, 2] <= 0){
      gridArray <- gridArray[-j,]
    }
  }
  
  return(gridArray)
}

getsInfected.return.positions(testGrid3, c(5,4))









