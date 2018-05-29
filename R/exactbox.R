library(Matrix) # For bdiag() and sparse matrices.
library(gurobi) # For the MIP-model
#' Exactbox-Algorithm.
#' 
#' This code requires gurobi and the gurobi-library to solve the MIP.
#' 
#' @param positiveTraining Positive training data.
#' @param negativeTraining Negative training data.
#' @param positiveTesting Positive test data.
#' @param negativeTesting Negative test data.
#' @param cSize Number of different weight for negative data point.
#' @param maxK Cluster size.
#' @param cExpand Parameter to control the number of boxes.
#' @param timePerProblem Time that you are willing to invest for a single MIP problem.
#' @param varType 
#' @return A list containing training TP, training FP, testing FTP and testing FP.
#' @keywords Mixed Integer Programming,  box classification
#' @author Algorithm and Matlab-code by Cynthia Rudin and Siong Thye Go (see References). Implemented in R by Hendrik Pfaff
exactboxes <- function(positiveTraining, negativeTraining, positiveTesting, negativeTesting, cSize, maxK, cExpand, timePerProblem=4500, varType='C', env=NULL){
  # This is the margin size.
  v <- 0.05
  # This is a big number.
  M <- 10000
  # This is a small number.
  epsilon <- 0.00001
  
  # Create Matrizes (cSize x 1) and fill them with 0s. (Don't make these sparse)
  MIPtrainingTP <- matrix(0,cSize,1)
  MIPtrainingFP <- matrix(0,cSize,1)
  MIPtrainingTN <- matrix(0,cSize,1)
  MIPtrainingFN <- matrix(0,cSize,1)
  MIPtestingFP <- matrix(0,cSize,1)
  MIPtestingTP <- matrix(0,cSize,1)
  MIPtestingTN <- matrix(0,cSize,1)
  MIPtestingFN <- matrix(0,cSize,1)
  
  
  #### Working with the training data.####
  
  # Concatenate both training sets row-wise.
  A <- rbind(positiveTraining, negativeTraining)
  # Number of positive training points.
  mPositive <- nrow(positiveTraining)
  # Number of positive testing points.
  mPositiveTesting <- nrow(positiveTesting)
  # Number of negative training points.
  mNegative <- nrow(negativeTraining)
  # Number of rows (mnegativetesting) and columns (n) of the negative testing data points.
  mNegativeTesting <- nrow(negativeTesting)
  n <- ncol(negativeTesting)
  # Sum of all training points.
  m <- mPositive +  mNegative
  
  #### Set up for the l and u contraint. ####
  
  # Create a vector indicating the posTrain with 1 and negTrain with -1
  tempA <- rbind(Matrix(1,mPositive,1, sparse=TRUE),-Matrix(1,mNegative,1, sparse=TRUE))
  # Make a Matrix out of the vector.
  for(k in 2:(maxK*n)){
    tempA <- Matrix(bdiag(tempA, rbind(Matrix(1,mPositive,1, sparse=TRUE),Matrix(-1,mNegative,1, sparse=TRUE))), sparse=TRUE)
  }
  
  # Create a sparse Matrix with lower contraints.
  lConstraintA <- Matrix(cbind(tempA, Matrix(0, m*n*maxK, n*maxK, sparse=TRUE), M*Diagonal(m*n*maxK), Matrix(0, m*n*maxK, m*n*maxK+m*maxK+m, sparse=TRUE)), sparse=TRUE)
  lConstraintA <- rbind(-lConstraintA, lConstraintA)
  
  # Create a sparse Matrix analogous for upper constraints.
  uConstraintA <- cbind(Matrix(0, m*n*maxK, n*maxK, sparse=TRUE), tempA, Matrix(0, m*n*maxK, m*n*maxK, sparse=TRUE), -M*Diagonal(m*n*maxK), Matrix(0, m*n*maxK, m*maxK+m, sparse=TRUE)) 
  rm(tempA)
  gc()
  uConstraintA <- rbind(uConstraintA, -uConstraintA)
  
  #### Set up for the y constraint. ####
  
  # Create a n times repeating sparse Identity Matrix with m dimensions.
  T <- Matrix(repmat(Diagonal(m), 1, n), sparse=TRUE)
  D <- T
  if(maxK >= 2){
    for(k in 2:maxK){
      D <- Matrix(bdiag(D,T), sparse=TRUE)
    }
  }
  
  rm(T)
  gc()
  
  W1 <- bdiag(-Diagonal(mPositive), 2*n*Diagonal(mNegative))
  if(maxK >= 2){
    for(k in 2:maxK){
      W1 <- Matrix(bdiag(W1,-Diagonal(mPositive), 2*n*Diagonal(mNegative)), sparse=TRUE)
    }
  }
  
  
  W2 <- bdiag(2*n*Diagonal(mPositive), -Diagonal(mNegative))
  if(maxK >= 2){
    for(k in 2:maxK){
      W2 <- Matrix(bdiag(W2,2*n*Diagonal(mPositive),-Diagonal(mNegative)))
    }
  }
  
  yConstraintA <- Matrix(cbind(Matrix(0,2*m*maxK,2*n*maxK, sparse=TRUE), rbind(cbind(D,D),cbind(-D,-D)), rbind(W1,W2), Matrix(0,2*m*maxK,m, sparse=TRUE)), sparse=TRUE)
  
  rm(W1)
  rm(W2)
  rm(D)
  gc()
  
  #### Positive and negative (z) constraint. ####
  tempA <- Matrix(0, 2*mPositive, (2+2*m)*n*maxK, sparse=TRUE)
  tempB <- rbind(repmat(cbind(Diagonal(mPositive), Matrix(0, mPositive, mNegative, sparse=TRUE)), 1, maxK), -repmat(cbind(Diagonal(mPositive), Matrix(0, mPositive, mNegative, sparse=TRUE)), 1, maxK))
  tempC <- rbind(-maxK*Diagonal(mPositive), Diagonal(mPositive))
  tempD <- Matrix(0, 2*mPositive, mNegative, sparse=TRUE)
  positiveZConstraintA <- Matrix(cbind(tempA, tempB, tempC, tempD), sparse=TRUE)
  
  tempA <- Matrix(0, 2*mNegative, (2+2*m)*n*maxK, sparse=TRUE)
  tempB <- rbind(repmat(cbind(Matrix(0, mNegative, mPositive, sparse=TRUE), Diagonal(mNegative)), 1, maxK), repmat(cbind(Matrix(0, mNegative, mPositive, sparse=TRUE), -Diagonal(mNegative)), 1, maxK))
  tempC <- Matrix(0, 2*mNegative, mPositive, sparse=TRUE)
  tempD <- rbind(maxK*Diagonal(mNegative), -Diagonal(mNegative))
  negativeZConstraintA <- Matrix(cbind(tempA, tempB, tempC, tempD), sparse=TRUE)
  
  rm(tempA)
  rm(tempB)
  rm(tempC)
  rm(tempD)
  gc()
  
  # Creating the MIP model for solving the problem.
  model <- list()
  result <- NULL
  
  model$A <- Matrix(rbind(lConstraintA, uConstraintA, yConstraintA, positiveZConstraintA, negativeZConstraintA, cbind(Diagonal(n*maxK), -Diagonal(n*maxK), Matrix(0, n*maxK, 2*m*n*maxK+m*maxK+m, sparse=TRUE))),sparse=TRUE)
  # Clean the memory.
  rm(lConstraintA)
  rm(uConstraintA)
  rm(yConstraintA)
  rm(positiveZConstraintA)
  rm(negativeZConstraintA)
  gc()
  
  # Concatenation of both training set (like A) but with negative.
  B <- rbind(positiveTraining, -negativeTraining)
  # Writes both columns under each other.
  # if(typeof(B)=="list"){
  B <- Matrix(unlist(B), ncol=1, sparse=TRUE)
  # } else {
  #  B <- Matrix(as.vector(B), ncol=1, sparse=TRUE)
  # }
  
  model$rhs <- as.numeric(rbind(repmat(-B+v, maxK, 1), repmat(B+(-v+M-epsilon), maxK, 1), repmat(B+v, maxK, 1), repmat(-B+(-v+M-epsilon), maxK, 1), repmat(rbind((2*n-1)*Matrix(1, mPositive, 1, sparse=TRUE), 2*n*Matrix(1, mNegative, 1, sparse=TRUE)), maxK, 1), repmat(rbind(Matrix(0, mPositive, 1, sparse=TRUE), -Matrix(1, mNegative, 1, sparse=TRUE)), maxK, 1), Matrix(0, 2*mPositive, 1, sparse=TRUE), maxK*Matrix(1, mNegative, 1, sparse=TRUE), -Matrix(1, mNegative, 1, sparse=TRUE), Matrix(0, n*maxK, 1, sparse=TRUE)))
  rm(B)
  model$sense <- '<'
  model$vtype <- rbind(repmat(matrix(varType, ncol=1), 2*n*maxK, 1), repmat(matrix('B', ncol=1), 2*m*n*maxK+m*maxK+m, 1))
  model$modelsense <- 'max'
  
  # Wieso wird hier +/- 1 gerechnet?
  model$lb <- as.numeric(rbind((min(A)-1)*Matrix(1, 2*n*maxK, 1, sparse=TRUE), Matrix(0, 2*m*n*maxK+m*maxK+m, 1, sparse=TRUE)))
  model$ub <- as.numeric(rbind((max(A)+1)*Matrix(1, 2*n*maxK,1, sparse=TRUE), Matrix(1, 2*m*n*maxK+m*maxK+m, 1, sparse=TRUE))) 
  lowerlist <- list()
  upperlist <- list()
  tradeoff <- numeric()
  tempcount <- 1
  
  
  # Solving the model for c.
  for(imbalancedc in seq(1/cSize, 1, by=1/cSize)){
    tradeoff[tempcount] <- imbalancedc
    model$obj <- as.numeric(cbind(-cExpand*Matrix(1, 1, n*maxK, sparse=TRUE), cExpand*Matrix(1, 1, n*maxK, sparse=TRUE), Matrix(0, 1, 2*m*n*maxK+m*maxK, sparse=TRUE), Matrix(1, 1, mPositive, sparse=TRUE), imbalancedc*Matrix(1, 1, mNegative, sparse=TRUE)))
    params <- list()
    params$outputflag <- 1
    params$timelimit <- timePerProblem
    params$LogToConsole <- 0
    # params$logfile <- paste('gurobilog',imbalancedc,'.txt')
    
    # Calling the MIP-solver.
    result <- gurobi(model, params)
    lowerboundary <- result$x[seq(1,n*maxK)]
    upperboundary <- result$x[seq((n*maxK+1),(2*n*maxK))]
    lowerboundary <- matrix(lowerboundary, n, maxK)
    upperboundary <- matrix(upperboundary, n, maxK)
    
    #Transpose the two boundaries.
    lowerideal <- t(lowerboundary)
    upperideal <- t(upperboundary)
    
    lowerlist[[tempcount]] <- lowerideal
    upperlist[[tempcount]] <- upperideal
    
    # Create lists of zeros for later OR-Operations.
    trainingpositiveclassification <- matrix(0, nrow(positiveTraining), 1)
    testingpositiveclassification <- matrix(0, nrow(positiveTesting), 1)
    trainingnegativeclassification <- matrix(0, nrow(negativeTraining), 1)
    testingnegativeclassification <- matrix(0, nrow(negativeTesting), 1)
    
    # Logical OR to find all right classified data points.
    for(k in 1:maxK){
      trainingpositiveclassification <- trainingpositiveclassification | myAll((positiveTraining >= repmat(matrix(lowerideal[k,], nrow=1), nrow(positiveTraining), 1)) & (positiveTraining <= repmat(matrix(upperideal[k,], nrow=1), nrow(positiveTraining), 1)))
      testingpositiveclassification <- testingpositiveclassification | myAll((positiveTesting >= repmat(matrix(lowerideal[k,], nrow=1), nrow(positiveTesting), 1)) & (positiveTesting <= repmat(matrix(upperideal[k,], nrow=1), nrow(positiveTesting), 1)))
      trainingnegativeclassification <- trainingnegativeclassification | myAll((negativeTraining >= repmat(matrix(lowerideal[k,], nrow=1), nrow(negativeTraining), 1)) & (negativeTraining <= repmat(matrix(upperideal[k,], nrow=1), nrow(negativeTraining), 1)))
      testingnegativeclassification <- testingnegativeclassification | myAll((negativeTesting >= repmat(matrix(lowerideal[k,], nrow=1), nrow(negativeTesting), 1)) & (negativeTesting <= repmat(matrix(upperideal[k,], nrow=1), nrow(negativeTesting), 1)))
    }
    
    # Calculating the True and False Positives/Negatives for the training and test data sets.
    TP <- sum(testingpositiveclassification)
    FP <- sum(testingnegativeclassification)
    TN <- mNegativeTesting - FP
    MIPtestingTP[tempcount] <- TP
    MIPtestingFP[tempcount] <- mNegativeTesting - TN
    MIPtestingTN[tempcount] <- TN
    MIPtestingFN[tempcount] <- mPositiveTesting - TP
    
    TP <- sum(trainingpositiveclassification)
    FP <- sum(trainingnegativeclassification)
    TN <- mNegative - FP
    MIPtrainingTP[tempcount] <- TP
    MIPtrainingFP[tempcount] <- mNegative - TN
    MIPtrainingTN[tempcount] <- TN
    MIPtrainingFN[tempcount] <- mPositive - TP
    
    tempcount <- tempcount + 1
  }
  
  ret <- list(trainingTP=MIPtrainingTP, trainingFP=MIPtrainingFP, trainingTN=MIPtrainingTN, trainingFN=MIPtrainingFN, testingTP=MIPtestingTP, testingFP=MIPtestingFP, testingTN=MIPtestingTN, testingFN=MIPtestingFN, tradeoff=tradeoff, lowerIdeal=lowerlist, upperIdeal=upperlist)
  
  return(ret)
}
