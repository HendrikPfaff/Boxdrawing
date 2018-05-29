#' Fastbox-Algorithm
#'
#' Use the Fastbox-Algorithm for classification.
#' @param positiveTraining Positive training data.
#' @param negativeTraining Negative training data.
#' @param positiveTesting Positive test data.
#' @param negativeTesting Negative test data.
#' @param cSize The size that of weight for negative data to try. It will enumerate weight from 1/cSize, 2/cSize, etc... up to 1.
#' @param idealK The number of clusters.
#' @param beta The expansion parameter.
#' @return A list containing training TP, training FP, testing TP, testing FP and the upper/lower ideal boundray.
#' @keywords box classification
#' @author Algorithm and Matlab-code by Cynthia Rudin and Siong Thye Go (see References). Translation to R by Hendrik Pfaff
#' @references 
#' @export
fastboxes <- function(positiveTraining,negativeTraining,positiveTesting,negativeTesting,cSize,idealK,beta){
  writeLines("Starting FastBox.")
  
  # First, make the entries to be less than 1.
  mPositive <- nrow(positiveTraining)
  mNegative <- nrow(negativeTraining)
  mPositiveTesting <- nrow(positiveTesting)
  mNegativeTesting <- nrow(negativeTesting)
  
  ratioVector <- apply(rbind(abs(positiveTraining), abs(negativeTraining)), 2, max)
  ratioIndex <- ratioVector > 0
  ratioVector <- matrix(ratioVector[ratioIndex], nrow=1)
  
  positiveTraining <- positiveTraining[,ratioIndex]/repmat(ratioVector, mPositive, 1)
  negativeTraining <- negativeTraining[,ratioIndex]/repmat(ratioVector, mNegative, 1)
  positiveTesting <- positiveTesting[,ratioIndex]/repmat(ratioVector,mPositiveTesting, 1)
  negativeTesting <- negativeTesting[,ratioIndex]/repmat(ratioVector,mNegativeTesting, 1)
  
  # positiveTraining[,ratioIndex] <- positiveTraining[,ratioIndex]/repmat(ratioVector, mPositive, 1)
  # negativeTraining[,ratioIndex] <- negativeTraining[,ratioIndex]/repmat(ratioVector, mNegative, 1)
  # positiveTesting[,ratioIndex] <- positiveTesting[,ratioIndex]/repmat(ratioVector,mPositiveTesting, 1)
  # negativeTesting[,ratioIndex] <- negativeTesting[,ratioIndex]/repmat(ratioVector,mNegativeTesting, 1)
  
  # Normalize all dimensions.
  superUpperBound <- max(rbind(positiveTraining,negativeTraining,positiveTesting,negativeTesting))
  superLowerBound <- min(rbind(positiveTraining,negativeTraining,positiveTesting,negativeTesting))
  
  A <- rbind(positiveTraining,negativeTraining)
  overallSize <- nrow(A)
  overallMean <- matrix(apply(A,2,mean), nrow=1)
  overallStd <- matrix(apply(A,2,sd), nrow=1)
  
  tempA <- (A - repmat(overallMean,overallSize,1))/repmat(overallStd,overallSize,1)
  tempA[is.na(tempA)] <- 1
  tempA[,as.vector(overallStd) == 0] <- 1
  
  ourtrainingTP <- matrix(0,cSize,1)
  ourtrainingFP <- matrix(0,cSize,1)
  ourtrainingTN <- matrix(0,cSize,1)
  ourtrainingFN <- matrix(0,cSize,1)
  ourtestingTP <- matrix(0,cSize,1)
  ourtestingFP <- matrix(0,cSize,1)
  ourtestingTN <- matrix(0,cSize,1)
  ourtestingFN <- matrix(0,cSize,1)
  
  # Step 2: Cluster positive class.
  n <- ncol(positiveTraining)
  
  if(mPositive == 1){
    # Extreme case of a single sample point.
    IDX <- 1
  } else {
    tempD <- as.matrix(dist(tempA))
    tempStartIndex <- matrix(0,1,idealK)
    # What about a seed?
    #tempStartIndex[1] <- as.integer(runif(1,0,mPositive))
    tempStartIndex[1] <- 6
    if(idealK >= 2){
      for(tempIndex in 2:idealK){
        dummy <- setdiff(seq(1:mPositive), tempStartIndex[seq(1:tempIndex-1)])
        #dummyParticularIndex <- max(sum(tempD[tempStartIndex[seq(1:tempIndex-1)],dummy]))
        dummyParticularIndex <- 1
        tempStartIndex[tempIndex] <- dummy[dummyParticularIndex]
      }
    }
    # Generate clusters.
    IDX <- kmeans(tempA[seq(1:mPositive),],idealK)$cluster
  }
  
  # Just doing some initialization.
  lowerTempPositiveSumMatrix <- matrix(0,idealK,n)
  upperTempPositiveSumMatrix <- matrix(0,idealK,n)
  lowerTempNegativeSumMatrix <- matrix(0,idealK,n)
  upperTempNegativeSumMatrix <- matrix(0,idealK,n)
  lowerMatrix <- matrix(0,idealK,n)
  upperMatrix <- matrix(0,idealK,n)
  centerMatrix <- matrix(0,idealK,n)
  lowerMinimalExpansion <- matrix(0,idealK,n)
  upperMinimalExpansion <- matrix(0,idealK,n)
  
  # Define matlab constant.
  eps <- 2^-52
  
  # Next, we do space division.
  for(k in 1:idealK){
    if(!is.null(A[IDX==k,])){
      ind <- IDX==k
      ind[seq(length(ind)+1, nrow(A))] <- FALSE 
      B <- A[ind,]
      tempPositiveIndex <- IDX == k
      lowerMatrix[k,] <- apply(B, 2, min)-eps
      upperMatrix[k,] <- apply(B, 2, max)+eps
      centerMatrix[k,] <- (lowerMatrix[k,] + upperMatrix[k,])/2
      # Which points are less than the lower boundary of a box.
      lowerIndicator <- A < repmat(lowerMatrix[k,],overallSize,1)
      
      for(tempj in 1:n){
        if(sum(lowerIndicator[,tempj]) > 0){
          # Precompute where is the next negative point.
          lowerMinimalExpansion[k,tempj] <- 0.99 * max(A[lowerIndicator[,tempj], tempj]) + 0.01 * lowerMatrix[k,tempj]
        } else {
          lowerMinimalExpansion[k,tempj] <- superLowerBound
        }
      }
      
      centerIndicator <- A < repmat(centerMatrix[k,],overallSize,1)
      rightinthemiddleIndicator <- A == repmat(centerMatrix[k,],overallSize,1)
      # Upper points are higher than the upper boundary of a box.
      upperIndicator <- A > repmat(upperMatrix[k,],overallSize,1)
      
      for(tempj in 1:n){
        if(sum(upperIndicator[,tempj]) > 0){
          upperMinimalExpansion[k,tempj] <- 0.99 * min(A[upperIndicator[,tempj], tempj]) + 0.01 * upperMatrix[k,tempj]
        } else {
          upperMinimalExpansion[k,tempj] <- superUpperBound
        }
      }
      
      inTheBoxIndicator <- (!lowerIndicator) & (!upperIndicator)
      inTheBoxIndicator <- matrix(myAll(inTheBoxIndicator), ncol=1)
      # Which points are inside the box?
      inTheBoxIndicator <- repmat(inTheBoxIndicator,1,n)
      lowerFlag <- (lowerIndicator |((centerIndicator|rightinthemiddleIndicator) & inTheBoxIndicator))
      upperFlag <- (upperIndicator |((!centerIndicator|rightinthemiddleIndicator) & inTheBoxIndicator))
      distanceToLowerBoundary <- A - repmat(lowerMatrix[k,],overallSize,1)
      distanceToUpperBoundary <- A - repmat(upperMatrix[k,],overallSize,1)
      
      # Precompute some statistics.
      lowerTempPositiveSumMatrix[k,] <- apply((lowerFlag[1:mPositive,] * matrix(repmat(tempPositiveIndex,1,n),ncol=n)) * exp(-distanceToLowerBoundary[1:mPositive,]-1), 2, sum)
      # We can precompute this quantity.
      weightFactorStorage <- (lowerIndicator | upperIndicator) * exp(-pmin(abs(distanceToLowerBoundary),abs(distanceToUpperBoundary)))
      weightFactorStorage[weightFactorStorage == 0] <- 1
      weightFactorStorage <- repmat(apply(weightFactorStorage,1,prod),1,n) / weightFactorStorage
      # Numbers that are huge are truncated at 10^16.
      lowerTempPositiveSumMatrix[lowerTempPositiveSumMatrix > 10^16] <- 10^16
      lowerTempPositiveSumMatrix[is.nan(lowerTempPositiveSumMatrix)] <- 10^16
      
      expoDistanceToLowerBoundary <- exp(distanceToLowerBoundary + 1)
      expoDistanceToLowerBoundary[expoDistanceToLowerBoundary > 10^16] <- 10^16
      lowerTempNegativeSumMatrix[k,] <- apply(lowerFlag * rbind(repmat(as.matrix(!tempPositiveIndex),1,n),matrix(1,mNegative,n)) * weightFactorStorage * expoDistanceToLowerBoundary, 2, sum)
      lowerTempNegativeSumMatrix[is.nan(lowerTempNegativeSumMatrix)] <- 10^16
      
      upperTempPositiveSumMatrix[k,] <- apply((upperFlag[1:mPositive,] * repmat(as.matrix(tempPositiveIndex),1,n)) * exp(distanceToUpperBoundary[1:mPositive,]-1), 2, sum)
      upperTempPositiveSumMatrix[upperTempPositiveSumMatrix > 10^16] <- 10^16
      upperTempPositiveSumMatrix[is.nan(upperTempPositiveSumMatrix)] <- 10^16
      
      upperTempNegativeSumMatrix[k,] <- apply(upperFlag * rbind(repmat(as.matrix(!tempPositiveIndex),1,n), matrix(1,mNegative,n)) * weightFactorStorage * exp(-distanceToUpperBoundary+1), 2, sum)
      upperTempNegativeSumMatrix[upperTempNegativeSumMatrix > 10^16] <- 10^16
      upperTempNegativeSumMatrix[is.nan(upperTempNegativeSumMatrix)] <- 10^16
    }
  }
  
  cVector <- seq((1/cSize),1,(1/cSize))
  cVector <- t(cVector)
  lowerList <- list()
  upperList <- list()
  lowerIdeal <- matrix(0,idealK,n)
  upperIdeal <- matrix(0,idealK,n)
  
  for(c in 1:length(cVector)){
    trainingPositiveClassification <- matrix(0,nrow(positiveTraining),1)
    testingPositiveClassification <- matrix(0,nrow(positiveTesting),1)
    trainingNegativeClassification <- matrix(0,nrow(negativeTraining),1)
    testingNegativeClassification <- matrix(0,nrow(negativeTesting),1)
    
    for(k in 1:idealK){
      lowerIdeal[k,] <- log((-beta + sqrt(beta^2 + 4*lowerTempPositiveSumMatrix[k,] * (cVector[c]*lowerTempNegativeSumMatrix[k,]))) / (2*lowerTempPositiveSumMatrix[k,]))
      tempLowerIdeal <- lowerIdeal[k,]
      tempIndex <- ((-beta + sqrt(beta^2+4*lowerTempPositiveSumMatrix[k,]* (cVector[c]*lowerTempNegativeSumMatrix[k,])) ) / (2*lowerTempPositiveSumMatrix[k,]))
      tempLowerIdeal[tempIndex < eps] <- log(eps)
      
      # Adjustment of boundary.
      lowerIdeal[k,] <- tempLowerIdeal + lowerMatrix[k,]-1
      # Push to far end.
      lowerIdeal[k,(cVector[c]*lowerTempNegativeSumMatrix[k,])==0] <- superLowerBound
      # Push towards next negative point.
      lowerIdeal[k,] <- pmin(lowerIdeal[k,],lowerMinimalExpansion[k,])
      
      upperIdeal[k,] <- log((beta + sqrt(beta^2+4*upperTempPositiveSumMatrix[k,] * (cVector[c]*upperTempNegativeSumMatrix[k,]))) / (2*cVector[c]*upperTempNegativeSumMatrix[k,]))
      tempUpperIdeal <- upperIdeal[k,]
      tempIndex <- (beta + sqrt(beta^2+4*upperTempPositiveSumMatrix[k,]*(cVector[c]*upperTempNegativeSumMatrix[k,]))) / (2*cVector[c]*upperTempNegativeSumMatrix[k,])
      tempUpperIdeal[tempIndex < eps] <- log(eps);
      # Adjustment of boundary.
      upperIdeal[k,] <- tempUpperIdeal + upperMatrix[k,] + 1
      
      # Push to far end.
      upperIdeal[k,(cVector[c]*upperTempNegativeSumMatrix[k,])==0] <- superUpperBound
      # Push towards next negative point.
      upperIdeal[k,] <- pmax(upperIdeal[k,],upperMinimalExpansion[k,])
      
      trainingPositiveClassification <- trainingPositiveClassification | myAll(((positiveTraining >= repmat(lowerIdeal[k,],nrow(positiveTraining),1)) & (positiveTraining <= repmat(upperIdeal[k,],nrow(positiveTraining),1))))
      testingPositiveClassification <- testingPositiveClassification | myAll(((positiveTesting >= repmat(lowerIdeal[k,],nrow(positiveTesting),1)) & (positiveTesting <= repmat(upperIdeal[k,],nrow(positiveTesting),1))))
      trainingNegativeClassification <- trainingNegativeClassification | myAll(((negativeTraining >= repmat(lowerIdeal[k,],nrow(negativeTraining),1)) & (negativeTraining <= repmat(upperIdeal[k,],nrow(negativeTraining),1))))
      testingNegativeClassification <- testingNegativeClassification | myAll(((negativeTesting >= repmat(lowerIdeal[k,],nrow(negativeTesting),1)) & (negativeTesting <= repmat(upperIdeal[k,],nrow(negativeTesting),1))))
    }
    
    TP <- sum(trainingPositiveClassification)
    FP <- sum(trainingNegativeClassification)
    TN <- mNegative - FP
    FN <- mPositive - TP
    ourtrainingTP[c] <- TP
    ourtrainingFP[c] <- FP
    ourtrainingTN[c] <- TN
    ourtrainingFN[c] <- FN
    
    TP <- sum(testingPositiveClassification)
    FP <- sum(testingNegativeClassification)
    TN <- mNegativeTesting - FP
    FN <- mPositiveTesting - TP
    ourtestingTP[c] <- TP
    ourtestingFP[c] <- FP
    ourtestingTN[c] <- TN
    ourtestingFN[c] <- FN
    
    # Rescale back.
    tmp <- lowerIdeal
    tmp1 <- ratioVector
    tmp2 <- repmat(ratioVector,idealK,1)
    lowerIdeal <- lowerIdeal*repmat(ratioVector,idealK,1)
    upperIdeal <- upperIdeal*repmat(ratioVector,idealK,1)
    lowerList[[c]] <- lowerIdeal
    upperList[[c]] <- upperIdeal
  }
  
  # Rescale back.
  # lowerIdeal <- lowerIdeal*repmat(ratioVector,idealK,1)
  # upperIdeal <- upperIdeal*repmat(ratioVector,idealK,1)
  
  # Returning the approx. model.
  return(list(trainingTP=ourtrainingTP,
              trainingFP=ourtrainingFP,
              trainingTN=ourtrainingTN,
              trainingFN=ourtrainingFN,
              testingTP=ourtestingTP,
              testingFP=ourtestingFP,
              testingTN=ourtestingTN,
              testingFN=ourtestingFN,
              tradeoff=cVector,
              lowerIdeal=lowerList,
              upperIdeal=upperList))
}