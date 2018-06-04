# Boxdrawing in R
Implementing the Boxdrawing algorithms as a R package for my Master's Thesis.

This package contains the two box drawing algorithms for classification (Exactbox and Fastbox) and several helping functions. Credit to the original [matlab code](https://users.cs.duke.edu/~cynthia/code.html) goes to [Cynthia Rudin](https://users.cs.duke.edu/~cynthia/) and [Siong Thye Goh](http://web.mit.edu/stgoh/www/mypage/siongthye.html) for their [Paper](https://arxiv.org/pdf/1403.3378v2.pdf) from 2014.

The use of Exactbox requires [gurobi](http://www.gurobi.com/) and its [R-API](https://www.gurobi.com/documentation/7.0/refman/installing_the_r_package.html)
## Installation
Install the newest version right from GitHub.
```{r}
library("devtools")
install_github("hendrikpfaff/boxdrawing")
```

## Usage
### Exactbox
The Exactbox-algorithm creates a Mixed integer programming model and tries to find a solution.


### Fastbox
The Fastbox-algorithm uses a heuristic approach, characterizing and then discriminating elements for its boxes. 

Both algorithms return a list of 13 different elements:
* __execTime__ - Execution time of the function in seconds.
* __colNum__ - Number of Features in the data set.
* __trainingTP__ - Number of True Positive classifications in the training data for every tradeoff-parameter.
* __trainingFP__ - Number of False Positive classifications in the training data for every tradeoff-parameter.
* __trainingTN__ - Number of True Negative classifications in the training data for every tradeoff-parameter.
* __trainingFN__ - Number of False Negative classifications in the training data for every tradeoff-parameter.
* __testingTP__ - Number of True Positive classifications in the testing data for every tradeoff-parameter.
* __testingFP__ - Number of False Positive classifications in the testing data for every tradeoff-parameter.
* __testingTN__ - Number of True Negative classifications in the testing data for every tradeoff-parameter.
* __testingFN__ - Number of False Negative classifications in the testing data for every tradeoff-parameter.
* __tradeoff__ - All used tradeoff-parameters.
* __lowerIdeal__ - The lower boundary of every dimension per box.
* __upperIdeal__ - The upper boundary of every dimension per box.

### Example
```{r}
library(boxdrawing)

# Execute classifiers.
ebox <- exactboxes(positivetraining, negativetraining, positivetesting, negativetesting, 1, 1, 0.01, varType='C')
fbox <- fastboxes(positivetraining, negativetraining, positivetesting, negativetesting, 1,1, 1)

# Compare found box boundaries.
printBoundaries(ebox)
printBoundaries(fbox)

# Apply the boundaries on the data.
applyExact <- applyBoundaries(ebox, data)
applyFast <- applyBoundaries(fbox, data)
```
