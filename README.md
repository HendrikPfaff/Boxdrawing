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

## Example


