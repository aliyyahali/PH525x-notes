## Matrix algebra and linear regression


# generating matrix
X <- matrix(c(1,3,2,1,-2,1,1,1,-1), 3, 3)
beta <- c(3,2,1) # vector

# multiplying X by vector (beta)
X%*%beta
# = matrix(c(6, 6, 7), 3, 1)
# each row value in matrix X is multiplied by each value in vector beta (i.e. matrix row n1 * vector column n1; matrix row n2 * vector column n2...) ==> take sum of each row

X <- matrix(c(1,3,2,1,-2,1,1,1,-1), 3, 3)
Y <- matrix(c(6, 2, 1), 3, 1)
solve(X,Y). # or solve(X)%*%Y
# i.e. how to get from matrix X to matrix Y?
# ans = matrix(c(1,2,3), 3, 1) --> multiply by X to achieve matrix Y

# solve(X)%*%Y is equivalent to solving for X from equation y = XBeta + epsilon.
# rearrange for beta (the relationship between variables): Beta = X^-1Y. ignoring epsilon (unknown noise) means Beta is an estimate (==> Betahat)
# but the inverse X^-1 can only be used on square matrices. to make rectangular matrices square (before solve(X)), use (X^TX)^-1X^T

library(UsingR)
y <- father.son$sheight
print(mean(y))

N <- length(y)
Y <- matrix(y, N, 1) # matrix of son heights, nrow = N (number of recorded son heights), ncol = 1
A <- matrix(1, N, 1) # matrix of 1s, ncol = 1
barY <- t(A) %*% Y / N # transpose of a (t(A)) converts the matrix of 1s into 1 row. multiply this by matrix Y (son heights), then divide by N (no. of son height values)
# ==> i.e. calculated *mean* via matrix algebra
# or, barY <- crosspod(A,Y)/N


# Y = XBeta + epsilon
# X = matrix with vector data against 1 or more variables (beta coefficients)
# Beta coefficient determines slope of line. If jusy Y = X(Betahat0), see horizontal straight line. If Y = XBeta1~, see slanted line. If more than one beta coefficient (betahat1, betahat2...), this is already included in the X matrix, and adds more curve (acceleration) to the graph
# total sum of squares (tss): square each data point to the mean line (horizontal x line), then sum to see overall variance from the mean. the larger the tss, the higher the variance.
# residual sum of squares (rss) introduces variables (beta coefficients) to better match the data trend with a regression/predictor line.
# RSS = distance of each point from regression / predictor line > squared > sum of each residual squares ==> RSS

# minimising sum of squares: (Y - XBeta)^T(Y-XBeta) --> solve for Beta (coefficent/slope) --> Betahat = (X^TX)^-1(X^TY) --> i.e. betahat is an estimate bc epsilon (error) is not accounted for
library(UsingR)
x = father.son$fheight
y = father.son$sheight
X <- cbind(1,x) # create matrix X of father heights, listed in 1 column
# Betahat = (X^TX)^-1(X^TY)
betahat <- solve(t(X)%*%X)%*%t(X)%*%y # square matrix X via transpose(X) * X > then multiply transposed matrix X by vector y (son heights)
# solve completes the above formula with with inverse (i.e. solving for betahat)
# aka matrix X * transposed matrix X is equivalent to 'crossprod' function:
betahat <- solve(crossprod(X)) %*% crossprod(X,y)
# 2 values returned: 33.88 (row 1) = betahat0 (y-intercept); 0.514 = betahat1 = x (slope)

newx <- seq(min(x),max(x),len=100)
X <- cbind(1,newx) # create matrix for line so that you can multiply with matrix X
fitted <- X%*%betahat
plot(x,y,xlab="Father's height",ylab="Son's height")
lines(newx,fitted,col=2)

# rss example: falling object
g <- 9.8
n <- 25
tt <- seq(0,3.4, len = n) # generate seq of 25 value, ranged 0 - 3.4
f <- 56.67 + 0*tt - 0.5*g*tt^2 # physics formula
y <- f + rnorm(n, sd = 1) # 'f' would follow perfect parabola - rnorm() creates error/noise values --> add to each value in f sequence --> generates imperfect parabola
plot(tt, y, xlab = "Time in secs", ylab = "Distance in metres")
lines(tt, f, col=2)

rss <- function(beta0, beta1, beta2) {
  r <- y - (beta0 + beta1*tt+beta2*tt^2) # subtract predicted(betas) from actual data (falling heights 'f')
  sum(r^2)
}

# apply rs function to a sequence of values (beta2s) to find the best possible value to represent gravity
# i.e. solving for 'g', but by using rss:
beta2s <- seq(-10, 0, len = 100) # generate sequence of potential gravity values
RSS <- sapply(beta2s, rss, beta0=55, beta1=0) # apply rss function to each beta value; based on fixed values above (beta0 = y-intercept, beta1 = slope) to solve for g
# RSS generates a value for each number in the beta2s seq --> the lower the rss value, the better the predicted value ==> 'minimising residual sum of squares'  
plot(beta2s, RSS, type = "l")
# you can repeat for different beta0 values to find the best, minimal RSS --> but lm() function does that to find the minimal rss
tt2 <- tt^2
fit <- lm(y~tt+tt2) # actual data against 2 variables (variables are *not* added together)
summary(fit)

# minimsing rss via matrix algebra (RSS above uses lm(), produces same value as RSS below)
X <- cbind(1, tt, tt^2)
head(X)
Beta <- matrix(c(55, 0, 5), 3, 1) # create matrix of predicted beta coefficients (55, 0, 5)
r <- y - X %*% Beta # generate residuals: multiply X matrix (data) by each beta coefficient(Beta) > subtract from y (actual data, generated above with rnorm())
RSS <- t(r) %*% r # residual sum squared (transpose r matrix > multiply by r matrix)
# or
RSS <- crossprod(r) # use crossprod to square the same matrix
# solve for betahat: betahat = (t(X)*X)(t(X)*Y)^-1
betahat <- solve(t(X)%*%X) %*% (t(X) %*% y) # first bracket squares matrix X > second bracket integrates data into X matrix > multiply both matrices (complete the formula)
# or
betahat <- solve(crossprod(X)) %*% crossprod(X,y) # solve for betahat
betahat # produces same value as above 'fit' variable

# example questions
## ** What is the average height of the sons (don’t round off)? **
mean(father.son$sheight) # = 68.68407
## ** Create a list of son heights for sons that have fathers with heights of 71 inches, rounding to the nearest inch. **
View(father.son)
son <- (father.son %>%
  mutate(roundf = round(fheight)) %>% # create variable 'roundf'
  filter(roundf == 71) %>% # all values rounded, pull just 71inch fheights
  select(sheight) # pull sheight only
)
head(son) # son data not rounded, untouched
## ** What is the mean of the son heights for fathers that have a height of 71 inches (don’t round off your answer)? Hint: use the function round on the fathers’ heights. **
print(mean(unlist(son))) # = 70.54082 (similar to fheight of (rounded) 71inch)

## ** Suppose you model the relationship between weight and height across individuals with a linear model. You assume that the height of individuals for a fixed weight x follows a linear model Y=a+bx+ε. Which of the following do you feel best describes what e represents? **
## D) Between individual variability: people of the same height vary in their weight.

# matrix notation exercises
X = matrix(1:1000,100,10)
## ** What is the entry in row 25, column 3? **
X[25, 3] # nrow, ncol
# = 225
# or X[1:5] to pull all data from row 1-5; X[1:5, 3] to pull data row 1-5 in col 3; X[,3] to pull data in col 3 only
X[, 3]

## ** Using the function cbind, create a 10 x 5 matrix with first column x=1:10. Then add 2*x, 3*x, 4*x and 5*x to columns 2 through 5. What is the sum of the elements of the 7th row? **
x = 1:10
Y = cbind(x, 2*x, 3*x, 4*x, 5*x) # create matrix of variables (5 different vectors)
Y[7,] %>% # pull values in row 7
  sum
# = 105

## ** Which of the following creates a matrix with multiples of 3 in the third column? **
A <- matrix(1:60,20,3)
col3 <- A[, 3]
mean(col3 %% 3 == 0) # i.e. does dividing col3 values by 3 leave remainder of 0? > all true = multiples of 3 in 3rd column
A <- matrix(1:60,20,3,byrow=TRUE) # ==> returns all true > mean = 1 (100%)

## solve for c (4 equations, a-d)
# input all coefficient values:
J <- matrix(c(3, 4,-5, 1, 2, 2, 2, -1, 1, -1, 5, -5, 5, 0 , 0 , 1), nrow = 4, byrow = TRUE) # read by row when *generating* matrix, but read by column when *reading* matrix
b <- c(10,5,7,4) # vector of answers for each equation
z <- solve(J,b)
print(z)

## ** What is the value in the 3rd row and the 2nd column of the matrix product of a and b? **
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
z <- a %*% b
z[3, 2] # = 113

## ** Multiply the 3rd row of a with the 2nd column of b, using the element-wise vector multiplication with *. **
a[3, ] * b[, 2] # = 12, 35, 66
## ** What is the sum of the elements in the resulting vector? **
sum(a[3, ] * b[, 2]) # = 113



X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
## ** Suppose that the fitted parameters for a linear model give us: **
beta <- c(5, 2)
## ** What is the fitted value for the A samples? (The fitted Y values.) **
Xa <- X[1:2, ] # = a
Xb <- X[3:4, ] # = b
# ie use Y = Xbeta (Y = predicted outcomes based on beta coefficients + data)
Ya = Xa%*%beta # = 5, 5
## ** What is the fitted value for the B samples? (The fitted Y values.) **
Yb = Xb%*%beta # = 7, 7
Yb

## ** Suppose now we are comparing two treatments B and C to a control group A, each with two samples. This design can be represented with a model matrix like so: **
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
X
## ** Suppose that the fitted values for the linear model are given by: **
beta <- c(10,3,-3)
## ** What is the fitted value for the B samples? **
X
Xa <- X[1:2, ]
Xb <- X[3:4, ]
Xc <- X[5:6, ]
Yb = Xb%*%beta
Yb # 13, 13

## ** What is the fitted value for the C samples? **
Yc = Xc%*%beta
Yc # = 7, 7