# linear models

# expressing design formula in R
# calculated fitted values (predicted y based on betahat: Y = Xbeta); use lm() to perform same function (solve for ordinary least squares (OLS))
# lm() uses model.matrix to do the above^; use model.matrix() and formula() to help interpret lm() results

group <- factor(c(1,1,2,2)) # categorise vector values into groups (1, 2); i.e. do not treat 1s and 2s as 'numerical' (this applies to string text e.g. chow, hf)
# group 1 treated as reference (numerical/alphabetical order, unless specified with relevel(), see below)
model.matrix(~ group) # use tilde~ to  model based on each group (1, 2)
# ~ tilde already tells R that 'group' is a formula

# or, exclude intercept and only include 0s or 1s for each group, e.g. chow, hf, lf etc...
model.matrix(~group + 0)
# highlights which data/mice are +1 for which diet


dat <- read.csv("femaleMiceWeights.csv")
stripchart(dat$Bodyweight ~ dat$Diet,
           vertical = TRUE, method = "jitter",
           main = "Bodyweight over Diet")
levels(dat$Diet) # filter variables in Diet: chow, hf. default alphabetical order: 'chow' is the reference (ref)
X <- model.matrix(~ Diet, data = dat) # builds matrix X, intercept = 1
colnames(X)
# intercept = 1 (col1), Diethf (col2). No Dietchow bc chow is ref (intercept).

# connecting 2 variables together: diet and sex
diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
table(diet,sex)
# 1 for each positive data / satisfies criteria for diet/sex
model.matrix(~ diet + sex) # model assumes same diet effect size for males/females, i.e. one beta1 variable (either only hf or non-hf (chow)) as variable
# intercept = 1 (representing diet1 and sexf as baseline/ref); diet2 and sexm in matrix (non ref as 0,1)

# if expecting that there *is* a difference in diet effect for males/females, then you're introducing a different/new beta coefficient --> creates new column in matrix
model.matrix(~ diet * sex) # investigating interaction between diet effect and sex
# introduces col diet2:sexm:i.e. 1 = both male and diet2 (non-ref: hf) positive --> introduced new beta coefficient (beta3) --> extra 'adjustment' value for difference between male/female --> highlights difference in diet effect between sex

# relevelling: change reference level (if control not already as ref/baseline)
food <- factor(c("hf", "lchow", "hf", "lchow")) # chow not as ref (alphabetically lesser than h) --> appears as col2; hf acts as ref
food <- relevel(food, "lchow") # relevel it to chow as ref
model.matrix(~food)
# or, explicitly define levels with:
food <- factor(food, levels = c("lchow", "hf"))

# matrix modelling with continuous data
tt <- seq(0,3.4,len=4) # plot range 0-3.4 in 4 intervals 
model.matrix(~ tt + I(tt^2)) # model matrix: intercept = 1, tt against each interval, I(tt^2) (i.e. I() treat tt^2 as is) as squared tt col

## ----------------------------------------------------------------------------

# linear models in practice

head(dat)
stripchart(dat$Bodyweight ~ dat$Diet,
           vertical = TRUE, method = "jitter", # distinguish points
           main = "Bodyweight over Diet")
# hf reaches larger bodyweight values, although more spread, vs chow

# before modelling matrix, distinguish levels (intercept / reference levels:)
X <- model.matrix(~Diet, data=dat) # segregate based on Diet types (~ tilde)
colnames(X) # dietchow already intercept (ref) = control variable (for comparison to other variables: Diethf)
# or, relevel data column with: dat$Diet <- relevel(dat$Diet, ref = "chow")
# then model matrix again (Above^)

# run and fit linear model
# w/ one variable:
fit <- lm(Bodyweight ~ Diet, data = dat) # lm finds the minimal RSS (best beta0 intercept for lowest RSS) --> fits beta0 and beta1 against x values (matrix X) for predicted y values
summary(fit) # 1st column (estimate std) = intercept; Diethf = 3.021 (difference between chow vs hf)
coef(fit) # just intercept + diethf coefficients
# t-statistic in t-test is the same as t-value in summary(fit). t-test calulates difference in means between groups / SE noise (specific to each group),
# but lm() divides the mean difference (coefficient of hf, i.e. distance from beta0 to beta1) / overall SE (factors all groups' noise, more general --> better estrimate of residual error)

# additive effect: combination of two groups results in higher fitted y values: add coefficients (beta0 + beta1 + beta2)
# interaction effect: actual value is higher than predicted value above^ (extra y value from interaction between variables)

## ----------------------------------------------------------------------------

# standard errors

# betahat = solve((t(X)%*%)X) %*% t(X)%*%y; uses observed y values to get least squares estimate (betahat)
# predicted y = Xbetahat
# because betahat is a prediction value based on random variable y (observed values will have *noise*, e.g. measurement errors) --> betahat is also random variable
# this produces different coefficient values with every repeat experiment (new set of observed y values)

library(UsingR)
x <- father.son$fheight
y <- father.son$sheight
n <- length(y)
# assume father.son dataset represents entire population

N <- 50
B <- 1000
# montecarlo simulation, collecting random variable betahat coefficients
betahat <- replicate(B, {
  index <- sample(n, N)
  sampledat <- father.son[index, ]
  x <- sampledat$fheight
  y <- sampledat$sheight
  lm(y~x)$coef

})
# replicate() produces matrix (2length, 1000 wide) bc lm() produces 2 values in pairs (intercept, x)
betahat <- t(betahat) # use t() to transpose the matrix to 2 columns and 10000 rows
head(betahat)

qqnorm(betahat[,1]) # [,1] takes first column ([r,c]): intercept
qqline(betahat[,1])
qqnorm(betahat[,2])
qqline(betahat[,2])

# covariance: correlation * standard deviation of each group
# covariance can tell *direction of trend, but not the strength of cor
# ... use correlation coefficient (x,y) = cov(x,y) / √var(x)√var(y) (aka sqr root of variance is sd, as above^)


# in 1dimensional data (e.g. son height), can calculate sd of the y values (height)
# in 2dimensional data, sd of y values doesn't reflect effect of father height on son height
# must use residuals. residuals is the equivalent of predicted epsilon (epsilonhat) (variance from regression line)
# if Y = Xbeta + epsilon, then residuals = Y - Xbeta
# lm() calculates residuals as above --> predicted standard errors --> validate confidence of predicted betahat values

# standard error exercises

N <- 50
n <- length(y) # number of rows of f/sheight pairs
set.seed(1)
index <- sample(n, N) # take 50 rows (from n, above)
sampledat <- father.son[index, ]
x <- sampledat$fheight
y <- sampledat$sheight #***Yi (observed), calculated from sample above (N50)
betahat <- lm(y~x)$coef
betahat

## randomness from Y comes from epsilon (assuming Xbeta is fixed). estimate variance f epsilon from residuals via fitted Y value
# if Y = Xbeta + epsilon, then residuals = Y - Xbeta
fit <- lm(y ~ x)
hatY <- fit$fitted.values #***hatYi (expected)
## ** what is the sum of the squared residuals (if *individual residuals ri = Yi - hatYi) **
r <- y - hatY # residual
r2 <- sum(r^2)
r2 # = 133.3169

# use RSS (residual sum squared r2) to calculate variance (sigma^2): var = RSS / n-p, where p = number of terms (2: intercept and slope, don't include them in division bc these act as the reference itself for variance)
# overall/estimated variance (sigma^2) = overall RSS / n-p
var <- r2 / 48
# (var = sigma^2)
# estimate var(betahat) with: sigma^2 var (above estimate) * solve((t(X)%*%X)) aka (X^T*X)^1

# ** Form the design matrix X (Note: use a capital X). This can be done by combining a column of 1’s with a column containing ‘x’ , the fathers’ heights. **
N <- 50
X <- cbind(rep(1,N), x)
## or use model.matrix
## X <- model.matrix(~x) # X matrix = sampled fheights above
X
## ** Now calculate (X⊤X)−1. Use the solve function for the inverse and t for the transpose. What is the element in the first row, first column? **
X2 <- solve(t(X)%*%X)
head(X2)
# pull diag values to extract only intercept variance, and x(slope) variance (the other values = covariance)
varbeta <- var * diag(X2) # returns variance of betahat
sqrt(varbeta) # returns *standard error* of betahat (se for estimates, otherwise sd)
# = intercept: 6.808565, y: 0.103687

summary(fit) # performs the above statistics for standard error