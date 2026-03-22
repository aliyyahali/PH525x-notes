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
