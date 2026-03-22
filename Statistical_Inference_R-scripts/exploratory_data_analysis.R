library(UsingR)
View(father.son)
x <- father.son$fheight # univariate data: height + frequency
round(sample(x,20),1) # large dataset of 1000 values; take a smaller sample and round to 1 dp to visualise dataset
hist(x, breaks = seq(floor(min(x)), ceiling(max(x))), main = "Father heights", xlab = "Heights (inch)") # define dimensions of hist, idea of proportions of data
# general normal distribution, bell curve, 

# empiricial cummulative distribution: explains what % are below a certain threshold
xs <- seq(floor(min(x)), ceiling(max(x)), 0.1)
plot(xs, ecdf(x)(xs), type  = "l", main = "Father heights", xlab = "Heights (inch)")

# if distribution of heights appox normal: take mean, sd
# to find proportion of individuals above (right tail) x threshold:
mean(x>70) # empirical result = 0.21
1 - pnorm(70, mean(x), sd(x)) # normal approximation predicts similar = 0.20
# can repeat with diff threshold, to see how normal approx vs empirical (mean()) differ --> i.e. *qqplot* systematically performs this theory/function

ps <- seq(0.01, 0.99, 0.01) # generate percentiles: range from 0.01 to 0.99, go in steps of 0.01 (defines resolution of graph)
qs <- quantile(x, ps) # generate quantile plot against specified percentiles (seq)
normalqs <- qnorm(ps, mean(x), sd(x)) # generate qqplot against (same) specified percentiles via normal approx (considers mean and sd)
plot(normalqs, qs, xlab = "Normal percentiles", ylab = "Height percentiles")
abline(0,1) # reflects true normal distribution, for comparison
# normal distribution is very good at predicting heights, very close to abline --> use *qqnorm* to systematically perform the above theory:
qqnorm(x)
qqline(x)

## when data is not normally distributed, mean() and sd() not suitable
hist(exec.pay) # heavily skewed
qqnorm(exec.pay) # fat right tail (values much larger than expected/approximated by normal distribution)
boxplot(exec.pay, ylab = "10,000s of dollars", ylim = c(0,400)) # unlike mean and sd, min, median and max ranges (+outliers) extra information


## 2 dimensional data: show relationships between two variables, each with approx normal distributions
## father height vs son height
x <- father.son$fheight
y <- father.son$sheight
plot(x, y, xlab = "Father height (inch)", ylab = "Son height (inch)")
# not suitable to compute and compare mean and sd for each variable --> use *cor*
cor(x,y) # = 0.5
# if asked to predict son height (univariate data), compute the mean(). but if we know father's height is taller than average: how does it change the son's height prediction?
# --> stratify the data by father heights, to compare against son heights
boxplot(split(y, round(x))) # round father height to closest inch to set fixed intervals; split() groups son height to its respective father height interval
print(mean(y[round(x) == 72])) # mean son height for father height group 72inch == 70.7

## standardise father/son height data via z-score = x - (mean(x)) / sd(x)
x = (x - mean(x)) / sd(x)
y = (y - mean(y)) / sd(y) # z-score: average mean() height is standardised to unit 0. any other value represents a unit of sd away from this 0.
means = tapply(y, round(x*4) / 4, mean) # rounds father height z-score to nearest quarter (1 (0.25), 2 (0.5) or 3 (0.75)) 
## --> calculates mean son height (y) against each father height's quarter group: (1 (0.25), 2 (0.5) or 3 (0.75))
# tapply generates quarters of f.heights as 'numeric' vectors. must convert to numeric:
fatherheights = as.numeric(names(means)) # names(means) = returns the 'columns' i.e. names of the father height quarters from 'means' variable above^
# f.height quarters now numeric, can plot against s.height numeric vectors
plot(fatherheights, means, ylab = "Average strata of son heights", xlab = "Father heights")
abline(0, (cor(x,y))) # correlation / line of best fit / regression

## Exploratory data analysis questions --------------

data("InsectSprays")
View(InsectSprays)

# generating boxplot: which insecticide is most effective?
boxplot(count ~ spray, data = InsectSprays, ylab = "Surviving insects count", main = "Insecticide comparison", xlab = "Spray")
# A, B, F significantly lower insect neutralisation count than C, D, E


data(nym.2002)
View(nym.2002)
# ** Use boxplots and histograms to compare the finishing times of males and females. Which of the following best describes the difference? **
female <- filter(nym.2002, gender == "Female") %>%
  select(time) %>%
  unlist()
male <- filter(nym.2002, gender == "Male") %>%
  select(time) %>%
  unlist()

# finish <- nym.2002 %>%
#  filter(gender %in% c("Female", "Male")) %>%
#  select(gender, time)
#  unlist()

boxplot(female, male, ylab = "finishing times", names = c("female", "male"))
abs(median(female) - median(male))
# median male finish times is 21.7mins less than women, but large IQR overlaps means insignificant difference
# males have more right-tail outliers, i.e. greater finish times --> slower runners; females have fewer, but a larger range of right-tail outliers
# fastest = males (min(males))

hist(female)
# data heavily skewed to left, i.e. quicker finish times / few longer finish times
# data skew reflects on boxplot too: larger finsh times listed as outliers above^
hist(male)
# data more normally distributed than females, but only slightly skewed to left (males generally have more consistently faster finish times)

## **Are males are faster than most women? **
mean(male < median(female)) # = 0.69
# 69% of males have finishing times less than the median (50%) of females, i.e. a significant proportion of males are faster than women

## ** Use dplyr to create two new data frames: males and females, with the data for each gender
## For males, what is the Pearson correlation between age and time to finish? **
males <- filter(nym.2002, gender == "Male")
females <- filter(nym.2002, gender == "Female")

cor(males$age, males$time) # = 0.243
plot(males$time, males$age)
# weak positive correlation
cor(females$age, females$time) # = 0.244
plot(females$age, females$time) # but identified in plot an extreme outlier, possibly driving a higher correlation than expected --> continue below...

## ** If we interpret these correlations without visualizing the data, we would conclude that the older we get, the slower we run marathons, regardless of gender.
aget <- filter(nym.2002) %>%
  select(age, time)
## Look at scatterplots and boxplots of times stratified by age groups (20-25, 25-30, etc..).
min(aget$age)
max(aget$age)
# min age = 5, start stratified age buckets from 5yrs to 81 yrs. Final age bucket range to 85.

## ageStrat <- (round(aget$age / 5) * 5) would use, but doesn't create clean age group intervals
agebrack <- seq(5, 85, by = 5) # must not use alone, otherwise 100s of runner data allocated randomly between limited intervals
# must establish each runner's time and age to a specific age bucket/interval via:
agegroup <- cut(aget$age, breaks = agebrack) # cut() splits aget$age data into its corresponding breaks/intervals. generates agegroup variable: list of bucket/interval names (20-25) etc
stratAge <- boxplot(split(aget$time, agegroup)) # split aget$time data into each respective agebrack, generate boxplot for each brack
## After examining the data, what is a more reasonable conclusion? **
# null: 'the older we get, the slower we run marathons' is not true. no linear relationship:
# marathon finish times are relatively stagnant between ages [15,20] to [55,60].
# however, in age ranges [65,70] to [75,80], it appears slower finish times are more apparant with increasing age.
