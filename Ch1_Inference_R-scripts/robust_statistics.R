## Robust statistics

# pearson measures correlation between two variables - does not account for outliers, and may drive cor artificially up/down.
# spearman correlation accounts for outliers: e.g. nym.2002() marathon runners:
# runners are ranked on their age and finish times: raw data values/outliers have no impact, only depicts their rank

# if x <- age, y <- time:
# median x (average age rank) and median y (average time rank) would be expected for the average runner.
# if runner has median x (average age rank) but lower time rank (y) ==> faster finish times for their age.

# spearman can highlight differences in expected/average outcomes via rank
# but cannot highlight variances in the data (no raw values, just rank): use MAD to explain variance within strata/group

library(UsingR)
data(ChickWeight)
View(ChickWeight)
head(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col = ChickWeight$Diet) # colour code on for different diet types (1-4)
# currently clustering all chick diets into one time column. rearrange chick weights per diet into each row
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time", # timevar establishes labels for columns (i.e. chick weight per time point)
                direction="wide")
head(chick)
chick = na.omit(chick) # clean

## ** Focus on the chick weights on day 4 (check the column names of ‘chick’ and note the numbers). How much does the average of chick weights at day 4 increase if we add an outlier measurement of 3000 grams?
## Specifically, what is the average weight of the day 4 chicks, including the outlier chick, divided by the average of the weight of the day 4 chicks without the outlier.
## Hint: use cto add a number to a vector. **

x <- select(chick, weight.4) %>% # select day 4 weights
  unlist()
mean(x) # = 60.16
y <- c(x, 3000) # add outlier to vector via c()
mean(y) # = 124.07
mean(y) / mean(x) # = 2.06, i.e. outlier increases mean by 2x. mean is highly sensitive to outliers.

## **Compute the same ratio, but now using median instead of mean. **
median(y) / median(x) # = 1, i.e. median not sensitive to outliers ==> 'robust'
sd(y) / sd(x) # = 101.29, i.e. 100x larger sd than original/without outlier (x)
mad(y) / mad(x) # = 1, i.e. unaffected (even though MAD is x1.4826 to standardise with sd)

x21 <- select(chick, weight.21) %>%
  unlist()

plot(x, x21) # general rising trend: large weight on day 4 --> also large weight on day 21
cor(x, x21) # no outliers = 0.416
## **Now calculate how much the Pearson correlation changes if we add a chick that weighs 3000 on day4 and 3000 on day 21. **
y21 <- c(x21, 3000) # add outlier do day 21 weight
plot(y, y21)

cor(y, y21, method = "pearson") # = 0.986 --> 1 outlier artificially driving cor up to strong correlation
cor(y, y21, method = "spearman") # 0.467 --> weak positive correlation
cor(y, y21) / cor(x, x21) # = 2.37: outlier drives correlation 2x higher

## *Save the weights of the chicks on day 4 from diet 1 as a vector a. Save the weights of the chicks on day 4 from diet 4 as a vector b. **
a <- filter(chick, Diet == "1") %>%
  select(weight.4) %>%
  unlist()
b <- filter(chick, Diet == "4") %>%
  select(weight.4) %>%
  unlist()
## ** Perform a t-test comparing x and y (in R the function t.test(x,y) will perform the test). **
t.test(a, b)$p.value
## **Then perform a Wilcoxon test of x and y (in R the function wilcox.test(x,y) will perform the test). A warning will appear that an exact p-value cannot be calculated with ties, so an approximation is used, which is fine for our purposes. **
wilcox <- wilcox.test(a,b)$p.value
# Wilcox test = robust statistic, generates U statistic based on rank
aOut <- c(x, 200) # add 200g outlier to Diet 1 chicks
t.test(aOut, b) # compare with Diet 4 chicks (no outlier)
# p-value goes from < 0.05 --> 0.7; t.test very sensitive to outliers
wilcox.test(aOut, b) # = 0.008, i.e. still < 0.05, Mann-Whitney (Wilcoxon) not affected by outlier

library(rafalib)
mypar(1,3)
boxplot(a,b)
boxplot(a,b+10)
boxplot(a,b+100) # shifts all values in vector b up by +100, making difference betwen Diet 1 and Diet 4 chicks more prominent
t.test(a, b+10)$statistic - t.test(a, b+100)$statistic # = 67.75 t-statistic, i.e. the difference between b+10 to b+100 significantly inflates vector b sample means
# as t.test measures the difference between sample means + considering noise (diff / SE), the t-statistic grows bc sample mean is also growing.
# therefore +67.75 diff in t-stat indicates a huge difference from vector a (diet 1 chicks) --> i.e. *t-statistic is sensitive to magnitude of mean values*

# vs Wilcoxon: **Because the Wilcoxon works on ranks, once the two groups show complete separation, that is all points from group ‘y’ are above all points from group ‘x’, the statistic will not change, regardless of how large the difference grows **
wilcox.test(a, b+10)
wilcox.test(a, b+100)
# produces same p.value, ie diff = 0 --> robust statistic, not affected by inflated b values (diet 4 chicks)
# however Wilcox has minimum p-value, so not as powerful as t-test
wilcox.test(c(1, 2, 3))$p.value - wilcox.test(c(4,5,6))$p.value # = 0 (both produce same p-value of 0.25, despite sample size difference)
wilcox.test(c(1,2,3))$p.value - wilcox.test(c(400, 500, 600))$p.value # = 0 (both still produce same p-value)
## --> wilcox not suitable for small sample sizes. not suitable for measuring sample magnitude between groups. more suitable where 'rank' is used, e.g. marathon finishing places