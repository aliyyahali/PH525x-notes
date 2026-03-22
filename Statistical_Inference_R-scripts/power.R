View(babies)
#filter nonsmoke babies
bwt.nonsmoke <- filter(babies, smoke == 0) %>%
  select(bwt) %>%
  unlist()

bwt.smoke <- filter(babies, smoke == 1) %>%
  select(bwt) %>%
  unlist()

mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

## Q: are baby bwt of smoking mothers significantly diff than non-smoking mothers?

set.seed(1)
dat.ns <- sample(bwt.nonsmoke, 25)
dat.s <- sample(bwt.smoke, 25)
tval <- t.test(dat.ns, dat.s)$p.value
tval

# in relatively large samples, and where null hypothesis is true, there is usually a normal distribution
# when sampling, random variable 'tval' may fall within the normal distribution
# perform 2-tailed t.test to summarise area under / boundary of curve with pnorm()
# pnorm() only calculates deviation/extremes of ONE tail, so must *2 for BOTH tails (two-sided)
# ... 2*pnorm(-abs(tval)
 
# Our estimate of the difference between babies of smoker and non-smokers: mean(dat.s) - mean( dat.ns). If we use the CLT, what quantity would we add and subtract to this estimate to obtain a 99% confidence interval?
mean(dat.s) - mean(dat.ns)
# calculate standard error SE to calculate difference between 2 means (dat.s vs dat.ns)
se <- sqrt(var(dat.s)/length(dat.s) + sqrt(var(dat.ns)/length(dat.ns)))
se
# SE = 4.210214, multiply  by qnorm(0.995) to get the 0.5% extreme in each tail
clt <- qnorm(0.995)*se
clt
# = 10.84479

# * If instead of CLT, we use the t-distribution approximation, what do we add and subtract (use 2*N-2 degrees of freedom)?

# when sample size is small, it adds variability. t-distribution (qt()) is used to account for excess varaibility (with normal distribution populations)
N <- length(dat.ns) + length(dat.s)
# df = N (total sampel size - 2)
# df is a multiplier that decides how big the bell curve should be: smaller samples will have larger tails (more uncertainty)
qt <- qt(0.995, N - 2)
# this calculates how many SE away until you would reach the 0.5% extreme in each tail
se <- sqrt(var(dat.s)/length(dat.s) + sqrt(var(dat.ns)/length(dat.ns)))
# SE calculates how close the sample mean is vs the true population mean (i.e. factors noise)
qt * se
# returns baby bodyweight value - add/subtract this value to reach the 99% end, or 0.5% extremes in each tail

# * Why is calculating with clt (qnorm()) vs t-distribution (qt()) similar?
# bc they both calculate confidence intervals, under normal distribution, but qt() factors in higher variability.
# sample size is also large enough that bell curve is similar. as df is a reflection of sample size, they both have similar results
# ANS: N and thus the degrees of freedom is large enough to make the normal and t-distributions very similar.

# * Power is one minus the Type II error rate, or the probability that you will reject the null hypothesis when the alternative hypothesis is true.
# * Type II error control plays a major role in designing data collection procedures before you actually see the data, so that you know the test you will run has enough sensitivity or power
# hence lower alpha (for p-value threshold) decreases power (for a given effect size)
# but lower alpha means harder to reject null (need smaller p-value), which increases Type I errors (rejecting null when actually true (alternative))
# this is a trade-off:
set.seed(1)
smoke <- sample(dat.s, 5)
nonsmoke <- sample(dat.ns, 5)
t.test(smoke, nonsmoke)$p.value

N <- 120
alpha <- 0.01
set.seed(1)
pval <- replicate(10000, {
  smoke <- sample(bwt.smoke, N)
  nonsmoke <- sample(bwt.nonsmoke, N)
  t.test(smoke, nonsmoke)$p.value
})
# replicate simulation 10,000x, obtain 10,000 t.test p values
mean(pval < alpha)
# obtain % of t.test p values that are < 0.05
# aka mean(pval < alpha) is equivalent to: power = 1 - beta (where beta = total number of NON-significant p values)


