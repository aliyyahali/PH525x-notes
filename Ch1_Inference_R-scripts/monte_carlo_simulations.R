# Using Monte Carlo simulations to compare differences in CLT and t-distribution with different sample sizes

dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex == "F", Diet == "chow") %>%
  select(Bodyweight) %>%
  unlist()

# modelling null: compare 2 of the same variables to confirm/model the null
# i.e. compare 2 tvals between 2 control variables
# function: generates t-statistic (t-val) for sample size 'n'
ttestgenerator <- function(n){ 
  cases <- sample(controlPopulation, n)
  controls <- sample(controlPopulation, n)
  # tstat = (mean obsdiff) / (popsd / √n); or tstat = (obsdiff) / SE
  tstat <- mean(cases) - mean(controls) /
    sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}

# with sample size of 10, repeat ttestgenerator function x1000
ttests <- replicate(1000, ttestgenerator(10))
hist(ttests)
# shows bell shape curve, implies normal distribution
qqnorm(ttests)
qqline(ttests)
abline(0,1)

# compare with smaller sample size of 3
ttests<- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
qqline(ttests)
abline(0,1)
# more elongated tails / large quartile deviate away from normal distribution
# CLT not a suitable approximation for small sample sizes

# t-distribution more suitable for small sample sizes if population follows normal distributionqt()
ps <- (seq(0,999)+0.5)/1000 # generate sequence from 0 - 1000
qqplot(qt(ps, df = 2*3-2), ttests, xlim = c(-6, 6), ylim = c(-6,6)) # df = 2*n-2. Compare sample size of 3 from CLT to here (t-distribution), n = 3.
abline(0,1)

qqnorm(controlPopulation) # compare to entire population data - see that original data is not well approximated by normal distribution due to larger large quantiles (t-distribution may no be sutiable)
qqline(controlPopulation)

# Monte Carlo simulation above involved generating random samples from population data, but whole population is not realistically accessible
# instead, generate population data via parametric simulation: take parameters from population data, and apply normal distribution:


# ** For the case of weights, we could use our knowledge that mice typically weigh 24 ounces with a SD of about 3.5 ounces, and that the distribution is approximately normal, to generate population data:
controls <- rnorm(5000, mean = 24, sd = 3.5)
ttestgenerator <- function(n, mean=24, sd=3.5) { # add parameters to above ttestgenerator - generates random values and tvals within population data range of mean 24 with sd = 3.5
  cases <- rnorm(n,mean,sd)
  controls <- rnorm(n,mean,sd)
  tstat <- (mean(cases)-mean(controls)) / 
    sqrt( var(cases)/n + var(controls)/n ) 
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)

# * Monte Carlo simulation exercises
set.seed(1)
sample <- rnorm(5) # rnorm() parameters already assume normal distribution with mean = 0 sd = 1
t <- sqrt(5) * mean(sample) / sd(sample) # rearranged tval formula / t-statistic formula
t # tval closer to 0 is true mean of normally distributed population 't' variable produced 0.3 - close to true mean of 0

# * Set the seed to 1, generate B=1000 t-statistics as done in exercise 1. What percent are larger than 2?
set.seed(1)
B = 1000
monte <- function(n) {
  sample <- rnorm(n)
  t <- sqrt(n) * mean(sample) / sd(sample)
  return(t)
}
carlo <- replicate(B, monte(5))
mean(carlo > 2) # generate p value (total number of t values > 2)
# = 0.043

# * The answer to exercise 2 is very similar to the theoretical prediction: 1-pt(2,df=4). We can check several such quantiles using the qqplot function.
# generate percentiles 1 > x > 0
C <- 100
ps = seq(1/(C+1), 1-1/(C+1),len=C) # generate 100 evenly spaced percentiles (avoid true 0 or true 1 to avoid infinity values in plot)
theory <- qt(ps,df=4) # qt() generates model tval for each percentile in the sequence ps(), under normal distribution with df = 4.
# at 50th percentile (0.5), the tval would equal 0 (ie perfect median. t-distribution is symmetrial, hence = 0)
# at extreme percentile (0.975), tval equals > 2 etc.
qqnorm(theory)
qqplot(carlo, theory,
       xlim = c(-6, 6), ylim = c(-6,6)) # near perfect, linear line. few anomalies: total 3 in large quantiles (due to rnorm() sample generation extreme random variables?)


# * Use Monte Carlo simulation to corroborate that the t-statistic comparing two means and obtained with normally distributed (mean 0 and sd) data follows a t-distribution.
# * In this case we will use the t.test function with var.equal=TRUE. With this argument the degrees of freedom will be df=2*N-2 with N the sample size. For which sample sizes does the approximation best work?
N <- 5 # repeat with different sample sizes
t_stat <- replicate(1000, {
  sampleA <- rnorm(N, mean = 0 , sd = 1)
  sampleB <- rnorm(N, mean = 0, sd = 1)
  tstat <- t.test(sampleA, sampleB, var.equal = TRUE)
  return(tstat$statistic)
})
ps = seq(1/(N+1), 1-1/(N+1),len=C)
theory <- qt(ps, df = 2*N-2) # t.test var.equal = TRUE already has this df parameter - generate same parameters here for comparison
qqplot(t_stat, theory)
# repeated with range of sample sizes N = 5 - 100; larger sample sizes produces more normally distributed (linear) data
# smaller sample sizes had larger quantiles