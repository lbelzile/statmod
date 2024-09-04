# Slides 1, MATH 60604A


# Graphics library
library(ggplot2)

## Download package containing data
# remotes::install_github("lbelzile/hecstatmod")
## Load database
data(distraction, package = "hecstatmod")
# Compute paired t-test
ttest <- with(distraction,
              t.test(x = t,
                     y = c,
                     paired = TRUE,
                     alternative = "greater"))
# Equivalent to one sample t-test for difference
ttest <- with(distraction,
              t.test(x = t - c,
                     alternative = "greater"))
# Extract p-value and confidence interval
ttest$p.value
ttest$conf.int

# Permutation-based approach to t-test
# To establish a benchmark null distribution
# Under H0, the average observation falls on either sign
# Make the signs arbitrary
di <- with(distraction, t - c)
n <- length(di) # sample size
B <- 1e4 # number of replications for permutation
ttest_stats <- numeric(B) # create empty container
ttest_stats[1] <- t.test(di, alternative = "greater")$statistic
set.seed(20200608) # set seed of pseudo-random number generator
for(i in 2:B){
  # Recalculate the test statistic, permuting the signs
  ttest_stats[i] <- t.test(sample(x = c(-1, 1),
                                  size = n,
                                  replace = TRUE) * di,
                           alternative = "greater")$statistic
}
# Plot the empirical permutation distribution
ggplot(data = data.frame(statistic = ttest_stats),
       aes(x=statistic)) +
  geom_histogram(bins = 30, aes(y=after_stat(density)), alpha = 0.2) +
  geom_density() +
  geom_vline(xintercept = ttest_stats[1]) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.1))) +
  labs(y = "density", x = "statistic") +
  stat_function(fun = dnorm, linetype = "dashed")


# Compute test manually (just to check formulae)
d <- with(distraction, t - c) # time difference text vs conversation
n <- length(d) # sample size
(mean_d <- mean(d)) # mean difference
(se_d <- sd(d)/sqrt(n)) # standard error of sample mean
(stat <- mean_d/se_d) # t-test statistic
dof <- n - 1L # degrees of freedom
crit <- qt(p = 0.05, df = dof) # critical value, "q" for quantile
(pval <- pt(q = stat, df = dof, lower.tail = FALSE)) # Pr(T > stat)
(conf_low <- mean_d + se_d*crit) # lower bound of Wald confidence interval
# Since it's a one-sided interval, the upper bound is infinite

# Not all tests in R have the same output format
# the "broom" package function "tidy" standardizes names
# and makes things tidy (puts results in a data frame)
broom::tidy(ttest)
