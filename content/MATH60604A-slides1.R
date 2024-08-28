# Slides 1, MATH 60604A

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
