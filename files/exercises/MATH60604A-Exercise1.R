library(hecstatmod)
library(ggplot2) # grammar of graphics
library(poorman) # poorman version of dplyr
library(coin) # nonparametric tests

## Exercise 1.1
data(renfe, package = "hecstatmod")
# Select subsample with only high-speed trains
data <- renfe |> filter(type %in% c("AVE","AVE-TGV"))
# Transform duration in hours
duration <- data$duration / 60
# Compute summary statistics of the duration
mean_d <- mean(duration);
# Power simulation
B <- 1e4L # number of simulations
n <- nrow(data)
m <- 100L # subsample size
alpha <- 0.05 # level of test
# Create a grid of duration times for the alternative hypothesis
time <- seq(from = mean_d, to = 3, by = 0.005)
power <- matrix(nrow = B, ncol = length(time)) # container to store results
# fix random number generation seed for reproducibility
set.seed(2020)
for(i in seq_len(B)){ # for each replication
  for(j in seq_along(time)){ # for each alternative mean duration
    # Compute the p-value of the one-sample t-test
     power[i,j] <-  t.test(x = duration[sample.int(n = n, size = m)],
                           mu = time[j])$p.value
     # Store p-values
  }
}
# Plot the power curve as a function of time
ggplot(data = data.frame(time = time,
                         # Proportion of rejection of H0 at level alpha
                         power = colMeans(power < alpha)),
       mapping = aes(x = time, y = power)) +
  geom_point() +
  geom_vline(xintercept = 2.845, linetype = "dashed") +
  geom_hline(yintercept = alpha) +
  labs(x = "average duration time (in hours)",
       y = "",
       subtitle = "Power as a function of the mean alternative time") +
  scale_y_continuous(limits = c(0,1),
                     expand = expansion(add = 0.01)) +
  theme_classic()


# Exercise 1.2
# Coverage rate
summarise(renfe_simu,
          coverage = mean((cilb < -0.28) & (ciub > -0.28)))
# Histogram of mean price difference as a function of destination
ggplot(data = renfe_simu,
       aes(x = meandif)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = -0.28, col = "blue") +
  xlab("mean price difference (in euros)") +
  ylab("count")
# Power of test (alternative is true!)
summarise(renfe_simu, power = mean(pval < 0.05))
# Alternative code
# with(renfe_simu, mean(pval < 0.05))
# mean(renfe_simu$pval < 0.05)

# Exercise 1.3
with(renfe,
     t.test(x = price,
            mu = 43.25,
            conf.level = 0.9,
            subset = type %in% "AVE-TGV"))
