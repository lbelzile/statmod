# Slides 2, MATH 60604A

# Graphics packages
library(ggplot2)
library(patchwork)
## Load database
data(waiting, package = "hecstatmod")
# If you get an error message, redownload the 'hecstatmod' package via
# remotes::install_github("lbelzile/hecstatmod")
#
## Histogram of waiting time with rugs for the observations (left)
ggplot(data = data.frame(time = waiting), mapping = aes(x = time)) +
  geom_histogram(bins = 10) +
  geom_rug() +
  labs(x = "waiting time (in seconds)")

# Exponential log likelihood
exp_loglik <- function(lambda) {
  sum(dexp(waiting, rate = 1 / lambda, log = TRUE))
}
# Create a vector of values for the scale lambda
lambda_cand <- seq(min(waiting) + 10, max(waiting), by = 1)
# Compute the log likelihood for different values of lambda
ll_waiting <- sapply(lambda_cand, exp_loglik)
# Plot the log likelihood
ggplot(data = data.frame(x = lambda_cand, y = ll_waiting),
       mapping = aes(x = x, y = y)) +
  geom_line() +
  # Add a line for the mle
  geom_vline(xintercept = mean(waiting), linetype = "dashed") +
  labs(x = expression(lambda), y = "log likelihood")

# Compute the probability of exceeding 60 seconds using R
pexp(q = 60,
     rate = 1 / mean(waiting),
     lower.tail = FALSE)

### R DEMO

# Negative log likelihood for a Weibull sample
nll_weibull <- function(pars, y) {
  # Handle the case of negative parameter values
  if (isTRUE(any(pars <= 0))) {
    # parameters must be positive
    return(1e10) # large value (not infinite, to avoid warning messages)
  }
  - sum(dweibull(
    x = y,
    scale = pars[1],
    shape = pars[2],
    log = TRUE
  ))
}
# Gradient of the negative Weibull log likelihood
gr_nll_weibull <- function(pars, y) {
  scale <- pars[1]
  shape <- pars[2]
  n <- length(y)
  grad_ll <- c(
    scale = -n * shape / scale + shape * scale ^ (-shape - 1) * sum(y ^ shape),
    shape = n / shape - n * log(scale) + sum(log(y)) -
      sum(log(y / scale) * (y / scale) ^ shape)
  )
  return(-grad_ll)
}

# Use exponential submodel MLE as starting parameters
start <- c(mean(waiting), 1)
# Check gradient function is correctly coded!
# Returns TRUE if numerically equal to tolerance
isTRUE(all.equal(
  numDeriv::grad(nll_weibull, x = start, y = waiting),
  gr_nll_weibull(pars = start, y = waiting),
  check.attributes = FALSE
))
# Numerical minimization using optim
opt_weibull <- optim(
  par = start,
  # starting values
  fn = nll_weibull,
  # pass function, whose first argument is the parameter vector
  gr = gr_nll_weibull,
  # optional (if missing, numerical derivative)
  method = "BFGS",
  # gradient-based algorithm, common alternative is "Nelder"
  y = waiting,
  # vector of observations, passed as additional argument to fn
  hessian = TRUE
) # return matrix of second derivatives evaluated at MLE
# Alternative using pure Newton
# nlm(f = nll_weibull, p = start, hessian = TRUE, y = waiting)
# Parameter estimates - MLE
(mle_weibull <- opt_weibull$par)
# Check gradient for convergence
gr_nll_weibull(mle_weibull, y = waiting)
# Is the Hessian of the negative positive definite (all eigenvalues are positive)
# If so, we found a maximum and the matrix is invertible
isTRUE(all(eigen(opt_weibull$hessian)$values > 0))

# The Hessian matrix of the negative log likelihood
# evaluated at the MLE (observed information matrix)
obsinfo_weibull <- opt_weibull$hessian
vmat_weibull <- solve(obsinfo_weibull)
# Standard errors
se_weibull <- sqrt(diag(vmat_weibull))

# Deriving standard errors for a different function of the parameter
# Here for the exponential model using the Pr(Y>60)
lambda_hat <- mean(waiting)
# Function of the parameter
phi_hat <- exp(-60 / lambda_hat)
# Jacobian of the transformation
dphi <- function(lambda) {
  60 * exp(-60 / lambda) / (lambda ^ 2)
}
# Variance of the exponential scale
V_lambda <- lambda_hat ^ 2 / length(waiting)
# Variance of the Pr(Y>60) - via delta-method
V_phi <- dphi(lambda_hat) ^ 2 * V_lambda
# Extract standard error and print
(se_phi <- sqrt(V_phi))



nll <- matrix(nrow = 101, ncol = 100)
alpha <- seq(mle_weibull[2] - 2.5 * se_weibull[2],
             mle_weibull[2] + 2.5 * se_weibull[2],
             length.out = 100)
lambda <- seq(mle_weibull[1] - 2.5 * se_weibull[1],
              mle_weibull[1] + 2.5 * se_weibull[1],
              length.out = 101)
z <- rep(NA, length(nll))
for (i in seq_along(lambda)) {
  for (j in seq_along(alpha)) {
    z[(i - 1) * 100 + j] <- nll[i, j] <-
      nll_weibull(pars = c(lambda[i], alpha[j]), y = waiting)
  }
}


# Plot likelihood surface with curve for the R stat
ggplot() +
  geom_raster(data = data.frame(
    x = rep(lambda, each = length(alpha)),
    y = rep(alpha, length.out = length(alpha) *
              length(lambda)),
    z = c(-z + opt_weibull$value)
  ),
  mapping = aes(
    x = x,
    y = y,
    fill = pchisq(-2 * z, df = 2)
  )) +
  geom_contour(
    data = data.frame(
      x = rep(lambda, each = length(alpha)),
      y = rep(alpha, length.out = length(alpha) *
                length(lambda)),
      z = c(-z + opt_weibull$value)
    ),
    mapping = aes(x = x, y = y, z = z),
    col = "white",
    breaks = -qchisq(seq(0.1, 0.9, by = 0.1), df = 2) / 2
  ) +
  # Add cross for MLE
  geom_point(
    shape = 4,
    color = "white",
    data = data.frame(x = mle_weibull[1], y = mle_weibull[2]),
    mapping = aes(x = x, y = y)
  ) +
  # Use a different color palette
  scale_fill_viridis_c(direction = 1, option = "viridis") +
  labs(x = expression(paste("scale ", lambda)),
       y = expression(paste("shape ", alpha)),
       fill = "probability level",) +
  scale_y_continuous(expand = expansion(), limits = range(alpha)) +
  scale_x_continuous(expand = expansion(), limits = range(lambda)) +
  theme(legend.position = "bottom")


# Comparison of Weibull vs exponential model
# Calculate Wald statistic for alpha=1
wald_exp <- (mle_weibull[2] - 1) / se_weibull[2]
# Compute p-value
pchisq(wald_exp ^ 2, df = 1, lower.tail = FALSE)
# p-value less than 5%, reject null
# Obtain 95% confidence intervals
mle_weibull[2] + qnorm(c(0.025, 0.975)) * se_weibull[2]
# 1 is not inside the confidence interval, reject null

# Quantile-quantile plots
n <- length(waiting)
set.seed(1234)
g1 <- ggplot() +
  stat_qq(
    data = data.frame(y = waiting),
    mapping = aes(sample = y),
    distribution = qexp,
    dparams  = list(rate = 1 / mean(waiting))
  ) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "theoretical quantiles", y = "observed quantiles", subtitle = "exponential")
# Estimate parameters via optimization routine
fitweibull <- MASS::fitdistr(x = waiting, densfun = "weibull")
# Extract parameters
shape <- fitweibull$estimate['shape']
scale <- fitweibull$estimate['scale']
# Quantile-quantile plot of Weibull model
g2 <- ggplot() +
  # Do this manually to illustrate the concept
  geom_point(data = data.frame(
    y = sort(waiting),
    x = qweibull(ppoints(n), scale = scale, shape = shape)
  ),
  mapping = aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "theoretical quantiles", y = "observed quantiles", subtitle = "Weibull")
g1 + g2



# Exponential log likelihood
ll_exp <- function(lambda) {
  sum(dexp(waiting, rate = 1 / lambda, log = TRUE))
}
# MLE of the scale parameter
lambda_hat <- mean(waiting)
# Root search for the limits of the confidence interval
lrt_lb <- uniroot(
  # lower bound, using values below MLE
  f = function(r) {
    2 * (ll_exp(lambda_hat) - ll_exp(r)) - qchisq(0.95, 1)
  },
  interval = c(0.5 * min(waiting), lambda_hat)
)$root
lrt_ub <- uniroot(
  # upper bound,
  f = function(r) {
    2 * (ll_exp(lambda_hat) - ll_exp(r)) - qchisq(0.95, 1)
  },
  interval = c(lambda_hat, 2 * max(waiting))
)$root


# Profile log likelihood

# Conditional MLE for lambda
lambda_alpha <- function(alpha, y = waiting) {
  (mean(y ^ alpha)) ^ (1 / alpha)
}
# Profile log likelihood for alpha
prof_alpha_weibull <- function(par, y = waiting) {
  sapply(par, function(a) {
    nll_weibull(pars = c(lambda_alpha(a), a), y = y)
  })
}
ggplot() +
  stat_function(
    fun = function(par) {
      opt_weibull$value - prof_alpha_weibull(par)
    },
    xlim = c(1.8, 3.5),
    n = 1001L
  ) +
  geom_hline(yintercept = -qchisq(c(0.95, 0.99), df = 1) / 2, linetype = "dashed") +
  labs(x = expression(paste("shape ", alpha)), y = "profile log likelihood")


## Profile for the mean of the Weibull distribution
# Compute the MLE for the expected value via plug-in
mu_hat <- mle_weibull[1] * gamma(1 + 1 / mle_weibull[2])
# Create a profile function
prof_weibull_mu <- function(mu) {
  # For given value of mu
  alpha_mu <- function(mu) {
    # Find the profile by optimizing (line search) for fixed mu and the best alpha
    opt <- optimize(
      f = function(alpha, mu) {
        # minimize the negative log likelihood
        nll_weibull(c(mu / gamma(1 + 1 / alpha), alpha), y = waiting)
      },
      mu = mu,
      interval = c(0.1, 10) #search region
    )
    # Return the value of the negative log likelihood and alpha_mu
    return(c(nll = opt$objective, alpha = opt$minimum))
  }
  # Create a data frame with mu and the other parameters
  data.frame(mu = mu, t(sapply(mu, function(m) {
    alpha_mu(m)
  })))
}
# Create a data frame with the profile
prof <- prof_weibull_mu(seq(22, 35, length.out = 101L))
# Compute signed likelihood root r
prof$r <- sign(prof$mu - mu_hat) * sqrt(2 * (prof$nll - opt_weibull$value))

# Trick: fit a spline to obtain the predictions with mu as a function of r
# Then use this to predict the value at which we intersect the normal quantiles
fit.r <- stats::smooth.spline(x = cbind(prof$r, prof$mu), cv = FALSE)
pr <- predict(fit.r, qnorm(c(0.025, 0.975)))$y
# Plot the signed likelihood root - near linear indicates quadratic
g1 <- ggplot(data = prof, mapping = aes(x = mu, y = r)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_line() +
  geom_hline(yintercept = qnorm(0.025, 0.975), linetype = "dashed") +
  labs(x = expression(paste("expectation ", mu)), y = "signed likelihood root")
# Create a plot of the profile
g2 <- ggplot(data = prof,
             mapping = aes(x = mu, y = opt_weibull$value - nll)) +
  geom_line() +
  geom_hline(yintercept = -qchisq(c(0.95), df = 1) / 2, linetype = "dashed") +
  geom_vline(linetype = "dotted", xintercept = pr) +
  labs(x = expression(paste("expectation ", mu)), y = "profile log likelihood")

g1 + g2
