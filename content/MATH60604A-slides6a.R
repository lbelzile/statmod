## Load packages
library(ggplot2) # grammar of graphics
library(knitr) #
theme_set(theme_classic())
library(patchwork) # compose graphs
library(car) # companion to applied regression
library(ggfortify) # ggplot for base graphs
library(nlme) # mixed models and variance specification
library(mgcv) # generalized additive models and smoothing
library(qqplotr, warn.conflicts = FALSE) # qqplots for ggplot2
library(MASS)

## APPLICATION 1 - Linear model for insurance data
data(insurance, package = "hecstatmod")
# Create dummy for obesity
insurance <- insurance |>
  dplyr::mutate(obesity = factor(bmi >= 30, levels = c(FALSE, TRUE),
                                 labels = c("non-obese","obese")))
# Fit several (incorrect models of increasing complexity)
linmod0_insurance <- lm(charges ~ age + obesity*smoker, data = insurance)
linmod1_insurance <- lm(charges ~ age + obesity*smoker*bmi, data = insurance)
linmod2_insurance <- lm(charges ~ age + obesity*smoker*bmi + children, data = insurance)
linmod3_insurance <- lm(charges ~ splines::bs(age) + obesity*smoker*bmi + children, data = insurance)
# Model 0 has misspecified effect of age, and missing slope for bmi for smokers, and omitted variable

## APPLICATION 2 - Linear model for college data
data(college, package = "hecstatmod")
linmod1_college <- lm(salary ~ rank + field + sex + service + years, data = college)

## APPLICATION 3 - Poison data
data(poisons, package = "SMPracticals")
linmod1_poisons <- lm(time ~ poison + treat, data = poisons)
linmod2_poisons<- lm(I(1/time) ~ poison + treat, data = poisons)

## APPLICATION 4 - air passengers
data(airpassengers, package = "hecstatmod")
# Model with monthly dummies
linmod1_airpass <- lm(passengers ~ factor(month) + year, data = airpassengers)
# Log linear model with monthly dummies
linmod2_airpass <- lm(log(passengers) ~ factor(month) + year, data = airpassengers)
# Log linear model with Fourier basis and nonlinear effect of year
linmod3_airpass <- lm(log(passengers) ~ I(cos(2*pi*month/12)) +
                        I(sin(2*pi*month/12)) +
                        I(cos(4*pi*month/12)) +
                        I(sin(4*pi*month/12)) +
                        I(cos(6*pi*month/12)) +
                        I(sin(6*pi*month/12)) +
                        I(year) + I(year^2),
                      data = airpassengers)
linmod4_airpass <- mgcv::gam(
  log(passengers) ~ s(month, bs = "cc") + s(year),
  data = airpassengers)

# Diagnostics plots
autoplot(linmod0_insurance, 1:6) # requires ggfortify + ggplot2
plot(linmod0_insurance)

# Plot manually
insurance <- insurance |>
  dplyr::mutate(
    e0 = resid(linmod0_insurance),
    e1 = resid(linmod1_insurance),
    e2 = resid(linmod2_insurance),
    smokobese = interaction(obesity, smoker)
  )
g1 <- ggplot(data = insurance,
             mapping = aes(x = bmi,
                           y = e0,
                           col = smokobese,
                           fill = smokobese)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = FALSE) +
  scale_color_viridis_d() + #color blind palette
  scale_fill_viridis_d() +
  labs(x = "body mass index",
       y = "ordinary residuals",
       color = "obese/smoker",
       fill = "obese/smoker") +
  theme(legend.position = "bottom")
g2 <- ggplot(data = insurance,
       mapping = aes(x = children, y = e1)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x, col = "black") +
  scale_color_viridis_d() +
  labs(x = "number of children",
       y = "ordinary residuals")
g1 + g2


## Linearity: residual plots

# Clear nonlinear effect of age per group
car::residualPlots(linmod2_insurance, terms = ~ age)
# The model with smoothing splines does better (same for quadratic term)
car::residualPlots(linmod3_insurance, terms = ~ splines::bs(age), test = FALSE)

# No pattern, except for heteroscedasticity
# Evidence of increasing variance with fitted values
car::residualPlots(linmod1_college)

# Some visible patterns
car::residualPlots(linmod1_poisons)

# Increasing variance with level, quadratic-like effect
airpassengers <- airpassengers |>
  dplyr::mutate(e3 = resid(linmod3_airpass),
                e4 = resid(linmod4_airpass))
car::residualPlots(linmod1_airpass)
car::residualPlots(linmod2_airpass)
# The last model is better, but there are residual monthly effects
ggplot(data = airpassengers,
       mapping = aes(x = month, y = e3)) +
  geom_jitter()
ggplot(data = airpassengers,
       mapping = aes(x = year, y = e3)) +
  geom_jitter()
ggplot(data = airpassengers,
       mapping = aes(x = month, y = e4)) +
  geom_jitter()

ggplot(data = airpassengers,
       mapping = aes(x = year, y = e4)) +
  geom_point()


## Tests for homogeneity of variance

# Extract externally studentized residuals
r <- rstudent(linmod1_college)
# Levene test
car::leveneTest(r ~ rank, center = "mean", data = college)
# Bartlett test (not recommended)
bartlett.test(r ~ rank, data = college)
# Breusch-Pagan (with a score test), with specific covariates
# Here, we use only rank
car::ncvTest(linmod1_college, var.formula =  ~ rank)


# Fit model via generalized least squares
linmod_college2 <- nlme::gls(
  model = salary ~ rank + field + sex + service, # mean specification
  weights = nlme::varIdent(form = ~1 | rank), # constant variance per rank
  data = college)
# Plot of fitted versus standardized residuals
plot(linmod_college2)
# Note the change in p-values for Wald tests
summary(linmod_college2)
# Model fitted via REML, so difference between likelihood ratio and Wald tests
anova(linmod_college2)

# Impact of sex not significant once we account for rank

# Sandwich matrix
vcov_HCE <- car::hccm(linmod1_college)
# Wald tests with sandwich matrix
w <- coef(linmod1_college) / sqrt(diag(vcov_HCE))
# Variance ratios
diag(vcov_HCE) / diag(vcov(linmod1_college))
# Compute p-values for Wald tests with modified sandwich estimator
pval <- 2*pt(abs(w),
             df = linmod1_college$df.residual,
             lower.tail = FALSE)
# Also with lmtest
lmtest::coeftest(modlin1_college, vcov. = vcov_HCE)

library(forecast)
# Correlogram and partial autocorrelation function
acf(resid(linmod2_airpass))
pacf(resid(linmod2_airpass))
forecast::ggAcf(resid(linmod2_airpass),lag.max=25) +
  labs(x = "lag", y ="autocorrelation", title = "") +
  ylim(c(-1,1))
# Fit a model accounting for the autocorrelation (AR1)
linmod4_airpass <- nlme::gls(
  model = formula(linmod2_airpass),
  data = airpassengers,
  correlation = nlme::corAR1())
# Autocorrelation mostly gone
forecast::ggAcf(resid(linmod4_airpass, type = "normalized"), lag.max=25) +
  labs(x = "lag", y ="autocorrelation", title = "") +
  ylim(c(-1,1))


## Quantile-quantile plots for normality
car::qqPlot(linmod1_poisons, id = FALSE, ylab= 'externally studentized residuals')
car::qqPlot(linmod2_poisons, id = FALSE, ylab= 'externally studentized residuals')


## Box-Cox transformation for model
car::boxCox(linmod1_poisons)

## Robust regression and outliers
rmod_ins <- MASS::rlm(
  data = insurance,
  charges ~ splines::bs(age) + obesity*smoker*bmi + children)
residualPlots(rmod_ins, tests = FALSE)
