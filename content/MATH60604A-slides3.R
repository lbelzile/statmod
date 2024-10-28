## Load packages
library(patchwork) # arrange ggplot plots
library(ggplot2) # grammar of graphics
library(dplyr) # data wrangling
library(hecstatmod) # course databases
library(hecedsm)
# Change the ggplot theme for all plots
theme_set(theme_classic())

### Exploratory data analysis
# Data from Example 1
data(LC19_S1, package = "hecedsm") # load data
?hecedsm::LC19_S1 # get description of database (help menu)
str(LC19_S1) # quick summary of data
summary(LC19_S1) # summary statistics
with(LC19_S1, cor(familiarity, prodeval)) # linear correlation between variables
length(unique(LC19_S1$prodeval)) # number of unique instances of

# Scatterplot of familiarity vs product evaluation
ggplot(data = LC19_S1,
       mapping = aes(x = familiarity,
                     y = prodeval)) +
  geom_point()

# Data from Example 2
data("BSJ92", package = "hecedsm")
?hecedsm::BSJ92
summary(BSJ92)
# Scatterplot of pre-test vs post-test 1
ggplot(data = BSJ92,
       mapping = aes(x = pretest1,
                     y = posttest1,
                     col = group)) +
  # jitter points to better see ties
  geom_point(position = position_jitter(),
             size = 2) +
  # add a line for pre-test score = post-test score
  geom_abline(intercept = 0, slope = 1) +
  # change axis limits to get the full range
  scale_y_continuous(limits = c(1,16)) +
  scale_x_continuous(limits = c(1,16))


# Data from Example 3
data(college, package = "hecstatmod")
?hecstatmod::college
# Notable salary difference by sex, but these are observational data
t.test(salary ~ sex, data = college)
# Descriptive statistics
summary(college)
# Remark the small number of women (strong data imbalance)


# Scatterplot of salary as a function of number of years of
# service, with sex, facetted by academic rank
ggplot(data = college,
       mapping = aes(color = sex,
                     fill = sex,
                     y = salary,
                     x = service)) +
  geom_point() +
  facet_grid(~rank, scales = "free_x")
# The average salary increases with the rank and the number of
# years of service, but more variability the higher the rank
# There seems to be a hard limit of six years (+maternity leave) for
# assistant professors. Some profs have not retired after 60 years of service...

# Women/men split as a function of rank
with(college, table(sex, rank))
# The table shows the ratio men/women is different
# There is a strong correlation between the number of years from PhD and years of service
ggplot(data = college,
       mapping = aes(color = sex,
                     fill = sex,
                     y = service,
                     x = years)) +
  geom_point() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.1,0.9))
# Compute linear correlation between service and years since PhD
with(college, cor(years, service))

# Data from example 4
data(MV23_S1, package = "hecedsm")
?hecedsm::MV23_S1
str(MV23_S1)

summary(MV23_S1)
# 73% of participants donated
# donations range from $0.25 to $25, only non-zero donations are recorded
# "donate" is a derivative from response variable "amount": it should not be
# used as a covariate.
# If we are interested in the donation including zero amounts, we need to replace
# the missing values by 0
MV23_S1 <- MV23_S1 |> dplyr::mutate(amount = ifelse(is.na(amount), 0, amount))
# Calculate descriptive statistics separately for each group
MV23_S1 |>
  dplyr::group_by(condition) |> # group by levels of categorical variables
  dplyr::summarise(moy = mean(amount)) # calculate summaries


# Linear regression model (t-test)
mod <- lm(amount ~ condition, data = MV23_S1)
# The formula is 'response ~ explanatories', where
# we specify the name of each explanatory separated by a plus sign (+)
coef(mod) # estimation of the mean coefficients (betas)
summary(mod) # summary table

# Check the model matrix
head(model.matrix(~condition, data = MV23_S1))
# Compare indicators with factor levels
head(MV23_S1)



## Example 3 - ANOVA with K=3 levels

# By default, the parametrization of categorical variables (factors)
# is in terms of treatment contrasts (contr.treat), meaning that
# the reference category is part of the intercept
# and the coefficients for each levels are differences between the level
# indicated and the reference category
#
# In ANOVA, aov(, the parametrization used by software is contr.sum
# for which the intercept is the global mean and other coefficients
# are mean difference to the global mean (the missing coefficient can
# be deduced from the sum-to-zero constraint as -sum(coefs)

# If the reference category (by default the first alphanumerical value)
# is not the one of interest, you can use the "relevel" function to change it
# BSJ92 |> mutate(group = relevel(group, ref = "DRTA"))

# Different parametrizations for the mean
# Treatment contrasts
lm(posttest1 ~ group, data = BSJ92)
# Sum-to-zero constrast parametrization
lm(posttest1 ~ group, data = BSJ92, contrasts = list(group = "contr.sum"))
# No intercept - each coefficient represents the group mean
lm(posttest1 ~ -1 + group, data = BSJ92)
# The standard errors of each average is the same here because the sample is
# balanced = same number of observation per experimental condition

# A more complex model (ANCOVA) with the post score as a function of the pre-test result
lm(posttest1 ~ group +  pretest1,
   data = BSJ92)
# Fit the linear model with a centered covariate
linmod <- lm(
  posttest1 ~ group +  pretest1,
  data = BSJ92 |>
    dplyr::mutate( # center pre-test result by subtracting the overall pre-test mean
      pretest1 = pretest1 - mean(pretest1)))
# Notice that only the intercept coefficient changes
coef(linmod)
# Mean coefficients
## (Intercept)    pretest1   groupDRTA     groupTA
##       6.188       0.693       3.627       2.036
summary(linmod)


## Estimation

# We can compute manually quantities to check the output and
# highlight the calculations
# First, define the response variable and the model matrix
y <- BSJ92$posttest1
X <- model.matrix(linmod)
mco <- function(X, y){ as.numeric(solve(t(X) %*% X) %*% t(X) %*% y)}
# OLS coefficients for the mean
beta_hat <- as.numeric(coef(linmod))
# In practice, we use a SVD or QR decomposition to invert the matrix
isTRUE(all.equal(
  beta_hat,
  mco(X = X, y = y)))

# Other quantities of interest
# Ordinary residuals,
residuals <- resid(linmod)
# Standard deviation sigma, with sum of squared errors divided by n-p-1
sd_linmod <- sqrt(sum(residuals^2)/(length(y) - length(beta_hat)))
# Covariance matrix of the OLS residuals
vcov_beta <- vcov(linmod)
# Extract standard errors
se_beta <- sqrt(diag(vcov_beta))
# Check the calculations for the covariance matrix of betas
isTRUE(all.equal(
  sd_linmod^2 * solve(t(X) %*% X),
  vcov_beta))

# Fitted values
yhat <- fitted(linmod)
# R2 (coefficient of determination)
# is the square of the linear correlation between yhat and y
cor(yhat, y)^2
