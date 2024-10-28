
library(ggplot2)
theme_set(theme_classic())
library(patchwork)

data(college, package = "hecstatmod")
mod <- lm(salary ~ sex + field + rank + service, data = college)
# Zero correlations
cor(resid(mod), model.matrix(mod))[-1]
cor(resid(mod), fitted(mod))
# Mean zero errors
mean(resid(mod))

# Plot of residuals against fitted values (left), and against the explanatory variable `service` (right)
# for the linear regression of the `college` data.
# The intercept and the slope of the simple linear regressions are zero.
mod <- lm(salary ~ sex + field + rank + service, data = college)
g1 <- ggplot(data = data.frame(yhat = fitted(mod),
                         e = resid(mod)),
       mapping = aes(x = yhat, y = e)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, col = "grey") +
  labs(x = "fitted values", y = "residuals")
g2 <- ggplot(data = data.frame(service = college$service,
                         e = resid(mod)),
       mapping = aes(x = service, y = e)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, col = "grey") +
  labs(x = "years of service", y = "residuals")
g1 + g2


# Verify model invariance
modA <- lm(salary ~ sex +  rank + service, data = college)
modB <- lm(salary ~ 0 + sex + rank + service, # 0+ = remove intercept
           data = college |>
             dplyr::mutate(service = scale(service)), # standardize variable (mean zero, unit std. dev)
           contrasts = list(rank = contr.sum)) # change parametrization of dummies
head(model.matrix(modA), n = 3L)
head(model.matrix(modB), n = 3L)
# Model invariance
isTRUE(all.equal(fitted(modA), fitted(modB)))


# Residuals and other similar quantities
e <- resid(modA) # ordinary residuals
r <- rstudent(modA) # ext. studentized resid
# Q-Q plot of jackknife studentized residuals
car::qqPlot(modA, id = FALSE)

# Number of observations
n <- nrow(modA$model)
# Number of mean parameters
nbetas <- length(coef(modA))
## Calculate leverage
leverage <- hatvalues(modA)
# Which points have high leverage?
which(leverage > 2*nbetas/n)
?influence.measures
## Cook distance
distCook <- cooks.distance(modA)


# Definition of the coefficient of determination
summary(mod)$r.squared # output from "lm"
y <- college$salary
yhat <- fitted(mod)
cor(y, yhat)^2

