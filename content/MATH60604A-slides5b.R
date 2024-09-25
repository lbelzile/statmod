
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

# Scale variables, change parametrization of dummies, remove intercept
modB <- lm(salary ~ 0 + sex + field + rank + service,
           data = college |>
            dplyr::mutate(service = scale(service)),
           contrasts = list(rank = contr.sum))
head(model.matrix(mod), n = 3L)
head(model.matrix(modB), n = 3L)
# Verify model invariance
isTRUE(all.equal(fitted(mod), fitted(modB)))


# Definition of the coefficient of determination
summary(mod)$r.squared # output from "lm"
y <- college$salary
yhat <- fitted(mod)
cor(y, yhat)^2

