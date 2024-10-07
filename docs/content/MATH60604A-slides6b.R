data(bixicoll, package = "hecstatmod")

linmod1_bixi <- lm(lognuser ~ celcius, data = bixicoll)
linmod2_bixi <- lm(lognuser ~ farenheit, data = bixicoll)
# Extract coefficients
beta_c <- as.numeric(coef(linmod1_bixi))
beta_f <- as.numeric(coef(linmod2_bixi))
# Check that we can retrieve coefficients using the linear relationship
isTRUE(all.equal(beta_c, c(sum(c(1,32) * beta_f), 1.8*beta_f[2])))

# Model with exact colinearity
linmod3 <- lm(lognuser ~ celcius + farenheit, data = bixicoll)
Xmat <- model.matrix(linmod3)
eigen(t(Xmat) %*% Xmat, only.values = TRUE)$values
# Model output includes warnings
summary(linmod3)
# One of the parameters is not estimated


linmod4 <- lm(lognuser ~ celcius + rfarenheit, data = bixicoll)
summary(linmod4)
# Estimates, but none of the two temperature coef are significant
# given the other is already in the linmodel
car::vif(linmod4)
with(bixicoll, cor(celcius, rfarenheit))
car::avPlots(linmod4)

## Example with the college data
data(college, package = "hecstatmod")
linmod1_college <- lm(salary ~ rank + sex + service + field + years, data = college)
summary(linmod1_college)
# Coefficients for year and service are of opposite sign
car::vif(linmod1_college)
car::avPlots(linmod1_college, terms = ~ service + years, id = FALSE)
