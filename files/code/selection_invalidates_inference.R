# Model selection invalidates inference
options(digits = 3)
set.seed(2024)
d <- 10L
n <- 1e3L
B <- 1e3L
# Generate a covariance matrix with correlated inputs
Sigma <- rWishart(n = 1,
                  df = d+1L,
                  Sigma = diag(rep(0.75, d)) + matrix(0.25, d, d))[,,1]
out_select <- matrix(1, nrow = B, ncol = d)
out_normal <- matrix(1, nrow = B, ncol = d)
for(j in seq_len(B)){
 # Generate uncorrelated data
 X <- MASS::mvrnorm(n = n, mu = rep(0, d), Sigma = Sigma)
 y <- rnorm(n, mean = 2)
 dat <- data.frame(y = y, X = X)
 # Stepwise selection using AIC
 modfin <- MASS::stepAIC(object = lm(y ~ ., data = dat),
                         scope = lm(y ~ 1, data = dat),
                         trace = 0) # no printing
 # Extract the names of the columns (-intercept) left over
 cols <- as.integer(substr(colnames(modfin$model)[-1],3,4))
 # If some variables were selected
 if(length(cols) > 0){
   # Extract p-values
   pvals <- summary(modfin)$coefficients[,4][-1]
   out_select[j,cols] <- pvals
   out_normal[j,] <- summary(lm(y ~ ., data = dat))$coefficients[-1,4]
 }
}
# Compute size of test at level alpha=5% and 10%
colMeans(out_select < 0.05)
colMeans(out_select < 0.1)
# Without selection
colMeans(out_normal < 0.05)
colMeans(out_normal < 0.1)
