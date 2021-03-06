
## Exercise 3.2
X <- c(5,  6,  3,  7,  1,  2, 11,  8,  7, 34,  1,  7, 10,  1,  0)
n <- length(X)
mle <- 1/mean(X)
hess <- numDeriv::hessian(function(x){sum(dgeom(x = X, prob = x, log = TRUE))}, x = mle)
  
obsinfo <- function(p, X){length(X)*(mean(X)-1)/(1-p)^2 + length(X)/p^2}
jhat <- obsinfo(p = mle, X = X)
se_mle <- sqrt(solve(jhat))

lrt <- 2*(sum(dgeom(x = X, prob = mle, log = TRUE)) - sum(dgeom(x = X, prob = 0.1, log = TRUE)))
pchisq(lrt, df = 1, lower.tail = FALSE)

wald <- (mle - 0.1)/se_mle
pchisq(wald^2, df = 1, lower.tail = FALSE)

## Exercise 3.3
loglik <- function(y, beta, sales){
stopifnot(length(y) == length(sales), length(beta) == 2L)
sum(y*(beta[1] + beta[2]*sales) - exp(beta[1] + beta[2]*sales))
}
y <- c(2,5,9,3,6,7,11, 12,9,10, 9,7)
sales <- c(rep(0L, 7), rep(1L, 5))
# Maximum likelihood estimate
betahat <- c(log(mean(y[sales == 0L])), 
             log(mean(y[sales == 1L])) - log(mean(y[sales == 0L])))
se <- sqrt(c(1/43, 90/(43*47)))


mod <- glm(y ~ sales, family=poisson(link="log"))

se_beta <- sqrt(diag(vcov(mod)))
isTRUE(all.equal(betahat, mod$coef, check.attributes = FALSE))

betahat_prof <- c(log(sum(y)) - log(7+25/4), log(5/4))
R <- 2*(loglik(y = y, beta = betahat, sales = sales) - 
          loglik(y = y, beta = betahat_prof, sales = sales))
pchisq(R, 1, lower.tail = FALSE)

mod0 <- glm(y ~ 1, offset = log(5/4)*sales, family=poisson(link="log"))
anova(mod0, mod, test = "Chisq")