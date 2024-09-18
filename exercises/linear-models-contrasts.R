data(HBSC24_S4, package = "hecedsm")
# Summary statistics of 'authenticity'
HBSC24_S4 |>
  dplyr::group_by(cond) |>
  dplyr::summarize(mword = mean(authenticity),
                   sdword = sd(authenticity))
# Fit an ANOVA model
mod1 <- lm(authenticity ~ cond, data = HBSC24_S4)
# Compute marginal means by condition
emm <- emmeans::emmeans(mod1, specs = "cond")
# Define a list with the contrast weights
cweights <- list(C1 = c(1,-0.5,-0.5), C2 = c(0, 1, -1))
# Get summary of contrasts tests
emm |> emmeans::contrast(method = list(cweights))
# Note: coefficients may be different, but the test and p-values should match those reported in the paper

# Test for impact of authenticity on 'words'
summary(lm(words ~ authenticity, data = HBSC24_S4))$coefficients
