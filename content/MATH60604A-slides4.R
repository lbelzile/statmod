library(ggplot2) # grammar of graphics
theme_set(theme_classic())
library(patchwork) # combine ggplot objects
library(emmeans) # estimated marginal means
library(knitr)
knitr::opts_chunk$set(fig.retina = 3, collapse = TRUE)
options(digits = 3, width = 75)

# Load datasets
data(SKD23_S2A, package = "hecedsm")
data(BSJ92, package = "hecedsm")
data(MV23_S1, package = "hecedsm")


# Fit simple linear regression
lm_simple <- lm(pef ~ proportion, data = SKD23_S2A)

# Predict on a grid of values of 'proportion'
prop_grid = seq(0, 3, by = 0.1)
# Confidence intervals for the mean
predci <- data.frame(cbind(proportion = prop_grid,
                predict(lm_simple, newdata = data.frame(proportion = prop_grid),
                        interval = "confidence")))
# Prediction intervals for the new observation
predpi <- data.frame(cbind(proportion = prop_grid,
                predict(lm_simple, newdata = data.frame(proportion = prop_grid),
                        interval = "prediction")))
# Marginal means
mmeans <- emmeans(aov(pef ~ factor(proportion), data = SKD23_S2A),
        specs = "proportion")

# Plot observations with the regression line,
# hyperbolic pointwise intervals (conf. + pred)
# and the group means
ggplot(data = SKD23_S2A) +
   # add jittered obsrvations
    geom_point(mapping = aes(x = proportion, y = pef),
               col = "grey", alpha = 0.8,
               position = position_jitter(width = 0.1, height = 0)) +
   # add prediction intervals (beyond the range of the data!
    geom_ribbon(data = predpi,
                mapping = aes(x = proportion, ymin = lwr, ymax = upr),
                fill = "grey70",
                alpha = 0.2) +
    geom_ribbon(data = predci,
                aes(x = proportion, ymin = lwr, ymax = upr),
                fill = "grey70",
                alpha = 0.5) +
   geom_segment(data = data.frame(x = c(0,0.5,1,2),
                                 y = summary(mmeans)$emmean),
               mapping = aes(x = x - 0.1, y = y, xend = x + 0.1),
               linewidth = 1.5) +
    geom_abline(slope = coef(lm_simple)[2],
                intercept = coef(lm_simple)[1]) +
    scale_y_continuous(oob = scales::squish,
                       breaks = 1:7,
                       labels = 1:7) +
    scale_x_continuous(limits = c(0,3),
                       breaks = c(0, 0.5, 1, 2),
                       labels = c("0","0.5","1","2"),
                       oob = scales::squish) +
    labs(x = "proportion of paper/plastic",
         subtitle = "perceived environmental friendliness",
         y = "")

# Create a table with only the predictions at the proportion from the paper
tab1 <- predict(lm_simple,
        newdata = data.frame(proportion = c(0, 0.5, 1, 2)),
        interval = "prediction") # prediction intervals
tab2 <- predict(lm_simple,
        newdata = data.frame(proportion = c(0, 0.5, 1, 2)),
        interval = "confidence") # confidence for mean
# Print the table (for Rmarkdown/Quarto, either html or pdf.
knitr::kable(cbind(proportion = c(0, 0.5, 1, 2), tab1, tab2),
             align = c("ccccccc"),
             booktabs = TRUE,
             col.names = c("`proportion`", "prediction", "lower","upper",
                           "mean", "lower CI","upper CI"))

# Inference of model parameters of the simple linear regression model
summary(lm_simple)$coefficients # t-tests (Wald) for beta=0 with p-values
confint(lm_simple) # confidence intervals for betas

# Code for the two-sample t-test using "lm"

MV23_S1 <- MV23_S1 |>
    dplyr::mutate(amount2 = ifelse(is.na(amount), 0, amount))
linmod_MV23 <- lm(amount2 ~ condition, data = MV23_S1)
# Wald tests with coefficients
summary(linmod_MV23)$coefficients
# Analysis of variance table with F tests
anova(linmod_MV23)
# Compare with t.test
t.test(amount2 ~ condition, data = MV23_S1, var.equal = TRUE)

# Second test - compare simple linear regression with ANOVA
linmod <- lm(pef ~ proportion, data = SKD23_S2A) # fit simple linear regression
anovamod <- lm(pef ~ factor(proportion), # one-way ANOVA
               data = SKD23_S2A)
#  Compare the nested models via 'anova' call
anova(linmod, anovamod) # is the change in PEF linear?
# Specifying the linear restrictions for the full model
car::linearHypothesis(model = anovamod,
   hypothesis = rbind(c(0, -2, 1, 0),
                      c(0, 0, -2, 1)))


# Test effect of group globally
mod_post <- lm(posttest1 ~ group + pretest1,
               data = BSJ92) # change name for package
# Global F-tests comparing models with a covariate (to one without)
car::Anova(mod_post, type = 3)
# NOTE: not the same as `anova`, which adopts a sequential decomposition


## Estimated marginal means for posttest1
emmeans_post <- emmeans(object = mod_post,
                        specs = "group") # which variable to keep
# By default, 'emmeans' compute at the mean of the continuous variable

# the `pretest1` covariate is fixed to it's overall mean
knitr::kable(emmeans_post,
      digits = c(2,2,2, 0,2,2),
      booktabs = TRUE,
      col.names = c("terms",
                    "marg. mean",
                    "std. err.",
                     "dof",
                    "lower (CI)",
                    "upper (CI)"))

# Identify the order of the level of the variables
with(BSJ92, levels(group))
# DR, DRTA, TA (alphabetical)
contrasts_list <- list(
  # Contrasts: linear combination of means, coefficients sum to zero
  "C1: average (DRTA+TA) vs DR" = c(-1, 0.5, 0.5),
  "C2: DRTA vs TA" = c(0, 1, -1)
)
# Compute contrasts
contrasts_post <-
  contrast(object = emmeans_post,
           method = contrasts_list)
# Print results
contrasts_post
# Extract a data frame with the results
contrasts_summary_post <- summary(contrasts_post)

# Estimated contrasts for post-test 1.
knitr::kable(contrasts_post,
      booktabs = TRUE,
      digits = c(2,2,2,0,2,2),
      col.names = c("contrast",
                    "estimate",
                    "std. err.",
                    "dof",
                    "stat",
                    "p-value"))


## Testing whether a coefficient is equal to 1
# Extract coefficients and standard errors
beta_pre <- coefficients(mod_post)['pretest1']
se_pre <- sqrt(c(vcov(mod_post)['pretest1', 'pretest1']))
wald <- (beta_pre - 1)/se_pre # Wald statistic, signed version
# P-value based on Student-t distribution, with n-p-1 dof
pval <- 2*pt(abs(wald), df = mod_post$df.residual, lower.tail = FALSE)

# Model comparison via 'anova' call
mod0 <- lm(posttest1 ~ offset(pretest1) + group, data = BSJ92)
# The 'offset' fixes the term and so this is equivalent to a coefficient of 1
aov_tab <- anova(mod0, mod_post)


## Other contrasts (ref vs each treatment, pairwise comparisons)
anovamod <- lm(pef ~ factor(proportion), data = SKD23_S2A) # one-way ANOVA
margmean <- anovamod |>
  emmeans::emmeans(specs = "proportion") # group means
contrastlist <- list( # specify contrast vectors
   refvshalf = c(1, -1, 0, 0),
   refvsone =  c(1, 0, -1, 0),
   refvstwo =  c(1, 0, 0, -1))
# compute contrasts relative to reference
contrast <- margmean |>
  emmeans::contrast(
 method = contrastlist)


## Estimated group averages of PEF per proportion with standard errors
knitr::kable(margmean,
      digits = c(2,2,3,0,2,4),
      booktabs = TRUE,
      col.names = c("proportion",
                    "marg. mean",
                    "std. err.",
                    "dof",
                    "lower (CI)",
                    "upper (CI)"))

# Estimated contrasts for differences of PEF to no paper
knitr::kable(contrast,
      booktabs = TRUE,
      digits = c(2,2,2,0,2,2),
      col.names = c("contrast",
                    "estimate",
                    "std. err.",
                    "dof",
                    "stat",
                    "p-value"))

