
library(ggplot2)
library(knitr)
theme_set(theme_classic())
library(patchwork)
library(emmeans)

# Scatterplots and fitted lines for a model with a single continuous and
#  binary explanatory, without (left) and with (right) an interaction term.
data(interaction, package = "hecstatmod")
interaction <- interaction |>
  dplyr::mutate(sex = factor(sex, levels = c(0,1),
                             labels = c("men","women")))
mod0 <- lm(intention ~ fixation + sex, data = interaction)
predmod <- predict(mod0)
g1 <- ggplot(data = interaction,
       mapping = aes(
  x = fixation,
  y = intention,
  color = sex)) +
 geom_point() +
 geom_line(aes(y = predmod), linewidth = 1,
           show.legend = FALSE) +
 MetBrewer::scale_color_met_d(name = "Hiroshige") +
 labs(color = "sex",
     x = "fixation time (in seconds)",
     y = "intention to buy")
g2 <- ggplot(data = interaction,
       mapping = aes(x = fixation,
                     color = sex,
                     y = intention)) +
 geom_point() +
 geom_smooth(formula = y ~ x, se = FALSE, method = "lm", linewidth = 1,
             show.legend = FALSE) +
 MetBrewer::scale_color_met_d(name = "Hiroshige") +
 labs(color = "sex",
     x = "fixation time (in seconds)",
     y = "intention to buy")
g1 + g2 + plot_layout(guides = 'collect') & theme(legend.position = "bottom")


# To specify an interaction use :
mod <- lm(intention ~ sex + fixation +  sex:fixation,
          data = interaction)
# A shortcut is sex*fixation, which expands to the above

# Check significance of the interaction term
summary(mod)$coefficients


# Example 1
# Fit model with sum-to-zero contrasts
options(contrasts = c("contr.sum","contr.poly"))
data(STC21_SS5, package = "hecedsm")
# Fit 2x2 ANOVA model with interaction
mod1 <- lm(likelihood ~ purchase * debttype, data = STC21_SS5)
# Extract marginal means of all four groups
emm <- emmeans::emmeans(
  mod1,
  specs = c("debttype","purchase"))
# Produce an interaction plot
emmeans::emmip(emm,  debttype ~ purchase, CIs = TRUE) +
  theme_classic() +
  theme(legend.position = "bottom")
# Also ?interaction.plot

# Example 2

data(LKUK24_S4, package = "hecedsm")
# Fit three-way model
mod <- lm(appropriation ~ politideo * chefdax * brandaction,
          data = LKUK24_S4)
# Extract all marginal means
emm <- emmeans(mod,
        specs = c("chefdax", "brandaction", "politideo"))
# Decide on which variables to map to x/y axis + color/panel
emmip(object = emm,
        formula = brandaction ~ chefdax | politideo,
        CIs = TRUE) # add 95% confidence intervals for mean


# Analysing Supplementary Study 5
# of Sharma, Tully, and Cryder (2021)
data(STC21_SS5, package = "hecedsm")
# Use 'aov' to fit models to balanced data, with categorical variables
# Equivalent to 'lm' with sum-to-zero contrasts
# Check counts per subcategory (data are unbalanced)
xtabs(~purchase + debttype, data = STC21_SS5)
# Compute overall/rows/columns/cells means
model.tables(x = aov(likelihood ~ purchase*debttype,
                  data = STC21_SS5),
             type = "means")

# Unbalanced data, use type II sum of square decomposition
# Analysis of variance reveals non-significant
# interaction of purchase and type
car::Anova(mod, type = 2)


# Pairwise comparisons within levels of purchase
# Using the main effects
emmeans::emmeans(mod,
                 # what variable to keep (so average over "debttype")
                 specs = "purchase",
                 contr = "pairwise")

# Example 2
data(LKUK24_S4, package = "hecedsm")
mod <- lm(appropriation ~ politideo * chefdax * brandaction,
   data = LKUK24_S4)
# ANOVA table
car::Anova(mod, type = 2)
# Reveals an interaction between political ideology and Chef Dax

# Marginal means for political ideology/Chef Dax
# Compute simple effects, by political ideology
emmeans(mod,
         specs = "chefdax",  # variable to keep
         by = "politideo", # variable to condition on
         contrast = "pairwise") # follow-up contrasts

# Marginal mean for brandaction
# Main effects since not interacting with others
(emm_brand <- emmeans(mod, specs = c("brandaction")))
# Joint F test for the main effect of brandaction
emm_brand |> pairs() |> joint_tests()
# Note the degrees of freedom: even though we average,
# the denominator of the F-test is based on the residuals
# of the full three-way model.

# We can also compute these by casting the multiway ANOVA
# into a one-way ANOVA with more categories
# and compute contrasts as before
mod <- lm(likelihood ~ group,
          data = STC21_SS5 |>
            dplyr::mutate(group = interaction(debttype, purchase)))
emmeans(mod, specs = "group") |>
  contrast(method = list(main_pairwise = c(1,-1,1,-1)/2))
# Or compute custom contrasts based on the parts of the data above.
