library(hecedsm)

# Rosen and Jerdee (1974)
## Create a 2x2 matrix (contingency table) with the counts
RJ_cont <- matrix(c(32L, 12L, 19L, 30L),
                     ncol = 2,
                     nrow = 2,
                     byrow = TRUE)
# Calculate the statistic on data
obs_stat <- chisq.test(x = RJ_cont,
                       correct = FALSE)


# Liu et and (2023)
data(LRMM23_S1, package = "hecedsm")
head(LRMM23_S1)
ttest <- t.test(appreciation ~ role,
                data = LRMM23_S1,
                var.equal = FALSE)

# Brucks and Levav (2022)
data(BL22_L, package = "hecedsm")
# Negative binomial regression model
negbin <- MASS::glm.nb(ncreative ~ cond,
             data = BL22_L)
# Get test statistic
negbin_Anova <- car::Anova(negbin, type = 3)
broom::tidy(negbin_Anova)
# Alternative test
ttest_BL22 <- t.test(ncreative ~ cond,
             data = BL22_L, var.equal = TRUE)
