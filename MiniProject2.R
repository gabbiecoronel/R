df <- data.frame(Penguins)
print(df)
attach(df)

# Validate normal assumption
hist(body_mass_g)

# Shapiro-Wilk Test for Nomrality
shapiro.test(body_mass_g)

# ANOVA
levels(species)
results <- aov(body_mass_g ~ species, data=df)
summary(results)

# Nonparametric Test
np_results <- kruskal.test(body_mass_g ~ species, data=df)
print(np_results)

# Goodness of Fit Test
counts <- c(324, 284, 709)
exp_probs <- c(0.246, 0.216, 0.538)
chisq_results <- chisq.test(counts, p = exp_probs)

print(chisq_results)
print(chisq_results$expected)

# Chi-Square Test for Independence
treatment <- matrix(c(77, 19634, 833, 18908), nrow = 2, byrow = TRUE)
print(treatment)
dimnames(treatment) <- list(c("Vaccine","Placebo"), 
                        c("Infection", "No Infection"))
print(treatment)
results <- chisq.test(treatment)
print(results)
print(results$expected)