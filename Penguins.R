print(Penguins)
dataset <-data.frame(Penguins)
print(dataset)

attach(dataset)

# Part A: Variance Test
# Is the variance of body mass different from each sex of the penguin?
# Step 0: Assumptions
# Assume that the samples are random and independent. Distribution of body mass is approximately normal
# Steps 1/2: Null Hypothesis and Alternative Hypothesis
# Ho: The variances for body mass are equal for male and female to 1
# Ha: The variances are not equal to 1
# Step 3: Test Statistic
# ratio of variances = 0.7153681
var.test(body_mass_g ~ sex, dataset, alternative = "two.sided")
# Step 4: Rejection Region
# alpha / 2 
# 0.05 / 2 = 0.025
# P-Value = 0.03198
# P-Value < alpha
# 0.03198 < 0.05
# Step 5: Conclusion
# At alpha = 0.05, we reject Ho. There is enough evidence to claim that the variances of the 2 sexes are not the same

# Part B: One Factor ANOVA
# Does the species of penguins have different mean weights?
hist(body_mass_g)
shapiro.test(body_mass_g)
# Step 0: Assumptions
# All 4 species of penguins of mean weights is approximately normal with equal variance. Treatments are randomly assigned
# Steps 1/2: Null Hypothesis and Alternative Hypothesis
# Ho: mean of Adelie = mean of Chinstrap = mean of Gentoo
# Ha: At least 2 means differ
# Step 3: Test Statistic
# F = 72.96
levels(species)
results <- aov(body_mass_g ~ species, data=Penguins)
summary(results)
# Step 4: Rejection Region
# P-Value = 4.9e-16
# P-Value < 0.001 < alpha = 0.05
# Step 5: Conclusion
# At alpha = 0.05, we reject Ho. There is sufficient evidence that the weights differs based on species

# Part C: Multiple Comparisons of Means
# Since we rejected Ho in Part (b), we should proceed with the multiple means of comparison
# Determine which penguin has the lowest mean and which penguin has the highest mean weight
table(species)
results_block <- aov(body_mass_g ~ species + as.factor(sex), data=Penguins)
TukeyHSD(results_block)
plot(TukeyHSD(results_block, which = "species"))
# Based on the data and the plot of the Differences in mean levels of species, Chinstrap has the lowest mean and Adelie has the 
# highest mean

# Part D: One Factor ANOVA Test
# Does the different islands have different average weight of penguins?
hist(body_mass_g)
shapiro.test(body_mass_g)
# Step 0: Assumptions
# All 3 islands of average weight of penguins is approximately normal with equal variance. Treatments are randomly assigned
# Steps 1/2: Null Hypothesis and Alternative Hypothesis
# Ho: mean of Biscoe = mean of Dream = mean of Torgersen
# Ha: At least 2 means differ
# Step 3: Test Statistic
levels(island)
results1 <- aov(body_mass_g ~ island, data=Penguins)
summary(results1)
# F = 105.1
# Step 4: Rejection Region
# P-Value = <2e-16
# P-Value < 0.001 < alpha = 0.05
# Step 5: Conclusion
# At alpha = 0.05 we reject Ho. There is sufficient evidence that different islands have different average weight of penguins

# Part E: Two Factor ANOVA Test
# Conduct a two-factor ANOVA with species as one factor and sex as another factor to compare the mean weights of penguins
mod <- aov(body_mass_g ~ sex * species, data = Penguins)
hist(mod$residuals)
shapiro.test(mod$residuals)
# Step 0: Assumptions
# Normality and constant variance for the response at each treatment. Random and independent samples
# Step 1/2: Null Hypothesis and Alternative Hypothesis
# Ho: There is no difference among species and sex treatment means
# Ha: At least 2 treatment means differ
# Step 3: Test Statistic
results_twofactor <- aov(body_mass_g ~ species * sex, data=Penguins)
summary(results_twofactor)
# F = 758.358
# F = 387.460
# Step 4: Rejection Region
# P-Value = <2e-16
# P-Value < 0.0001 < alpha = 0.05 for both species and sex factors
# Step 5: Conclusion
# At alpha = 0.05, we reject Ho and conclude that at least 2 treatment means differ
# Step 0: Assumptions
# Normality and constant variance for the response at each treatment. Random and independent samples
# Step 1: Null Hypothesis and Alternative Hypothesis
# Ho: Factors species and sex don't interact
# Ha: Factors species and sex do interact
# Step 3: Test Statistic
# F = 8.757
# Step 4: Rejection Region
# P-Value = 0.000197
# P-Value < 0.001 < alpha = 0.05
# Step 5: Conclusion
# At alpha = 0.05, we conclude that the factors species and sex interact