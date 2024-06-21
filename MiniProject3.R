df <- data.frame(STA4163_Mini_Project_3_Dataset_1)
print(df)
attach(df)

# simple linear regression
model <- lm (Rating ~ Income, data = dataset)
plot(Income, Rating, xlab = "Income", ylab = "Credit Score Rating", main = "Income and Credit Score Rating")
summary(model)
anova(model)
confint(model, level = 0.95)
cor(Income, Rating)
plot(model)
shapiro.test(model$residuals)
predict(model, newdata=data.frame(Income = 56), interval="prediction", level=0.95)

# multiple linear regression
multiple_model <- lm(Rating ~ Income + Age + Education, data = df)
summary(multiple_model)
anova(multiple_model)