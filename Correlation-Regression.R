# Linear Regression model
# y = beta0 + beta1 * x + epsilon
# unknown: beta0, beta1: estimate from data

# 1. Data
x = c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37)
y = c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)

# 2. Linear model
model = lm(y ~ x)
summary = summary(model)
summary

# 3. Scatter plot, Regression line
plot(x,y, main = "Scatter plot and Regression line",
     xlab = "x", ylab = "y", pch = 19, col = "blue") 
abline(model, col = "red", lwd = 2)

# 4. Residuals (Thang du)
resid(model) 

# 5. Coefficients (He so)
coef(model)
beta0 <- coef(model)[[1]]
beta1 <- coef(model)[[2]]
cat("Linear Regression model: y=", beta0, "+", beta1,"x")

# 6. Correlation Coefficient (He so tuong quan)
correlation_coefficient <- cor(x, y)
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# 7. Coefficient of determination (R^2 - He so xac dinh)
r_squared <- summary$r.squared
cat("Coefficient of determination:", r_squared, "\n")

# 8. MSE
sigma2_estimate <- summary(model)$sigma^2
cat("sigma2_estimate = ", sigma2_estimate)

# 9. Confidence Interval
confint(model, level = 0.95)

# 10. Analysis of variance ANOVA
anova(model)
