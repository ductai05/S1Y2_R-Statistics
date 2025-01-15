# Dinh Duc Tai - 23122013
# TH XSTK - Week 11 

# VD mo dau
x<-c(77,50,71,72,81,94,96,99,67)
y<-c(82,66,78,34,47,85,99,99,68)
plot(x,y)
kq = lm(y~x)
resid(kq)
coef(kq)[1]
coef(kq)[2]
###beta0<-....; beta1<-...
# beta1<-(coef(kq))[['x']]
# beta0<-(coef(kq))[['(Intercept)']]
beta1<-(coef(kq))[[2]]
beta0<-(coef(kq))[[1]]
## Mo hinh hoi quy
cat("Mo hinh hoi quy la : y=", beta0, "+", beta1,"x")

summary_kq = summary(kq)

correlation <- cor(x, y)
cat("He so tuong quan:", correlation , "\n")

r_squared <- summary_kq$r.squared
cat("He so xac dinh:", r_squared, "\n")

# BT 1->3: Ve do thi phan tan, duong hoi quy len cung 1 do thi.
# xuat ra mo hinh hoi quy tuyen tinh; thang du; he so tuong quan, he so xac dinh

# BT 4a->4d

### Bai 1

# 1. Du lieu
price <- c(300, 250, 400, 550, 317, 389, 425, 289, 389, 559)
bedrooms <- c(3, 3, 4, 5, 4, 3, 6, 3, 4, 5)

# 2. linear model
model = lm(price ~ bedrooms)
summary_model <- summary(model)
summary_model

# 3. Ve do thi phan tan
plot(bedrooms, price, main = "Do thi phan tan va duong hoi quy", xlab = 
       "bedrooms", ylab = "price", pch = 19, col = "blue") 
# 4. Them duong hoi quy
abline(model, col = "red", lwd = 2)

# 5. Thang du (residuals)
resid(model) 

# 6. He so (coefficients)
coef(model)
beta0 <- coef(model)[[1]]
beta1 <- coef(model)[[2]]
cat("Mo hinh hoi quy la : y=", beta0, "+", beta1,"x")

# 7. He so tuong quan
correlation_coefficient <- cor(bedrooms, price)
cat("He so tuong quan:", correlation_coefficient, "\n")

# 8. He so xac dinh
r_squared <- summary_model$r.squared
cat("He so xac dinh:", r_squared, "\n")


### Bai 2

# 1. Du lieu
beer <- c(5, 2, 9, 8, 3, 7, 3, 5, 3, 5)
BAL <- c(0.10, 0.03, 0.19, 0.12, 0.04, 0.095, 0.07, 0.06, 0.02, 0.05)

# 2. linear model
model = lm(BAL ~ beer) # (y ~ x)
summary_model <- summary(model)
summary_model

# 3. Ve do thi phan tan
plot(beer, BAL, main = "Do thi phan tan va duong hoi quy", xlab = 
       "beers", ylab = "BAL", pch = 19, col = "blue") 
# 4. Them duong hoi quy
abline(model, col = "red", lwd = 2)

# 5. Thang du (residuals)
resid(model) 

# 6. He so (coefficients)
coef(model)
beta0 <- coef(model)[[1]]
beta1 <- coef(model)[[2]]
cat("Mo hinh hoi quy la : y=", beta0, "+", beta1,"x")

# 7. He so tuong quan
correlation_coefficient <- cor(beer, BAL)
cat("He so tuong quan:", correlation_coefficient, "\n")

# 8. He so xac dinh
r_squared <- summary_model$r.squared
cat("He so xac dinh:", r_squared, "\n")


### Bai 3

# 1. Du lieu
Elevation <- c(600, 1000, 1250, 1600, 1800, 2100, 2500, 2900)
Temperature <- c(56, 54, 56, 50, 47, 49, 47, 45)

# 2. linear model
model = lm(Temperature ~ Elevation) # (y ~ x)
summary_model <- summary(model)
summary_model

# 3. Ve do thi phan tan
plot(Elevation, Temperature, main = "Do thi phan tan va duong hoi quy", xlab = 
       "Elevation(ft)", ylab = "Temperature(F)", pch = 19, col = "blue") 
# 4. Them duong hoi quy
abline(model, col = "red", lwd = 2)

# 5. Thang du (residuals)
resid(model) 

# 6. He so (coefficients)
coef(model)
beta0 <- coef(model)[[1]]
beta1 <- coef(model)[[2]]
cat("Mo hinh hoi quy la : y=", beta0, "+", beta1,"x")
cat("Toc do giam nhiet la: ", beta1 * 1000 * 5/9 ,"do C")
# 7. He so tuong quan
correlation_coefficient <- cor(Elevation, Temperature)
cat("He so tuong quan:", correlation_coefficient, "\n")

# 8. He so xac dinh
r_squared <- summary_model$r.squared
cat("He so xac dinh:", r_squared, "\n")


### Bai 4

setwd("D:/23122013")

# 1. Du lieu
data <- read.csv("rocket.motor.csv")
age <- data$age
streng <- data$streng

# 2. linear model
model <- lm(streng ~ age, data = data)
summary(model)

# (a)
# 3. Ve do thi phan tan
plot(age, streng, xlab = "Tuoi (tuan)", ylab = "Luc day", 
     main = "Do thi phan tan cua luc day theo tuoi", pch = 19, col = "blue")

# 4. Them duong hoi quy
abline(model, col = "red", lwd = 2)

# (b)
# 5. Slope, intercept, sigma^2
slope = coef(model)[2]
intercept = coef(model)[1]
cat("Slope = ", slope)
cat("Intercept = ", intercept)

sigma2_estimate <- summary(model)$sigma^2
cat("sigma2_estimate = ", sigma2_estimate)

# (c)
# 6. Uoc luong luc day tu chat no 20 tuan tuoi
predicted_strength <- coef(model)[1] + coef(model)[2] * 20
print(predicted_strength)

# (d)
# 7. Nhan xet
cat("Quan he tuyen tinh giua luc day va tuoi van co sai so.")
cat("Bien hoi quy tuoi la mot lua chon hop li cho mo hinh nay.")
