# Dinh Duc Tai - 23122013
# TH XSTK 23TNT1A - Week6

### Bai 1

# Tao vector x, y
x <- c(1, 2, 5, 7, -3, 0, 5, 1, 5, 6)
y <- c(2, 2, 0, -5, 7, 8, 11, 9, 3, 2)

# a. 
sum_xy <- x + y
prod_xy <- x * y
diff_xy <- x - y

# b. 
z <- x[x %% 2 == 0]
t <- y[y %% 2 != 0]

# c. 
x_positive <- x[x > 0]
y_positive <- y[y > 0]

# d. 
mean_x <- mean(x)
sd_x <- sd(x)
se_x <- sd_x / sqrt(length(x))

mean_y <- mean(y)
sd_y <- sd(y)
se_y <- sd_y / sqrt(length(y))

# e. 
max_x <- max(x)
min_x <- min(x)
max_y <- max(y)
min_y <- min(y)

# f. 
sorted_x <- sort(x)
sorted_y <- sort(y, decreasing = TRUE)

# g. 
save(x, y, file = "xy_data.rda")

# In ket qua
list(
  sum_xy = sum_xy,
  prod_xy = prod_xy,
  diff_xy = diff_xy,
  z = z,
  t = t,
  x_positive = x_positive,
  y_positive = y_positive,
  mean_x = mean_x,
  sd_x = sd_x,
  se_x = se_x,
  mean_y = mean_y,
  sd_y = sd_y,
  se_y = se_y,
  max_x = max_x,
  min_x = min_x,
  max_y = max_y,
  min_y = min_y,
  sorted_x = sorted_x,
  sorted_y = sorted_y
)


### Bai 2

# a. Nhap so lieu tu file data01.csv
data1 <- read.csv("data01.csv")
# data1 <- read.csv("/kaggle/input/xstk-hcmus/XSTK/data01.csv")

# FPSA & TPSA
mean_FPSA <- mean(data1$FPSA, na.rm = TRUE)
var_FPSA <- var(data1$FPSA, na.rm = TRUE)
median_FPSA <- median(data1$FPSA, na.rm = TRUE)

mean_TPSA <- mean(data1$TPSA, na.rm = TRUE)
var_TPSA <- var(data1$TPSA, na.rm = TRUE)
median_TPSA <- median(data1$TPSA, na.rm = TRUE)

# b. Ve bieu do duong va boxplot cho FPSA va TPSA
library(ggplot2)

# Bieu do dang duong
ggplot(data1, aes(x = 1:nrow(data1))) +
  geom_line(aes(y = FPSA, color = "FPSA")) +
  geom_line(aes(y = TPSA, color = "TPSA")) +
  labs(title = "Bieu do dang duong cua FPSA va TPSA", x = "Chi so", y = "Gia tri") +
  scale_color_manual(values = c("FPSA" = "blue", "TPSA" = "red")) +
  theme_minimal()

# Boxplot
ggplot(data1) +
  geom_boxplot(aes(y = FPSA, fill = "FPSA")) +
  geom_boxplot(aes(y = TPSA, fill = "TPSA"), alpha = 0.5) +
  labs(title = "Boxplot cua FPSA va TPSA", y = "Gia tri") +
  scale_fill_manual(values = c("FPSA" = "blue", "TPSA" = "red")) +
  theme_minimal()

# c. FPSA co K=0 va K=1
fpsa_k0 <- data1[data1$K == 0, "FPSA"]
fpsa_k1 <- data1[data1$K == 1, "FPSA"]

# d. Doc file data02.csv va merge 2 frame theo bien K
data2 <- read.csv("data02.csv")
# data2 <- read.csv("/kaggle/input/xstk-hcmus/XSTK/data02.csv")
merged_data <- cbind(data1, data2)
merged_data <- merged_data[, !duplicated(colnames(merged_data))]
# e. tPSA
merged_data$tPSA <- ifelse(merged_data$Age <= 30, 0,
                            ifelse(merged_data$Age <= 50, 1, 2))

# Bang thong ke tPSA
tPSA_table <- table(merged_data$tPSA)

# In ket qua
list(
  mean_FPSA = mean_FPSA,
  var_FPSA = var_FPSA,
  median_FPSA = median_FPSA,
  mean_TPSA = mean_TPSA,
  var_TPSA = var_TPSA,
  median_TPSA = median_TPSA,
  fpsa_k0 = fpsa_k0,
  fpsa_k1 = fpsa_k1,
  merged_data = merged_data,
  tPSA_table = tPSA_table
)

### Bai 3

# Nhap du lieu tu ban phim
sinh_vien <- 1:10  # Tao danh sach sinh vien tu 1 den 10
diem_cau_hoi1 <- numeric(10)  
diem_cau_hoi2 <- numeric(10)  
diem_cau_hoi3 <- numeric(10)  

Nhap diem cho tung sinh vien
for (i in 1:10) {
  cat("Nhap diem cho sinh vien", i, "cau hoi 1: ")
  diem_cau_hoi1[i] <- as.numeric(readline())
  
  cat("Nhap diem cho sinh vien", i, "cau hoi 2: ")
  diem_cau_hoi2[i] <- as.numeric(readline())
  
  cat("Nhap diem cho sinh vien", i, "cau hoi 3: ")
  diem_cau_hoi3[i] <- as.numeric(readline())
}

# diem_cau_hoi1 = c(1,4,7,10,8,5,2,2,5,8)
# diem_cau_hoi2 = c(2,5,8,10,7,4,1,3,6,9)
# diem_cau_hoi3 = c(3,6,9,9,6,3,1,4,7,10)

# Tao bang du lieu
diem_data <- data.frame(Sinh_Vien = sinh_vien,
                         Cau_Hoi_1 = diem_cau_hoi1,
                         Cau_Hoi_2 = diem_cau_hoi2,
                         Cau_Hoi_3 = diem_cau_hoi3)

# Hien thi bang du lieu
print(diem_data)

# c. Ve bieu do diem cho tung cau hoi
boxplot(diem_data[, -1], 
        main = "Bieu do diem cho cac cau hoi",
        xlab = "Cau hoi",
        ylab = "Diem",
        col = c("red", "green", "blue"),
        names = c("Cau hoi 1", "Cau hoi 2", "Cau hoi 3"))

# d. Ve bieu do bar dang nam ngang cho cau hoi 2 va 3
diem_matrix <- t(as.matrix(diem_data[, c("Cau_Hoi_2", "Cau_Hoi_3")]))

barplot(diem_matrix,
        main = "Bieu do diem cho cau hoi 2 va 3 (Nam ngang)",
        xlab = "Diem",
        ylab = "Sinh vien",
        col = c("green", "blue"),
        names.arg = paste("SV", 1:ncol(diem_matrix)),
        horiz = TRUE,
        legend.text = c("Cau hoi 2", "Cau hoi 3"))


### Bai 4

par(mfrow=c(2,2))  
# a. pp nhi thuc
binom_data <- rbinom(100, size=60, prob=0.4)
hist(binom_data, main="Phan phoi nhi thuc\nn=60, p=0.4", 
     xlab="Gia tri", ylab="Tan so")

# b. pp poisson
pois_data <- rpois(100, lambda=4)
hist(pois_data, main="Phan phoi Poisson\nλ=4",
     xlab="Gia tri", ylab="Tan so")

# c. pp chuan
norm_data <- rnorm(100, mean=50, sd=4)
# histogram va duong cong mat do
hist(norm_data, probability=TRUE, main="Phan phoi chuan\nμ=50, σ=4",
     xlab="Gia tri", ylab="Mat do")
curve(dnorm(x, mean=50, sd=4), add=TRUE, col="red")

# d. pp mu
exp_data <- rexp(100, rate=1/25)
hist(exp_data, probability=TRUE, main="Phan phoi mu\nλ=1/25",
     xlab="Gia tri", ylab="Mat do")
curve(dexp(x, rate=1/25), add=TRUE, col="red")


### Bai 5

if(!require(readxl)) install.packages("readxl")
library(readxl)

# a. 
diesel_engine <- read.table("diesel_engine.dat", header = TRUE)
diesel_time <- read_excel("diesel_time.xls")
# diesel_engine <- read.table("/kaggle/input/xstk-hcmus/XSTK/diesel_engine.dat", header = TRUE)
# diesel_time <- read_excel("/kaggle/input/xstk-hcmus/XSTK/diesel_time.xls")

# b.
names(diesel_engine)
names(diesel_time)

# c. 
sum(is.na(diesel_engine)) # đếm số NA
diesel_engine$speed[is.na(diesel_engine$speed)] <- 1500
diesel_engine$load[is.na(diesel_engine$load)] <- 20

# d. 
summary_alcohol <- summary(diesel_engine$alcohol)
var_alcohol <- var(diesel_engine$alcohol)
sd_alcohol <- sd(diesel_engine$alcohol)

# e. 
diesel <- merge(diesel_engine, diesel_time)

# f. 
subset(diesel, delay < 1000)$run

# g. 
sum(diesel$timing == 30)

# h. 
library(ggplot2)
boxplot(list(Speed=diesel$speed, Timing=diesel$timing, Delay=diesel$delay),
        main="Boxplot of Speed, Timing and Delay")

# i. 
par(mfrow=c(1,2))
plot(diesel$timing, diesel$speed, main="Timing vs Speed")
plot(diesel$temp, diesel$press, main="Temperature vs Pressure")

# j. 
diesel$load <- as.factor(diesel$load)

# k. 
breaks <- seq(min(diesel$delay), max(diesel$delay), length.out = 5)
delay_cut <- cut(diesel$delay, breaks = breaks)
delay_table <- table(delay_cut)
barplot(delay_table, main="Distribution of Delay")

# l. 
custom_breaks <- c(0.283, 0.7, 0.95, 1.2, 1.56)
delay_cut_custom <- cut(diesel$delay, breaks = custom_breaks)
delay_table_custom <- table(delay_cut_custom)
barplot(delay_table_custom, main="Distribution of Delay (Custom Breaks)")


### Cau 6

data <- data.frame(
  year = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979),
  snow_cover = c(6.5, 12.0, 14.9, 10.0, 10.7, 7.9, 21.9, 12.5, 14.5, 9.2)
)
print(data)

plot(data$year, data$snow_cover, type = "o", col = "blue",
     xlab = "Year", ylab = "Snow Cover (cm)",
     main = "Snow Cover Over Years")

hist(data$snow_cover, col = "lightblue", 
     xlab = "Snow Cover (cm)", 
     main = "Histogram of Snow Cover")

# log_snow_cover
data$log_snow_cover <- log(data$snow_cover)

plot(data$year, data$log_snow_cover, type = "o", col = "red",
     xlab = "Year", ylab = "Log(Snow Cover)",
     main = "Logarithm of Snow Cover Over Years")

hist(data$log_snow_cover, col = "pink", 
     xlab = "Log(Snow Cover)", 
     main = "Histogram of Log(Snow Cover)")


### Cau 7

data <- data.frame(
  Temperature = c(53, 57, 63, 70, 70, 75),
  Erosion = c(3, 1, 1, 1, 1, 0),
  Blowby = c(2, 0, 0, 0, 0, 2),
  Total = c(5, 1, 1, 1, 1, 1)
)

print(data)

plot(data$Temperature, data$Total, type = "o", col = "blue",
     xlab = "Temperature (F)", ylab = "Total Incidents",
     main = "Total Incidents vs Temperature",
     pch = 16, lwd = 2)

grid()


### Cau 8

# a.
lamphat1 <- data.frame(
  Nam = 1960:1980,
  US = c(1.5, 1.1, 1.1, 1.2, 1.4, 1.6, 2.8, 2.8, 4.2, 5.0, 5.9, 4.3, 3.6, 6.2, 10.9, 9.2, 5.8, 6.4, 7.6, 11.4, 13.6),
  Anh = c(1.0, 3.4, 4.5, 2.5, 3.9, 4.6, 3.7, 2.4, 4.8, 5.2, 6.5, 9.5, 6.8, 8.4, 16, 24.2, 16.5, 15.9, 8.3, 13.4, 18)
)

lamphat2 <- data.frame(
  Nam = 1960:1980,
  Nhat = c(3.6, 5.4, 6.7, 7.7, 3.9, 6.5, 6.0, 4.0, 5.5, 5.1, 7.6, 6.3, 4.9, 12.0, 24.6, 11.7, 9.3, 8.1, 3.8, 3.6, 8.0),
  Duc = c(1.5, 2.3, 4.5, 3.0, 2.3, 3.4, 3.5, 1.5, 18, 2.6, 3.7, 5.3, 5.4, 7.0, 7, 5.9, 4.5, 3.7, 2.7, 4.1, 5.5)
)

# b.
lamphat <- merge(lamphat1, lamphat2, by = "Nam")
print(lamphat)

# c. Dem so nam ti le lam phat > 5%

print("So nam ti le lam phat > 5%:")
count_above_5 <- colSums(lamphat[, -1] > 5)
print(count_above_5)

# d. Do thi phan tan
plot(lamphat$Nam, lamphat$US, type = "o", col = "blue", pch = 16,
     xlab = "Nam", ylab = "Ti le lam phat(%)",
     main = "Ti le lam phat theo thoi gian",
     ylim = range(lamphat[, -1]))

lines(lamphat$Nam, lamphat$Anh, type = "o", col = "red", pch = 16)
lines(lamphat$Nam, lamphat$Nhat, type = "o", col = "green", pch = 16)
lines(lamphat$Nam, lamphat$Duc, type = "o", col = "purple", pch = 16)

legend("topleft", legend = c("US", "Anh", "Nhat", "Duc"),
       col = c("blue", "red", "green", "purple"), pch = 16, lty = 1)

# e. Thong ke mo ta
summary_stats <- function(x) {
  mean_val <- mean(x)
  median_val <- median(x)
  max_val <- max(x)
  min_val <- min(x)
  sd_val <- sd(x)
  se_val <- sd_val / sqrt(length(x))
  
  return(c(Mean = mean_val, Median = median_val, Max = max_val, Min = min_val, 
           SD = sd_val, SE = se_val))
}

stats_US <- summary_stats(lamphat$US)
stats_Anh <- summary_stats(lamphat$Anh)
stats_Nhat <- summary_stats(lamphat$Nhat)
stats_Duc <- summary_stats(lamphat$Duc)

summary_table <- data.frame(US = stats_US, Anh = stats_Anh, Nhat = stats_Nhat, Duc = stats_Duc)
print(summary_table)

# f.
country_sd <- summary_table["SD", ]
print(country_sd)

# Quoc gia co do lech chuan lon nhat -> bien thien lon nhat
most_variable_country <- names(country_sd)[which.max(country_sd)]
cat("Quoc gia co lam phat bien thien nhieu nhat la:", most_variable_country, "\n")

# g.
lamphat1 <- subset(lamphat, Nam != 1980)
print(lamphat1)

# h.
X <- lamphat1$Nam
Y <- lamphat1$US
n <- length(X)

X_mean <- mean(X)
Y_mean <- mean(Y)

beta_2 <- sum((X - X_mean) * (Y - Y_mean)) / sum((X - X_mean)^2)

beta_1 <- Y_mean - beta_2 * X_mean

cat("He so beta_1:", beta_1, "\n")
cat("He so beta_2:", beta_2, "\n")

plot(X, Y, col = "blue", pch = 16, xlab = "Nam", ylab = "Ti le lam phat (%)",
     main = "Hoi quy ti le lam phat cua US theo thoi gian")

abline(a = beta_1, b = beta_2, col = "red", lwd = 2)

legend("topleft", legend = c("Du lieu thuc te", "Hoi quy tuyen tinh"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))

# i.

# Du doan lam phat 1980
year_1980 <- 1980
predicted_inflation_1980 <- beta_1 + beta_2 * year_1980
cat("Ti le lam phat du doan nam 1980:", predicted_inflation_1980, "\n")

# Thuc te
actual_inflation_1980 <- lamphat$US[lamphat$Nam == 1980]
cat("Ti le lam phat thuc te nam 1980:", actual_inflation_1980, "\n")

# So sanh chenh lech
difference <- abs(predicted_inflation_1980 - actual_inflation_1980)
cat("Chenh lech giua du doan va thuc te la:", difference, "\n")
