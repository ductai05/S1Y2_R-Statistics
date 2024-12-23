# Dinh Duc Tai - 23122013
# 23TNT1A - TH XSTK - Week 8

# BT Week7:

#Bai 05
ktc.tb <- function(mean, adj, s, n, alpha) {
        if (adj == TRUE) { #Da biet DLC cua tong the
                epsilon = qnorm(1 - alpha / 2) * s / sqrt(n)
        } else {
                if (n >= 30) {
                        epsilon = qnorm(1 - alpha / 2) * s / sqrt(n)
                } else {
                        epsilon = qt(1 - alpha / 2, df = n - 1) * s / sqrt(n)
                }
        }
        return(c(mean - epsilon, mean + epsilon))
}

mean <- 25
adj <- FALSE # Khong biet do lech chuan tong the
s <- 3      # Do lech chuan mau
n <- 40
alpha <- 0.01

khoang_tin_cay <- ktc.tb(mean, adj, s, n, alpha)
print(khoang_tin_cay)


#Bai 06
ktc.tb.mau <- function(x, adj, s, alpha) {
        n = length(x)
        mean = mean(x)
        if (adj == F) # Chua biet DLC tong the
                s = sd(x)
        ktc.tb(mean, adj, s, n, alpha)
}

x = rnorm(30, 15, 4)
adj <- FALSE # Khong biet do lech chuan tong the
s <- 4      # Do lech chuan mau
alpha <- 0.01
ktc.tb.mau(x, adj, s, alpha)

#Bai 07

# Du Lieu Cho Truoc
X <- c(12.00, 12.05, 12.10, 12.15, 12.20, 12.25, 12.30, 12.35, 12.40)
n <- c(2, 3, 7, 9, 10, 8, 6, 5, 3)

# Tinh trung binh mau (x¯), va do lech chuan mau (s) cua du lieu nhom
weighted.mean <- function(x, n){
  sum(x*n)/sum(n)
}
sample.mean <- weighted.mean(X,n)

sample.variance <- function(x, n, mean){
  sum(n * (x - mean)^2)/(sum(n)-1)
}
sample.sd <- sqrt(sample.variance(X,n,sample.mean))

# Tinh sai so tieu chuan cua trung binh
standard.error <- sample.sd / sqrt(sum(n))

# Tinh bac tu do (df)
df <- sum(n) - 1

# Tim gia tri t-critical cho khoang tin cay 95% voi df hien tai
alpha <- 0.05
t.critical <- qt(1-alpha/2, df)

# Tinh Sai So Bien (MOE)
MOE <- t.critical * standard.error

# Tinh khoang tin cay
lower.limit <- sample.mean - MOE
upper.limit <- sample.mean + MOE

# In ket qua
cat("Trung Binh Mau:", sample.mean, "\n")
cat("Do Lech Chuan Mau:", sample.sd, "\n")
cat("Bac Tu Do:", df, "\n")
cat("Gia Tri T-critical:", t.critical, "\n")
cat("Sai So Bien:", MOE, "\n")
cat("Khoang Tin Cay 95%:", paste("(", lower.limit, ", ", upper.limit, ")"), "\n")


# Week 8:

## Bai 1:
# 1. Doc du lieu
data <- read.csv("D:/23122013/profit.csv", header = TRUE)
sales <- data[,1] # L?y c?t doanh s?

head(sales) 
summary(sales)

# 2. Ve histogram
hist(sales, main = "Phân ph?i doanh s? bán hàng", xlab = "Doanh s? (tri?u d?ng)", ylab = "T?n s?")


# 3. Uoc luong doanh so trung binh cua "ngay ban dat hang"
# L?c ra các ngày bán d?t hàng (doanh s? > 65)
hot_sales <- sales[sales > 65]

# Trung binh, do lech chuan mau
mean_hot_sales <- mean(hot_sales)
sd_hot_sales <- sd(hot_sales)

# Kich thuoc mau
n_hot_sales <- length(hot_sales)

# Sai so chuan (standard error) [s/sqrt(n)]
standard_error <- sd_hot_sales / sqrt(n_hot_sales)


# Tinh khoang tin cay 99%
alpha <- 0.01
critical_value <- qt(1 - alpha/2, df = n_hot_sales - 1)

margin_of_error <- critical_value * standard_error

lower_bound <- mean_hot_sales - margin_of_error
upper_bound <- mean_hot_sales + margin_of_error

cat("U?c lu?ng doanh s? trung bình c?a ngày 'bán d?t hàng' v?i d? tin c?y 99%:\n")
cat("Kho?ng tin c?y:", lower_bound, "d?n", upper_bound, "tri?u d?ng\n")


# 4. Kiem dinh gia thuyet
# (H0): Doanh so trung binh <= 60 trieu dong
# (H1): Doanh so trung binh > 60 trieu dong (kiem dinh mot phia)

# Trung binh mau
mean_sales <- mean(sales)

# Do lech chuan mau
sd_sales <- sd(sales)

# Kich thuoc mau
n_sales <- length(sales)

# Sai so chuan
standard_error_all <- sd_sales/sqrt(n_sales)

# Gia tri kiem dinh t
t_statistic <- (mean_sales - 60) / standard_error_all

# p-gia tri (kiem dinh mot phia)
p_value <- pt(t_statistic, df = n_sales - 1, lower.tail = FALSE)

# In kq:
cat("Ket qua kiem dinh gia thuyet ve phuong phap moi:\n")
cat("Gia tri kiem dinh t:", t_statistic, "\n")
cat("P-value:", p_value, "\n")

# So sanh p-value voi muc y nghia alpha = 0.01
alpha <- 0.01
if (p_value < alpha) {
  cat("Co du bang chung de bac bo gia thuyet H0. \nPhuong thuc ban hang moi co y nghia thong ke o muc y nghia 1% (doanh so trung binh tang).\n")
} else {
  cat("Khong du bang chung de bac bo gia thuyet H0. \nPhuong thuc ban hang moi chua co y nghia thong ke o muc y nghia 1% (khong du bang chung cho thay doanh so trung binh tang).\n")
}


## Bai 2:

# 1. Tao du lieu
# Du lieu diem va tan so
scores <- c(5, 6, 7, 8, 9, 10)
frequencies <- c(5, 10, 15, 20, 12, 8)

# Bien doi thanh vector
data <- rep(scores, frequencies)

# 2. a) Ve stem & leaf plot
stem(data, scale=1)

# b) test.geq.oneside; H1: µ > 8
test.geq.oneside <- function(x, mu, alpha){
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    se <- sd_x/sqrt(n) # standard error
    t_statistic <- (mean_x - mu)/se
    p_value <- pt(t_statistic, df=n-1, lower.tail=FALSE)

    cat("Ket qua kiem dinh gia thuyet mot phia (H1: µ > mu0):\n")
    cat("Gia tri kiem dinh t:", t_statistic, "\n")
    cat("P-value:", p_value, "\n")
    if(p_value < alpha){
       cat("Co du bang chung de bac bo H0. \n")
        cat("Ket luan: Diem trung binh lon hon ",mu, " voi muc y nghia ",alpha, "\n")
    } else {
       cat("Khong du bang chung de bac bo H0.\n")
        cat("Ket luan: Khong du bang chung de ket luan diem trung binh lon hon ",mu, " voi muc y nghia ",alpha,"\n")
    }

    return(list(statistic = t_statistic, p.value = p_value))
}

# Thuc hien kiem dinh
result_geq <- test.geq.oneside(data, mu = 8, alpha = 0.05)

# So sanh voi t.test
t.test(data, mu=8, alternative = "greater")

# c) test.leq.oneside; H1: µ < 8
test.leq.oneside <- function(x, mu, alpha){
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    se <- sd_x/sqrt(n)
    t_statistic <- (mean_x - mu)/se
    p_value <- pt(t_statistic, df=n-1, lower.tail=TRUE)
    cat("Ket qua kiem dinh gia thuyet mot phia (H1: µ < mu0):\n")
    cat("Gia tri kiem dinh t:", t_statistic, "\n")
    cat("P-value:", p_value, "\n")
    if(p_value < alpha){
       cat("Co du bang chung bac bo H0. \n")
        cat("Ket luan: Diem trung binh nho hon ",mu, " voi muc y nghia ",alpha,"\n")
    } else {
       cat("Khong du bang chung de bac bo H0.\n")
        cat("Ket luan: Khong du bang chung de ket luan diem trung binh nho hon",mu, " voi muc y nghia ",alpha,"\n")
    }
      return(list(statistic = t_statistic, p.value = p_value))
}


# Thuc hien kiem dinh
result_leq <- test.leq.oneside(data, mu = 8, alpha = 0.05)


# So sanh voi ket qua t.test
t.test(data, mu=8, alternative = "less")
