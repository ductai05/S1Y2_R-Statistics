# Dinh Duc Tai - 23122013
# TH XSTK - Week 10
setwd("D:/23122013") 

################ BAI 1 ######################

# Doc du lieu
data <- read.csv("volume.csv")
machine1 <- data$machine1
machine2 <- data$machine2

# sigma de bai cho
sigma1 <- 0.002
sigma2 <- 0.0025

# Muc y nghia alpha
alpha <- 0.05

# Kiem dinh t cho hai mau doc lap (kiem dinh hai phia)
t.test(machine1, machine2, alternative = "two.sided", var.equal = FALSE)

# leq oneside
test.leq.oneside <- function(x, y, mu0, sigma1, sigma2, alpha) {
  n1 <- length(x)
  n2 <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  # mu0 <- 0  # Gia thuyet H0: mu1 - mu2 = 0

  z_stat <- (mean_x - mean_y - mu0) / sqrt((sigma1^2 / n1) + (sigma2^2 / n2))

  p_value <- pnorm(z_stat) # Su dung pnorm cho phan phoi chuan

  if (p_value < alpha) {
    result <- "Bac bo H0"
  } else {
    result <- "Chap nhan H0"
  }

  return(list(statistic = z_stat, p.value = p_value, result = result, alternative = "mu1 - mu2 < 0"))
}

# geq oneside
test.geq.oneside <- function(x, y, mu0, sigma1, sigma2, alpha) {
  n1 <- length(x)
  n2 <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  # mu0 <- 0  # Gia thuyet H0: mu1 - mu2 = 0

  z_stat <- (mean_x - mean_y - mu0) / sqrt((sigma1^2 / n1) + (sigma2^2 / n2))

  p_value <- 1 - pnorm(z_stat) # Su dung pnorm cho phan phoi chuan

  if (p_value < alpha) {
    result <- "Bac bo H0"
  } else {
    result <- "Chap nhan H0"
  }

  return(list(statistic = z_stat, p.value = p_value, result = result, alternative = "mu1 - mu2 > 0"))
}

# ----------------------------------------------------
# d) Kiem dinh gia thuyet H0: µ1 - µ2 = 0; doi thuyet H1: µ1 - µ2 < 0
# ----------------------------------------------------

result_leq <- test.leq.oneside(machine1, machine2, 0, sigma1, sigma2, alpha)
print(result_leq)

# ----------------------------------------------------
# e) Kiem dinh gia thuyet H0: µ1 - µ2 = 0, doi thuyet H1: µ1 - µ2 > 0
# ----------------------------------------------------

result_geq <- test.geq.oneside(machine1, machine2, 0, sigma1, sigma2, alpha)
print(result_geq)


################ BAI 2 ######################

setwd("D:/23122013")
# Doc du lieu tu file diameter.csv
data <- read.csv("diameter.csv")

# Trich xuat du lieu duong kinh cua hai may
may1 <- data$extru.ma.1
may2 <- data$extru.ma.2

# ==================================================
# a) Kiem tra xem hai may co cho ra cac thanh thep voi duong kinh khac nhau hay khong
# ==================================================

# Gia thuyet:
# H0: Duong kinh trung binh cua thanh thep do hai may san xuat la nhu nhau (µ1 = µ2)
# H1: Duong kinh trung binh cua thanh thep do hai may san xuat la khac nhau (µ1 ? µ2)

# Muc y nghia
alpha <- 0.05

# Kiem dinh t cho hai mau doc lap (vi de bai gia su phuong sai bang nhau)
# alternative = "two.sided" de thuc hien kiem dinh hai phia (khac nhau)
kiem_dinh_a <- t.test(may1, may2, alternative = "two.sided", var.equal = TRUE)

# In ket qua kiem dinh
print(kiem_dinh_a)

# Ket luan
if (kiem_dinh_a$p.value < alpha) {
  cat("Bac bo gia thuyet H0 voi muc y nghia", alpha, "\n")
  cat("Vay, co bang chung cho thay duong kinh trung binh cua thanh thep do hai may san xuat la khac nhau.\n")
} else {
  cat("Khong du bang chung de bac bo gia thuyet H0 voi muc y nghia", alpha, "\n")
  cat("Vay, khong co bang chung cho thay duong kinh trung binh cua thanh thep do hai may san xuat la khac nhau.\n")
}

# ==================================================
# b) Tim P-gia tri cho thong ke o cau a
# ==================================================

p_value_a <- kiem_dinh_a$p.value

cat("P-gia tri cho kiem dinh o cau a la:", p_value_a, "\n")

# ==================================================
# c) Uoc luong su sai biet ve duong kinh trung binh cua cac thanh thep do hai may nay san xuat voi do tin cay 95%
# ==================================================

# Uoc luong khoang tin cay cho su khac biet ve trung binh (µ1 - µ2)
# Su dung ket qua kiem dinh t o cau a (da luu trong bien kiem_dinh_a)
khoang_tin_cay <- kiem_dinh_a$conf.int

cat("Khoang tin cay 95% cho su khac biet ve duong kinh trung binh giua hai may la:", khoang_tin_cay, "\n")

# ==================================================
# d) Viet ham test.leq.oneside
# ==================================================

test.leq.oneside <- function(x, y, mu0, alpha) {
  # Kiem dinh gia thuyet H0: µ = µ0 (µ >= µ0) va doi thuyet H1: µ < µ0
  # Voi µ = µ1 - µ2 (µ1, µ2 lan luot la duong kinh trung binh cua cac thanh thep do may so 1 va may so 2 san xuat ra)
  # x, y la hai vector du lieu chua duong kinh cua cac thanh thep do may so 1 va may so 2 san xuat ra
  # mu0 la gia tri gia thuyet cua µ
  # alpha la muc y nghia

  # Thuc hien kiem dinh t cho hai mau doc lap (gia su phuong sai bang nhau)
  kiem_dinh <- t.test(x, y, alternative = "less", mu = mu0, var.equal = TRUE)

  # Tra ve ket qua
  return(list(statistic = kiem_dinh$statistic,
              p.value = kiem_dinh$p.value,
              result = ifelse(kiem_dinh$p.value < alpha, "Bac bo H0", "Chap nhan H0"),
              alternative = paste("mu1 - mu2 <", mu0)))
}

# ==================================================
# e) Viet ham test.geq.oneside
# ==================================================

test.geq.oneside <- function(x, y, mu0, alpha) {
  # Kiem dinh gia thuyet H0: µ = µ0 (µ <= µ0) va doi thuyet H1: µ > µ0
  # Voi µ = µ1 - µ2 (µ1, µ2 lan luot la duong kinh trung binh cua cac thanh thep do may so 1 va may so 2 san xuat ra)
  # x, y la hai vector du lieu chua duong kinh cua cac thanh thep do may so 1 va may so 2 san xuat ra
  # mu0 la gia tri gia thuyet cua µ
  # alpha la muc y nghia

  # Thuc hien kiem dinh t cho hai mau doc lap (gia su phuong sai bang nhau)
  kiem_dinh <- t.test(x, y, alternative = "greater", mu = mu0, var.equal = TRUE)

  # Tra ve ket qua
  return(list(statistic = kiem_dinh$statistic,
              p.value = kiem_dinh$p.value,
              result = ifelse(kiem_dinh$p.value < alpha, "Bac bo H0", "Chap nhan H0"),
              alternative = paste("mu1 - mu2 >", mu0)))
}

# Kiem tra hoat dong cua hai ham (vi du)
# Thu kiem dinh voi mu0 = 0
result_leq_example <- test.leq.oneside(may1, may2, mu0 = 0, alpha = 0.05)
print(result_leq_example)

result_geq_example <- test.geq.oneside(may1, may2, mu0 = 0, alpha = 0.05)
print(result_geq_example)






























