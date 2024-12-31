# Dinh Duc Tai - 23122013

setwd("D:/23122013") # Thay d?i du?ng d?n n?u c?n

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
test.leq.oneside <- function(x, y, sigma1, sigma2, alpha) {
  n1 <- length(x)
  n2 <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  mu0 <- 0  # Gia thuyet H0: mu1 - mu2 = 0

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
test.geq.oneside <- function(x, y, sigma1, sigma2, alpha) {
  n1 <- length(x)
  n2 <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  mu0 <- 0  # Gia thuyet H0: mu1 - mu2 = 0

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

result_leq <- test.leq.oneside(machine1, machine2, sigma1, sigma2, alpha)
print(result_leq)

# ----------------------------------------------------
# e) Kiem dinh gia thuyet H0: µ1 - µ2 = 0, doi thuyet H1: µ1 - µ2 > 0
# ----------------------------------------------------

result_geq <- test.geq.oneside(machine1, machine2, sigma1, sigma2, alpha)
print(result_geq)