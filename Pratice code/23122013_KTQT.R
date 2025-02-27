# Dinh Duc Tai - 23122013
# TH XSTK - k=1

setwd("D:/23122013")

# C�u 4. H?i d?ng khoa h?c Tru?ng �H Khoa h?c T? nhi�n mu?n thay d?i d�nh gi� h?c l?c c?a sinh
# vi�n t? thang di?m 10 sang thang di?m 4. H� Khoa h?c quy?t d?nh kh?o s�t � ki?n c�c gi?g vi�n
# trong tru?ng tru?c khi ra quy?t d?nh. N?u nhu t? l? gi?ng vi�n d?ng � v?i s? thay d?i tr�n 60%
# th� vi?c thay d?i thang di?m s? du?c th?c hi?n. Kh?o s�t ng?u nhi�n 80 gi?ng vi�n trong tru?ng,
# g?i p l� t? l? gi?ng vi�n d?ng � v?i s? thay d?i.

# a/ H0: p = 0.6 v� H1: p > 0.6
# Ch�ng ta ch?n c?p gi? thuy?t n�y v� ch�ng ta mu?n ki?m d?nh xem c� d? b?ng ch?ng d? b�c b? gi? 
# thuy?t r?ng t? l? d?ng � kh�ng l?n hon 60% v� ch?p nh?n gi? thuy?t r?ng t? l? n�y th?c s? l?n 
# hon 60%. N�i c�ch kh�c, ch�ng ta c?n b?ng ch?ng ?ng h? vi?c t? l? d?ng � l?n hon 60% th� m?i 
# th?c hi?n thay d?i thang di?m.

# b/ Kiem dinh bang R:

load("data04.rda")

# Ki?m tra d? li?u
head(survey)

# T�nh t? l? gi?ng vi�n d?ng �
proportion_agree <- mean(survey)
print(paste("Ty le dong y:", proportion_agree))

# S? lu?ng gi?ng vi�n kh?o s�t
n <- length(survey)
p0 <- 0.6
# T�nh to�n th?ng k� ki?m d?nh (z-statistic)
z <- (proportion_agree - p0) / sqrt(p0 * (1 - p0) / n)

# T�nh p-value (ki?m d?nh m?t ph�a, greater)
p_value <- pnorm(z, lower.tail = FALSE)

# In k?t qu?
print(paste("Z-statistic:", z))
print(paste("P-value:", p_value))

# Quy?t d?nh d?a tr�n m?c � nghia 5%
alpha <- 0.05

if (p_value < alpha) {
  print("Bac bo H0. Co du bang chung de cho rang ty le dong y lon hon 60%, nen thay doi thang diem")
} else {
  print("Khong du bang chung de bac bo H0. Khong nen thay doi thang diem")
}


# C�u 6b:

# �?c d? li?u t? file times.csv
data <- read.csv("times.csv")

# L?y c?t th?i gian t? h?c c?a tru?ng Khoa h?c T? nhi�n (KHTN)
khtn_times <- data$KHTN

# Lo?i b? c�c gi� tr? NA ho?c r?ng n?u c�
khtn_times <- khtn_times[!is.na(khtn_times) & khtn_times != ""]
khtn_times <- as.numeric(khtn_times)

# �?nh nghia h�m proptest.geq
proptest.geq <- function(f, n, p0, alpha) {
  # T�nh p_hat (t? l? m?u)
  p_hat <- f / n
  
  # T�nh sai s? chu?n
  se <- sqrt((p0 * (1 - p0)) / n)
  
  # T�nh z-score
  z <- (p_hat - p0) / se
  
  # T�nh p-value (cho ki?m d?nh m?t ph�a >)
  p_value <- 1 - pnorm(z)
  
  # In ra k?t qu?
  cat("T? l? m?u (p_hat): ", p_hat, "\n")
  cat("Z-score: ", z, "\n")
  cat("P-value: ", p_value, "\n")
  
  # So s�nh p-value v?i alpha v� dua ra k?t lu?n
  if (p_value < alpha) {
    cat("B�c b? gi? thuy?t H0\n")
  } else {
    cat("Kh�ng b�c b? gi? thuy?t H0\n")
  }
  
  # Tr? v? p-value (c� th? d�ng cho c�c ph�n t�ch kh�c)
  invisible(p_value)
}

# �p d?ng cho c�u a (H1: p > 0.5)
# T�nh s? sinh vi�n c� th?i gian t? h?c tr�n 5 gi?
f <- sum(khtn_times > 5)

# T�nh t?ng s? sinh vi�n
n <- length(khtn_times)

# �?t p0 v� alpha
p0 <- 0.5
alpha <- 0.05

# G?i h�m proptest.geq
cat("K?t qu? ki?m d?nh cho c�u a:\n")
proptest.geq(f, n, p0, alpha)


# C�u 8. 

# (1): X = 1.96, X ~ N(0,1), ki?m d?nh hai ph�a
p_value_1 <- 2 * pnorm(1.96, lower.tail = FALSE) # nh�n 2 cho 2 ph�a
print(paste("p-value c�u 1:", p_value_1))

# (4): X = 1.7, X ~ N(0,1), ki?m d?nh hai ph�a
p_value_4 <- 2 * pnorm(1.7, lower.tail = FALSE)
print(paste("p-value c�u 4:", p_value_4))

# (7): Y = 18, Y ~ B(50, 0.5), ki?m d?nh hai ph�a
p_value_7 <- 2 * pbinom(18, size = 50, prob = 0.5, lower.tail = TRUE)
if (p_value_7 > 1) {
  p_value_7 <- 2 * pbinom(18, size = 50, prob = 0.5, lower.tail = FALSE)
} else {
  p_value_7 <- 2 * pbinom(18, size = 50, prob = 0.5, lower.tail = TRUE)
  p_value_7<- min(p_value_7, 2 - p_value_7)
}
print(paste("p-value c�u 7:", p_value_7))

# (1): p-value l� kho?ng 0.05, di?u n�y c� nghia l� c� kho?ng 5% kh? nang 
# ch�ng ta s? quan s�t m?t gi� tr? tuy?t d?i l?n hon 1.96 khi H0 d�ng.

# (4): p-value l� kho?ng 0.089, di?u n�y c� nghia l� c� kho?ng 8.9% kh? nang
# ch�ng ta s? quan s�t m?t gi� tr? tuy?t d?i l?n hon 1.7 khi H0 d�ng.

# (7): p-value l� kho?ng 0.065, di?u n�y c� nghia l� c� kho?ng 6.5% kh? nang 
# ch�ng ta s? quan s�t m?t s? quan s�t (18 ho?c �t hon, ho?c m?t s? quan s�t m� 
# �t c� kh? nang x?y ra hon) khi H0 d�ng.




