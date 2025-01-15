# Dinh Duc Tai - 23122013
# TH XSTK - k=1

setwd("D:/23122013")

# Câu 4. H?i d?ng khoa h?c Tru?ng ÐH Khoa h?c T? nhiên mu?n thay d?i dánh giá h?c l?c c?a sinh
# viên t? thang di?m 10 sang thang di?m 4. HÐ Khoa h?c quy?t d?nh kh?o sát ý ki?n các gi?g viên
# trong tru?ng tru?c khi ra quy?t d?nh. N?u nhu t? l? gi?ng viên d?ng ý v?i s? thay d?i trên 60%
# thì vi?c thay d?i thang di?m s? du?c th?c hi?n. Kh?o sát ng?u nhiên 80 gi?ng viên trong tru?ng,
# g?i p là t? l? gi?ng viên d?ng ý v?i s? thay d?i.

# a/ H0: p = 0.6 và H1: p > 0.6
# Chúng ta ch?n c?p gi? thuy?t này vì chúng ta mu?n ki?m d?nh xem có d? b?ng ch?ng d? bác b? gi? 
# thuy?t r?ng t? l? d?ng ý không l?n hon 60% và ch?p nh?n gi? thuy?t r?ng t? l? này th?c s? l?n 
# hon 60%. Nói cách khác, chúng ta c?n b?ng ch?ng ?ng h? vi?c t? l? d?ng ý l?n hon 60% thì m?i 
# th?c hi?n thay d?i thang di?m.

# b/ Kiem dinh bang R:

load("data04.rda")

# Ki?m tra d? li?u
head(survey)

# Tính t? l? gi?ng viên d?ng ý
proportion_agree <- mean(survey)
print(paste("Ty le dong y:", proportion_agree))

# S? lu?ng gi?ng viên kh?o sát
n <- length(survey)
p0 <- 0.6
# Tính toán th?ng kê ki?m d?nh (z-statistic)
z <- (proportion_agree - p0) / sqrt(p0 * (1 - p0) / n)

# Tính p-value (ki?m d?nh m?t phía, greater)
p_value <- pnorm(z, lower.tail = FALSE)

# In k?t qu?
print(paste("Z-statistic:", z))
print(paste("P-value:", p_value))

# Quy?t d?nh d?a trên m?c ý nghia 5%
alpha <- 0.05

if (p_value < alpha) {
  print("Bac bo H0. Co du bang chung de cho rang ty le dong y lon hon 60%, nen thay doi thang diem")
} else {
  print("Khong du bang chung de bac bo H0. Khong nen thay doi thang diem")
}


# Câu 6b:

# Ð?c d? li?u t? file times.csv
data <- read.csv("times.csv")

# L?y c?t th?i gian t? h?c c?a tru?ng Khoa h?c T? nhiên (KHTN)
khtn_times <- data$KHTN

# Lo?i b? các giá tr? NA ho?c r?ng n?u có
khtn_times <- khtn_times[!is.na(khtn_times) & khtn_times != ""]
khtn_times <- as.numeric(khtn_times)

# Ð?nh nghia hàm proptest.geq
proptest.geq <- function(f, n, p0, alpha) {
  # Tính p_hat (t? l? m?u)
  p_hat <- f / n
  
  # Tính sai s? chu?n
  se <- sqrt((p0 * (1 - p0)) / n)
  
  # Tính z-score
  z <- (p_hat - p0) / se
  
  # Tính p-value (cho ki?m d?nh m?t phía >)
  p_value <- 1 - pnorm(z)
  
  # In ra k?t qu?
  cat("T? l? m?u (p_hat): ", p_hat, "\n")
  cat("Z-score: ", z, "\n")
  cat("P-value: ", p_value, "\n")
  
  # So sánh p-value v?i alpha và dua ra k?t lu?n
  if (p_value < alpha) {
    cat("Bác b? gi? thuy?t H0\n")
  } else {
    cat("Không bác b? gi? thuy?t H0\n")
  }
  
  # Tr? v? p-value (có th? dùng cho các phân tích khác)
  invisible(p_value)
}

# Áp d?ng cho câu a (H1: p > 0.5)
# Tính s? sinh viên có th?i gian t? h?c trên 5 gi?
f <- sum(khtn_times > 5)

# Tính t?ng s? sinh viên
n <- length(khtn_times)

# Ð?t p0 và alpha
p0 <- 0.5
alpha <- 0.05

# G?i hàm proptest.geq
cat("K?t qu? ki?m d?nh cho câu a:\n")
proptest.geq(f, n, p0, alpha)


# Câu 8. 

# (1): X = 1.96, X ~ N(0,1), ki?m d?nh hai phía
p_value_1 <- 2 * pnorm(1.96, lower.tail = FALSE) # nhân 2 cho 2 phía
print(paste("p-value câu 1:", p_value_1))

# (4): X = 1.7, X ~ N(0,1), ki?m d?nh hai phía
p_value_4 <- 2 * pnorm(1.7, lower.tail = FALSE)
print(paste("p-value câu 4:", p_value_4))

# (7): Y = 18, Y ~ B(50, 0.5), ki?m d?nh hai phía
p_value_7 <- 2 * pbinom(18, size = 50, prob = 0.5, lower.tail = TRUE)
if (p_value_7 > 1) {
  p_value_7 <- 2 * pbinom(18, size = 50, prob = 0.5, lower.tail = FALSE)
} else {
  p_value_7 <- 2 * pbinom(18, size = 50, prob = 0.5, lower.tail = TRUE)
  p_value_7<- min(p_value_7, 2 - p_value_7)
}
print(paste("p-value câu 7:", p_value_7))

# (1): p-value là kho?ng 0.05, di?u này có nghia là có kho?ng 5% kh? nang 
# chúng ta s? quan sát m?t giá tr? tuy?t d?i l?n hon 1.96 khi H0 dúng.

# (4): p-value là kho?ng 0.089, di?u này có nghia là có kho?ng 8.9% kh? nang
# chúng ta s? quan sát m?t giá tr? tuy?t d?i l?n hon 1.7 khi H0 dúng.

# (7): p-value là kho?ng 0.065, di?u này có nghia là có kho?ng 6.5% kh? nang 
# chúng ta s? quan sát m?t s? quan sát (18 ho?c ít hon, ho?c m?t s? quan sát mà 
# ít có kh? nang x?y ra hon) khi H0 dúng.




