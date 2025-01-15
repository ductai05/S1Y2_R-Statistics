# Dinh Duc Tai - 23122013
# TH XSTK - Week 9

setwd("D:/23122013")
# Cau 8

# Du lieu
n1 <- 500  # So nguoi o TP.HCM
x1 <- 385  # So nguoi ung ho tang toc do o TP.HCM
n2 <- 400  # So nguoi o Ha Noi
x2 <- 267  # So nguoi ung ho tang toc do o Ha Noi

# Tinh ti le mau
p1_hat <- x1 / n1
p2_hat <- x2 / n2

# Tinh ti le chung
p_pooled <- (x1 + x2) / (n1 + n2)

# Tinh sai so chuan
se <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))

# Tinh thong ke kiem dinh z
z_stat <- (p1_hat - p2_hat) / se

# Tinh gia tri p (p-value)
p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
print(p_value)

# Muc y nghia alpha
alpha <- 0.05
print(alpha)

# Kiem dinh gia thuyet
if (p_value < alpha) {
  cat("Bac bo gia thuyet H0.\n")
  cat("Co su khac biet co y nghia thong ke ve ti le nguoi ung ho viec tang toc do tai hai thanh pho.\n")
} else {
  cat("Khong du bang chung de bac bo gia thuyet H0.\n")
  cat("Khong co su khac biet co y nghia thong ke ve ti le nguoi ung ho viec tang toc do tai hai thanh pho.\n")
}

# In gia tri p
cat("Gia tri p:", p_value, "\n")

# Cau 9 (a) va (b)

# Du lieu goc
n1 <- 200  # So tai xe o TP.HCM
x1 <- 165  # So tai xe mang dai an toan o TP.HCM
n2 <- 250  # So tai xe o Ha Noi
x2 <- 198  # So tai xe mang dai an toan o Ha Noi

# Tinh ti le mau
p1_hat <- x1 / n1
p2_hat <- x2 / n2

# Tinh ti le chung
p_pooled <- (x1 + x2) / (n1 + n2)

# Tinh sai so chuan
se <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))

# Tinh thong ke kiem dinh z
z_stat <- (p1_hat - p2_hat) / se

# Ham kiem dinh gia thuyet
kiem_dinh_ty_le <- function(z_stat, alpha) {
  # Tinh gia tri p
  p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)

  # Ket luan
  if (p_value < alpha) {
    cat("Bac bo gia thuyet H0.\n")
    cat("Co su khac biet co y nghia thong ke giua ti le su dung dai an toan o TP.HCM va Ha Noi.\n")
  } else {
    cat("Khong du bang chung de bac bo gia thuyet H0.\n")
    cat("Khong co su khac biet co y nghia thong ke giua ti le su dung dai an toan o TP.HCM va Ha Noi.\n")
  }
  cat("Gia tri p:", p_value, "\n")
}


# Kiem dinh voi muc y nghia 0.05
cat("\nKiem dinh voi muc y nghia 0.05:\n")
kiem_dinh_ty_le(z_stat, 0.05)

# Kiem dinh voi muc y nghia 0.1
cat("\nKiem dinh voi muc y nghia 0.1:\n")
kiem_dinh_ty_le(z_stat, 0.1)


# Cau 9 (c)
# Du lieu nhan doi
n1_doubled <- 400  # So tai xe o TP.HCM (nhan doi)
x1_doubled <- 330  # So tai xe mang dai an toan o TP.HCM (nhan doi)
n2_doubled <- 500  # So tai xe o Ha Noi (nhan doi)
x2_doubled <- 396  # So tai xe mang dai an toan o Ha Noi (nhan doi)

# Tinh ti le mau moi
p1_hat_doubled <- x1_doubled / n1_doubled
p2_hat_doubled <- x2_doubled / n2_doubled

# Tinh ti le chung moi
p_pooled_doubled <- (x1_doubled + x2_doubled) / (n1_doubled + n2_doubled)

# Tinh sai so chuan moi
se_doubled <- sqrt(p_pooled_doubled * (1 - p_pooled_doubled) * (1/n1_doubled + 1/n2_doubled))

# Tinh thong ke kiem dinh z moi
z_stat_doubled <- (p1_hat_doubled - p2_hat_doubled) / se_doubled

# Kiem dinh voi muc y nghia 0.05 (du lieu nhan doi)
cat("\nKiem dinh voi muc y nghia 0.05 (du lieu nhan doi):\n")
kiem_dinh_ty_le(z_stat_doubled, 0.05)

# Kiem dinh voi muc y nghia 0.1 (du lieu nhan doi)
cat("\nKiem dinh voi muc y nghia 0.1 (du lieu nhan doi):\n")
kiem_dinh_ty_le(z_stat_doubled, 0.1)

# Nhan xet
cat("\nNhan xet:\n")
cat("Khi kich thuoc mau tang len ma ti le khong doi, gia tri z-stat thay doi (trong truong hop nay thi tang gia tri tuyet doi), va do do gia tri p cung giam di.\n")
cat("Ket qua la, ta de dang bac bo gia thuyet H0 hon, nghia la co nhieu bang chung hon cho thay co su khac biet co y nghia thong ke giua hai nhom.\n")
cat("Dieu nay cho thay rang viec tang kich thuoc mau giup tang do tin cay cua ket qua kiem dinh.\n")



# Cau 10

# Doc du lieu tu file Profit.csv
data <- read.csv("Profit-th05.csv")

# a) Tinh khoang tin cay 95% cho su sai khac ve doanh so trung binh
# Lay du lieu doanh so cua hai chi nhanh
dist1 <- data$Dist.1
dist3 <- data$Dist.3

# Thuc hien kiem dinh t de so sanh trung binh cua hai nhom
t_test_result <- t.test(dist1, dist3, conf.level = 0.95, paired=FALSE)

# In ket qua kiem dinh t, bao gom khoang tin cay
print("a) Khoang tin cay 95% cho su sai khac ve doanh so trung binh:")
print(t_test_result$conf.int)

# b) Kiem dinh gia thuyet ve ty le ngay co doanh so cao
# Xac dinh so ngay co doanh so cao (tren 600 trieu) cho moi chi nhanh
count_dist1_high <- sum(dist1 > 600)
count_dist3_high <- sum(dist3 > 600)

# Tong so ngay quan sat
n_days <- length(dist1)

# Thuc hien kiem dinh ty le
prop_test_result <- prop.test(x = c(count_dist1_high, count_dist3_high),
                              n = c(n_days, n_days),
                              alternative = "greater", # Kiem tra xem ty le o chi nhanh 1 co lon hon chi nhanh 3 khong
                              correct = FALSE # Khong ap dung hieu chinh Yates vi mau du lon
                              )

# In ket qua kiem dinh ty le, bao gom p-value
print("b) Ket qua kiem dinh gia thuyet ve ty le ngay co doanh so cao:")
print(paste("P-value =", prop_test_result$p.value))

# c) Dinh nghia ham prop.test.leq
prop.test.leq <- function(x, y, alpha) {
    # Tinh so ngay co doanh so tren 600 (doanh so cao) cho moi chi nhanh
    count_x_high <- sum(x > 600)
    count_y_high <- sum(y > 600)

    # Tinh tong so ngay quan sat
    n_x <- length(x)
    n_y <- length(y)

    # Thuc hien kiem dinh ty le mot phia (less)
    prop_test_result <- prop.test(x = c(count_x_high, count_y_high),
                                  n = c(n_x, n_y),
                                  alternative = "less", # Kiem tra xem ty le o x co nho hon y khong
                                  correct = FALSE) # Khong ap dung hieu chinh Yates
                                  
    # Lay P-gia tri
    p_value <- prop_test_result$p.value
    
    # In P-gia tri
    print(paste("P-value =", p_value))
    
    # Quyet dinh chap nhan hoac bac bo gia thuyet dua tren muc y nghia alpha
    if (p_value < alpha) {
        print("Bac bo gia thuyet H0: px = py.")
    } else {
        print("Chap nhan gia thuyet H0: px = py.")
    }
}

# Su dung ham da dinh nghia de kiem dinh gia thuyet
print("c) Ket qua kiem dinh bang ham prop.test.leq:")
prop.test.leq(dist1, dist3, alpha = 0.05)


# Cau 11:

# Doc du lieu tu file Inf.Sal.csv
data <- read.csv("Inf.Sal.csv")

# a) Tinh khoang tin cay 95% cho su sai khac ve muc luong trung binh
# Lay du lieu thu nhap cua hai thanh pho
hcm_salary <- data$HCM
hanoi_salary <- data$HaNoi

# Loai bo cac dong co gia tri NA (missing)
hcm_salary <- na.omit(hcm_salary)
hanoi_salary <- na.omit(hanoi_salary)

# Thuc hien kiem dinh t de so sanh trung binh luong cua hai thanh pho
t_test_result <- t.test(hcm_salary, hanoi_salary, conf.level = 0.95, paired=FALSE)

# In ket qua kiem dinh t, bao gom khoang tin cay
print("a) Khoang tin cay 95% cho su sai khac ve muc luong trung binh:")
print(t_test_result$conf.int)

# b) Kiem dinh gia thuyet ve ty le nguoi co thu nhap cao
# Xac dinh so nguoi co thu nhap cao (tren 11.5 trieu) cho moi thanh pho
count_hcm_high <- sum(hcm_salary > 11.5)
count_hanoi_high <- sum(hanoi_salary > 11.5)

# Tong so nguoi quan sat o moi thanh pho
n_hcm <- length(hcm_salary)
n_hanoi <- length(hanoi_salary)

# Thuc hien kiem dinh ty le
prop_test_result <- prop.test(x = c(count_hcm_high, count_hanoi_high),
                              n = c(n_hcm, n_hanoi),
                              alternative = "greater", # Kiem tra xem ty le o HCM co lon hon Ha Noi khong
                              correct = FALSE # Khong ap dung hieu chinh Yates vi mau du lon
                              )

# In ket qua kiem dinh ty le, bao gom p-value
print("b) Ket qua kiem dinh gia thuyet ve ty le nguoi co thu nhap cao:")
print(paste("P-value =", prop_test_result$p.value))

# c) Dinh nghia ham prop.test.geq
prop.test.geq <- function(x, y, alpha) {
    # Tinh so nguoi co thu nhap tren 11.5 trieu (thu nhap cao) cho moi thanh pho
    count_x_high <- sum(x > 11.5)
    count_y_high <- sum(y > 11.5)

    # Tinh tong so nguoi quan sat
    n_x <- length(x)
    n_y <- length(y)

    # Thuc hien kiem dinh ty le mot phia (greater)
    prop_test_result <- prop.test(x = c(count_x_high, count_y_high),
                                  n = c(n_x, n_y),
                                  alternative = "greater", # Kiem tra xem ty le o x co lon hon y khong
                                  correct = FALSE) # Khong ap dung hieu chinh Yates

    # Lay P-gia tri
    p_value <- prop_test_result$p.value

    # In P-gia tri
    print(paste("P-value =", p_value))

    # Quyet dinh chap nhan hoac bac bo gia thuyet dua tren muc y nghia alpha
    if (p_value < alpha) {
        print("Bac bo gia thuyet H0: px = py.")
    } else {
        print("Chap nhan gia thuyet H0: px = py.")
    }
}

# Su dung ham da dinh nghia de kiem dinh gia thuyet
print("c) Ket qua kiem dinh bang ham prop.test.geq:")
prop.test.geq(hcm_salary, hanoi_salary, alpha = 0.025)

# d) Tim khoang tin cay 95% cho hieu hai ty le
# Tinh ti le thu nhap cao o moi thanh pho
p_hcm <- count_hcm_high / n_hcm
p_hanoi <- count_hanoi_high / n_hanoi

# Tinh sai so tieu chuan
se_diff <- sqrt((p_hcm * (1 - p_hcm) / n_hcm) + (p_hanoi * (1 - p_hanoi) / n_hanoi))

# Tinh gia tri z-score cho khoang tin cay 95% (1.96)
z <- qnorm(0.975)

# Tinh khoang tin cay
margin_of_error <- z * se_diff
confidence_interval_diff <- c((p_hcm - p_hanoi) - margin_of_error, (p_hcm - p_hanoi) + margin_of_error)

# In khoang tin cay
print("d) Khoang tin cay 95% cho hieu hai ty le:")
print(confidence_interval_diff)
