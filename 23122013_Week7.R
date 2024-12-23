# Dinh Duc Tai - 23122013
# 23TNT1A - TH XSTK - Week 7

#Bai 01

a <- rnorm(35, 10, 5)
alpha <- 0.05
epsilon <- qt(1 - alpha / 2, df = 34) * sd(a) / sqrt(35)
lower_bound <- mean(a) - epsilon
upper_bound <- mean(a) + epsilon

cat("Kho?ng tin c?y 95%:", "[", lower_bound, ",", upper_bound, "]\n")

#Bai 02
data = read.csv("data31.csv")
ci.mean = function(x, alpha) {
    trung_binh = mean(x)
    do_lech_chuan = sd(x)
    so_luong = length(x)
    dung_sai = qnorm(1 - alpha / 2) * do_lech_chuan / sqrt(so_luong);
    cat(trung_binh - dung_sai, trung_binh + dung_sai)
}

profit = data[,2]
ci.mean(profit, 1 - 0.95)

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
adj <- FALSE # Không bi?t d? l?ch chu?n t?ng th?
s <- 3      # Ð? l?ch chu?n m?u
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
adj <- FALSE # Không bi?t d? l?ch chu?n t?ng th?
s <- 4      # Ð? l?ch chu?n m?u
alpha <- 0.01
ktc.tb.mau(x, adj, s, alpha)

