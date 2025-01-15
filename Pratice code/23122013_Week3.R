# Dinh Duc Tai - 23122013
# TH XSTK 23TNT1A - Week3

setwd("D:/23122013")

# VD1 pp nhi thuc
hamxacsuat = function(n, p, k) {
  p_k = c() 
  for(i in k) {
    p_k[i+1] = choose(n, i) * p^i * (1-p)^(n-i)
  }
  return (p_k)
}

n = 10  # So lan thu
k = 0:n  # Cac gia tri cua k tu 0 toi n
p = 0.3  # Xac suat p

hamxacsuat(n, p, k)
sum(hamxacsuat(n, p, k))
plot(k, hamxacsuat(n, p, k), type = "h", ylab = "P(X = x)", col = "blue", lwd = 5)

# VD2 
f = function(x, mu=0, sigma=1){
1/sqrt(2*pi*sigma^2) * exp(-(x-mu)^2/(2*sigma^2))
}
kq = integrate(function(x) f(x,0,1),lower=-Inf,upper=Inf)$value
#curve(f(x,0,1),from=-5,to=5, ylab = "fX(x)")
if (abs(1 - kq) < 10^-5){	                  nmn        XZmnn
	cat("Ham so la ham mat do")
} else {
	cat("Ham so khong la ham mat do")
}
#---------------------------------------------------------------------
# Bai 1

# 1a
f=function(p){.07*p**(-0.93)}
integrate(f,lower=0,upper=.2)

#1b
kq = integrate(f,lower=0,upper=1)$value
if (abs(1 - kq) < 10^-5){
	cat("Ham so la ham mat do")
} else {
	cat("Ham so khong la ham mat do")
}

# Bai 2

x=sample(1:5,100,TRUE,c(0.1,0.2,0.4,0.2,0.1))
table(x)/100

#----------------------------------------------------------------------
# De 1 : Bai 3.9 BTC3 (XSTK)

#a/ Ham f(x)
f=function(x){exp(-x/2)}
kq = integrate(f,lower = 0, upper=Inf)$value
a = 1/ kq
a

#b/ Ham phan phoi xac suat F(x)
F = function(x){
	f=function(x){1/2*exp(-x/2)}
	y = integrate(f,lower = 0, upper=x)$value
	return (y)
}
F(Inf)

#c/ Ky vong E(X)

f <- function(x) { (1/2) * exp(-x/2) }
expectation <- integrate(function(x) x * f(x), 0, Inf)$value
expectation 

#d/ Phuong sai Var(X)

f <- function(x) {
  1/2 * exp(-x/2)
}

E_X <- integrate(function(x) x * f(x), 0, Inf)$value

E_X2 <- integrate(function(x) x^2 * f(x), 0, Inf)$value

var_X <- E_X2 - E_X^2

var_X






















