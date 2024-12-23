# Dinh Duc Tai - 23122013
# TH XSTK 23TNT1A

# Bai 1:
x<-c(4,2,6)
y<-c(1,0,-1)

length(x)
sum(x)
sum(x^2)
x+y
x*y
x-2
x^2

# Bai 2:

7:11
seq(2,9)
seq(4,10,by=2)
seq(3,30,length=10)
seq(6,-4,by=-2)

# Bai 3:

rep(2,4)
rep(c(1,2),4)
rep(c(1,2),c(4,4))
rep(1:4,4)
rep(1:4,rep(3,4))

# Bai 4:

rep(6,6)
rep(c(5,8),4)
rep(c(5,8),c(4,4))

# Bai 5:

x<- c(5,9,2,3,4,6,7,0,8,12,2,9)
x[2]
x[2:4]
x[c(2,3,6)]
x[c(1:5,10:12)]
x[-(10:12)]

# Bai 6:

y <- c(33, 44, 29, 16, 25, 45, 33, 19, 54, 22, 21, 49, 11, 24, 56)

sales_data <- matrix(y, nrow = 5, ncol = 3, byrow = TRUE)
colnames(sales_data) <- c("Cua hang 1", "Cua hang 2", "Cua hang 3")
rownames(sales_data) <- c("T2", "T3", "T4", "T5", "T6")

sales_df <- as.data.frame(sales_data)
print(sales_df)

daily_summary <- apply(sales_df, 1, summary)
print(daily_summary)

store_summary <- apply(sales_df, 2, summary)
print(store_summary)


# Bai 7, 8:

x = matrix(c(3,-1,2,1),nrow = 2, ncol = 2)
y = matrix(c(1,0,4,1,0,-1),nrow = 2, ncol = 3)

2*x
x*x
x%*%x
x%*%y
t(y)
solve(x)

x[1,]
x[2,]
x[,2]
y[1,2]
y[,2:3]

# Bai 9:

data(quakes)
summary(quakes[, c("depth", "mag")])

data(mtcars)
avg_weight <- mean(mtcars$wt)
avg_mpg <- mean(mtcars$mpg)
avg_weight
avg_mpg


