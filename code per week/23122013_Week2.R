# Dinh Duc Tai - 23122013
# TH XSTK 23TNT1A - Week2

setwd("D:/23122013")

# Bai 1:
X <- c(2, 4, 6, 8, 10)

cumulative_sum <- function(vector) {
  result <- numeric(length(vector)) 
  result[1] <- vector[1] 
  
  for(i in 2:length(vector)) {
    result[i] <- result[i-1] + vector[i]  
  }
  
  return(result)
}

result <- cumulative_sum(X)
print("Vector X:")
print(X)
print("Tong tich luy:")
print(result)

# Bai 2:
sphere_volume <- function() {
  radius <- 3:20
  
  volume <- (4/3) * pi * radius^3
  
  df <- data.frame(radius = radius,
                  volume = volume)
  
  return(df)
}

result <- sphere_volume()
print(result)

# Bai 3
data <- read.csv("data01.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

print(data)

data$Index <- numeric(nrow(data))

for(i in 1:nrow(data)) {
  if(data$Age[i] <= 60) {
    data$Index[i] <- 0
  } else if(data$Age[i] > 60 & data$Age[i] <= 70) {
    data$Index[i] <- 1
  } else if(data$Age[i] > 70 & data$Age[i] <= 80) {
    data$Index[i] <- 2
  } else {
    data$Index[i] <- 3
  }
}

print(data)

# Bai 4

data <- read.csv("data11.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

print(data)

calculate_statistics <- function(df) {
  midpoints <- (df$a + df$b) / 2 
  total_counts <- df$n 
  
  total_n <- sum(total_counts)
  
  min_height <- min(df$a)
  max_height <- max(df$b)
  
  mean_height <- sum(midpoints * total_counts) / total_n
  variance_height <- sum(((midpoints - mean_height)^2) * total_counts) / (total_n - 1) 
  
  return(list(
    Min_Height = min_height,
    Max_Height = max_height,
    Mean_Height = mean_height,
    Variance_Height = variance_height
  ))
}

statistics <- calculate_statistics(data)
print(statistics)

# Bai 5

phanvi <- function(X, P) {
  sorted_X <- sort(X)
  n <- length(sorted_X)
  i <- (P / 100) * n
  
  if (i %% 1 == 0) {
    i <- as.integer(i)
    result <- (sorted_X[i] + sorted_X[i + 1]) / 2
  } else {
    i <- round(i)
    result <- sorted_X[i]
  }
  X = sorted_X
  return(result)
}

X <- c(9, 8, 7, 6, 5, 4, 3, 2, 1)
P <- 78

X <- sort(X)
result <- phanvi(X, P)
print(result)
print(X)