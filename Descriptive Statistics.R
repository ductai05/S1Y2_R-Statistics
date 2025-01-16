# Introduction about Statistics

# 0.1 Variable, qualitative variable, quantitative variable (discrete variable and
# continuous variable)
# 0.2 Population, sample, parameter, statistic

# 1. Descriptive statistics (Thong ke mo ta)
# 2. Inferential statistics (Thong ke suy luan)

# 1.1 Describing one-variable data using by Graphical methods 
# Bieu do tron, cot, Stem-Leaf, histogram, 

# 1.2 Describing one-variable data using Numerical methods

# 1.2.1 Measures of Center(Do do trung tam)
# Mean (trung binh), Median (trung vi), Mode

# 1.2.2 Measures of variation (su bien thien)
# Range (mien gia tri), Quartiles and Percentiles (mien phan vi)-(IQR, boxplot), 
# Variance (phuong sai), Standard Deviation (do lech chuan), 
# Coefficient of variation (he so bien thien)

# ------------------------------------------------------------------------------

# 0.1 
# Qualitative (Categorical) Variable Example:
colors <- c("Red", "Blue", "Green", "Red", "Yellow")
print(colors)
class(colors) # Output: "character"

# Quantitative Variable Examples:

# Discrete Variable Example:
num_children <- c(2, 0, 3, 1, 2, 4)
print(num_children)
class(num_children) # Output: "numeric"

# Continuous Variable Example:
heights <- c(175.5, 168.2, 180.0, 172.8, 165.5)
print(heights)
class(heights) # Output: "numeric"

# 0.2
# --- Population, Sample, Parameter, Statistic ---
# Let's imagine a population of all students in a university.
# Population: All students in a university (e.g., 10,000 students)
# Parameter: The average height of ALL students in the university (e.g., 170 cm)
#            We typically don't know the true population parameter.
# Sample: A subset of students selected from the university (e.g., 100 students)

set.seed(123)
sample_indices <- sample(1:10000, 100) 

sample_heights <- rnorm(100, mean = 170, sd = 10)
sample_mean_height <- mean(sample_heights)
print(paste("Sample Mean Height:", sample_mean_height))


# --- 1. Descriptive Statistics ---

# --- 1.1 Graphical Methods ---

# Example Data (One-variable - Quantitative)
data <- c(25, 30, 32, 35, 38, 40, 40, 42, 45, 48, 50, 50, 50, 52, 55, 58, 60)

# Pie Chart (for categorical data, not ideal for this quantitative data)
# Let's create some categorical data for a pie chart example:
fruit_counts <- c(Apples = 5, Bananas = 8, Oranges = 3)
pie(fruit_counts, main = "Fruit Distribution")

# Bar Chart (for categorical data)
barplot(fruit_counts, main = "Fruit Distribution", xlab = "Fruit", ylab = "Count")

# Bar chart for numerical data (using 'table' to count frequencies)
barplot(table(data), main = "Bar Chart of Data", xlab = "Value", ylab = "Frequency")

# Stem-and-Leaf Plot
stem(data)

# Histogram
hist(data, main = "Histogram of Data", xlab = "Value", breaks = 5) # 'breaks' controls the number of bins

# --- 1.2 Numerical Methods ---

# --- 1.2.1 Measures of Center ---

# Mean
mean_data <- mean(data)
print(paste("Mean:", mean_data))

# Median
median_data <- median(data)
print(paste("Median:", median_data))

# Mode (no built-in function, but we can create one)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_data <- getmode(data)
print(paste("Mode:", mode_data))

# --- 1.2.2 Measures of Variation ---

# Range
range_data <- range(data) # Gives min and max
range_diff <- diff(range_data) # Calculates the range (max - min)
print(paste("Range:", range_diff, ": [", min(data), ":", max(data), "]"))

# Quartiles and Percentiles
quartiles_data <- quantile(data)
print("Quartiles:")
print(quartiles_data)

# Interquartile Range (IQR)
iqr_data <- IQR(data)
print(paste("IQR:", iqr_data))

# Boxplot
boxplot(data, main = "Boxplot of Data")

# Variance
variance_data <- var(data)
print(paste("Variance:", variance_data))

# Standard Deviation
sd_data <- sd(data)
print(paste("Standard Deviation:", sd_data))

# Coefficient of Variation
cv_data <- (sd_data / mean_data) * 100
print(paste("Coefficient of Variation (%):", cv_data))

# --- 2. Inferential Statistics (Brief Example) ---

# We'll use a simple t-test as an example of inferential statistics.
# Suppose we want to test if the mean of our 'data' is significantly different from 45.

# Null Hypothesis (H0): The true mean is 45.
# Alternative Hypothesis (H1): The true mean is not 45.

t_test_result <- t.test(data, mu = 45)
print(t_test_result)

# Interpretation:
# The p-value is less than 0.05 (assuming a significance level of 0.05), so we
# would reject the null hypothesis. This suggests that the mean of our data
# is significantly different from 45.

# --- End of Examples ---





