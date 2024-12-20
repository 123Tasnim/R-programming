install.packages("readr")

# Load required libraries
library(readr)       # To read CSV files
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization

# Load the dataset
data <- read.csv("C:/Users/user/Downloads/Midterm_Project_Dataset_section(A).csv")

# Display the first few rows of the dataset
print(head(data))

# Check for missing values
missing_values <- sum(is.na(data))
cat("Number of missing values:", missing_values, "\n")

# Data Visualization before handling missing values
# Plot histograms for continuous attributes before handling missing values
numeric_columns <- data %>% select(where(is.numeric))

# Loop through numeric columns and plot histograms
for (col in colnames(numeric_columns)) {
  p <- ggplot(data, aes_string(col)) +
    geom_histogram(bins = 30, fill = 'red', color = 'black', alpha = 0.7) +
    ggtitle(paste("Histogram of", col, "before handling missing values")) +
    theme_minimal()
  print(p)
}

# Plot boxplots for continuous attributes before handling missing values
for (col in colnames(numeric_columns)) {
  p <- ggplot(data, aes_string(x = "1", y = col)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col, "before handling missing values")) +
    theme_minimal()
  print(p)
}

# Handle missing values by removing rows with NA values
data_clean <- na.omit(data)

# Verify no missing values remain
missing_values_clean <- sum(is.na(data_clean))
cat("Number of missing values after cleaning:", missing_values_clean, "\n")

# Data Visualization after handling missing values
# Plot histograms for continuous attributes after handling missing values
numeric_columns_clean <- data_clean %>% select(where(is.numeric))

# Loop through numeric columns and plot histograms
for (col in colnames(numeric_columns_clean)) {
  p <- ggplot(data_clean, aes_string(col)) +
    geom_histogram(bins = 30, fill = 'green', color = 'black', alpha = 0.7) +
    ggtitle(paste("Histogram of", col, "after handling missing values")) +
    theme_minimal()
  print(p)
}

# Plot boxplots for continuous attributes after handling missing values
for (col in colnames(numeric_columns_clean)) {
  p <- ggplot(data_clean, aes_string(x = "1", y = col)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col, "after handling missing values")) +
    theme_minimal()
  print(p)
}