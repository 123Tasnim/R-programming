# Load necessary libraries
library(readr)   # For reading CSV files
library(ggplot2) # For creating plots
library(fmsb)    # For creating radar charts
library(GGally)  # For scatterplot matrices
library(dplyr)
library(e1071)

# Load the dataset
dataPath <-"C:/Users/user/Desktop/DataScience/drug200.csv"
df <- read.csv(dataPath)


## ------------------------- step-1 ------------------------- (Check Missing Value)

# 1. Using dim() to get the dimensions
dataset_dimensions <- dim(df)
print(paste("Total number of rows (data points):", dataset_dimensions[1]))
print(paste("Total number of columns (attributes):", dataset_dimensions[2]))

# Using str() to see the structure of the dataset
print("Structure of the dataset:")
str(df)

# Count the number of numerical attributes
num_numerical <- sum(sapply(df, is.numeric))
print(paste("Total number of numerical attributes:", num_numerical))

# Count the number of categorical attributes
# Assuming categorical attributes are factors or character types
num_categorical <- sum(sapply(df, function(x) is.factor(x) || is.character(x)))
print(paste("Total number of categorical attributes:", num_categorical))




# Count missing values in each column
missing_values <- sapply(df, function(x) sum(is.na(x)))

# Print the count of missing values for each column
print(missing_values)

# Sum all missing values in the dataset
total_missing_values <- sum(is.na(df))

# Print the total number of missing values
print(paste("Total number of missing values in the dataset : ", total_missing_values))


#----------------- Multivariate Exploration + Bar Graph + Box Plot -----------------------

## ------------------------- Step 2 ------------------------- (Pearson Correlation & Scatter Plot)

# Select two numerical variables to analyze
# Replace 'Candidate_Age' and 'Adrenaline_Level' with your chosen variables
x_var <- df$Age  # Example column; adjust based on the actual dataset
y_var <- df$Na_to_K  # Example column; adjust based on the actual dataset

# Calculate the Pearson correlation coefficient
correlation_value <- cor(x_var, y_var, method = "pearson", use = "complete.obs")
print(paste("Pearson correlation coefficient (r):", round(correlation_value, 2)))


# Create a scatterplot for the two numerical attributes
ggplot(df, aes(x = Age, y = Na_to_K)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +  # Add a linear regression line with confidence interval
  labs(
    x = "Age",        # X-axis label
    y = "Na_to_K",     # Y-axis label
    title = paste("Scatter Plot of Age vs Na_to_K (r =", round(correlation_value, 2), ")")  # Title with correlation value
  ) +
  theme_minimal()  # Apply a minimal theme to the plot


## ------------------------- Step 3 ------------------------- (Apply ANOVA & Box Plot)

ggplot(df, aes(y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age")

ggplot(df, aes(y = Na_to_K)) +
  geom_boxplot() +
  labs(title = "Boxplot of Na_to_K")


# Apply ANOVA on a numerical variable against a categorical variable
# Replace 'Na_to_K' with your numerical variable and 'Drug' with your categorical variable
# anova_result <- aov(df$Na_to_K ~ df$Drug)
# print("ANOVA Results:")
# print(summary(anova_result))

# Box plot to visualize the distribution of Na_to_K by Drug
ggplot(df, aes(x = Drug, y = Na_to_K, fill = Drug)) +
  geom_boxplot() +
  labs(
    x = "Drug",  # X-axis label
    y = "Na_to_K",  # Y-axis label
    title = "Box Plot of Na_to_K by Drug"
  ) +
  theme_minimal()

# Box plot to visualize the distribution of Na_to_K by Gender
ggplot(df, aes(x = Sex, y = Na_to_K, fill = Sex)) +
  geom_boxplot() +
  labs(
    x = "Sex",  # X-axis label
    y = "Na_to_K",  # Y-axis label
    title = "Box Plot of Na_to_K by Sex"
  ) +
  theme_minimal()

# Box plot to visualize the distribution of Na_to_K by BP
ggplot(df, aes(x = BP, y = Na_to_K, fill = BP)) +
  geom_boxplot() +
  labs(
    x = "BP",  # X-axis label
    y = "Na_to_K",  # Y-axis label
    title = "Box Plot of Na_to_K by BP"
  ) +
  theme_minimal()

# Box plot to visualize the distribution of Na_to_K by Cholesterol
ggplot(df, aes(x = Cholesterol, y = Na_to_K, fill = Cholesterol)) +
  geom_boxplot() +
  labs(
    x = "Cholesterol",  # X-axis label
    y = "Na_to_K",  # Y-axis label
    title = "Box Plot of Na_to_K by Cholesterol"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------

# Box plot to visualize the distribution of Age by Drug
ggplot(df, aes(x = Drug, y = Age, fill = Drug)) +
  geom_boxplot() +
  labs(
    x = "Drug",  # X-axis label
    y = "Age",  # Y-axis label
    title = "Box Plot of Age by Drug"
  ) +
  theme_minimal()

# Box plot to visualize the distribution of Age by Gender
ggplot(df, aes(x = Sex, y = Age, fill = Sex)) +
  geom_boxplot() +
  labs(
    x = "Sex",  # X-axis label
    y = "Age",  # Y-axis label
    title = "Box Plot of Age by Sex"
  ) +
  theme_minimal()

# Box plot to visualize the distribution of Age by BP
ggplot(df, aes(x = BP, y = Age, fill = BP)) +
  geom_boxplot() +
  labs(
    x = "BP",  # X-axis label
    y = "Age",  # Y-axis label
    title = "Box Plot of Age by BP"
  ) +
  theme_minimal()

# Box plot to visualize the distribution of Age by Cholesterol
ggplot(df, aes(x = Cholesterol, y = Age, fill = Cholesterol)) +
  geom_boxplot() +
  labs(
    x = "Cholesterol",  # X-axis label
    y = "Age",  # Y-axis label
    title = "Box Plot of Age by Cholesterol"
  ) +
  theme_minimal()


## ------------------------- step-4 ------------------------- (Apply Chi-Squared & Bar plot)

# Apply Chi-Squared test on two categorical variables
# Replace 'Drug' and 'Sex' with your chosen categorical variables
# chisq_result <- chisq.test(table(df$Drug, df$Sex))
# print("Chi-Squared Test Results:")
# print(chisq_result)

# Bar plot to visualize the counts of combinations of Drug and Sex
ggplot(df, aes(x = Drug, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Drug",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Drug by Sex"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of Drug and BP
ggplot(df, aes(x = Drug, fill = BP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Drug",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Drug by BP"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of Drug and Cholesterol
ggplot(df, aes(x = Drug, fill = Cholesterol)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Drug",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Drug by Cholesterol"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

#--------------------------------------------

# Bar plot to visualize the counts of combinations of Sex and Drug
ggplot(df, aes(x = Sex, fill = Drug)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Sex",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Sex by Drug"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of Sex and BP
ggplot(df, aes(x = Sex, fill = BP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Sex",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Sex by BP"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of Sex and Cholesterol
ggplot(df, aes(x = Sex, fill = Cholesterol)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Sex",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Sex by Cholesterol"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

#--------------------------------------------

# Bar plot to visualize the counts of combinations of BP and Drug
ggplot(df, aes(x = BP, fill = Drug)) +
  geom_bar(position = "dodge") +
  labs(
    x = "BP",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of BP by Drug"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of BP and Sex
ggplot(df, aes(x = BP, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(
    x = "BP",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of BP by Sex"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of BP and Cholesterol
ggplot(df, aes(x = BP, fill = Cholesterol)) +
  geom_bar(position = "dodge") +
  labs(
    x = "BP",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of BP by Cholesterol"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

#--------------------------------------------

# Bar plot to visualize the counts of combinations of Cholesterol and Drug
ggplot(df, aes(x = Cholesterol, fill = Drug)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Cholesterol",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Cholesterol by Drug"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of Cholesterol and Sex
ggplot(df, aes(x = Cholesterol, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Cholesterol",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Cholesterol by Sex"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

# Bar plot to visualize the counts of combinations of Cholesterol and BP
ggplot(df, aes(x = Cholesterol, fill = BP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Cholesterol",  # X-axis label
    y = "Count",  # Y-axis label
    title = "Bar Plot of Cholesterol by BP"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot


#----------------------- Univariate Visualization (Histogram) -------------------------

# Reshape data for plotting multiple histograms
df_long <- reshape2::melt(df, measure.vars = c("Age"))

# Create histograms for both numerical attributes
ggplot(df_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5, alpha = 0.6, color = "black") +  # Adjust binwidth as needed
  labs(
    x = "Value",  # X-axis label
    y = "Frequency",  # Y-axis label
    title = "Histograms of Age"
  ) +
  facet_wrap(~variable, scales = "free_x") +  # Separate histograms by attribute
  theme_minimal()  # Apply a minimal theme to the plot






# Reshape data for plotting multiple histograms
df_long <- reshape2::melt(df, measure.vars = c("Na_to_K"))

# Create histograms for both numerical attributes
ggplot(df_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5, alpha = 0.6, color = "black") +  # Adjust binwidth as needed
  labs(
    x = "Value",  # X-axis label
    y = "Frequency",  # Y-axis label
    title = "Histograms of Na_to_k"
  ) +
  facet_wrap(~variable, scales = "free_x") +  # Separate histograms by attribute
  theme_minimal()  # Apply a minimal theme to the plot



#------------------------------- Skewness -------------------------------
# Calculate skewness for numerical variables

age_skewness <- skewness(df$Age, na.rm = TRUE)
na_to_k_skewness <- skewness(df$Na_to_K, na.rm = TRUE)

# Print skewness values with interpretation
cat("Skewness of Age:", age_skewness, "\n")
if (age_skewness > 0) {
  cat("Age is right-skewed (positive skew).\n")
} else if (age_skewness < 0) {
  cat("Age is left-skewed (negative skew).\n")
} else {
  cat("Age is symmetrical.\n")
}

cat("\nSkewness of Na_to_K:", na_to_k_skewness, "\n")
if (na_to_k_skewness > 0) {
  cat("Na_to_K is right-skewed (positive skew).\n")
} else if (na_to_k_skewness < 0) {
  cat("Na_to_K is left-skewed (negative skew).\n")
} else {
  cat("Na_to_K is symmetrical.\n")
}
  
  
  
#-------------------------- Line Histogram --------------------------

# Create line histograms (frequency polygons) for both numerical attributes
# Create the frequency polygon plot
ggplot(df) +
  geom_freqpoly(aes(x = Age, color = "Age"), binwidth = 5, size = 1) +  # Adjust binwidth as needed
  labs(
    x = "Value",  # X-axis label
    y = "Frequency",  # Y-axis label
    title = "Line Histograms (Frequency Polygons) of Age and Na_to_K"
  ) +
  scale_color_manual(name = "Attribute", values = c("Age" = "blue")) +  # Set legend and colors
  theme_minimal()  # Apply a minimal theme to the plot






ggplot(df) +
  geom_freqpoly(aes(x = Na_to_K, color = "Na_to_K"), binwidth = 5, size = 1) +  # Adjust binwidth as needed
  labs(
    x = "Value",  # X-axis label
    y = "Frequency",  # Y-axis label
    title = "Line Histograms (Frequency Polygons) of Age and Na_to_K"
  ) +
  scale_color_manual(name = "Attribute", values = c("Na_to_K" = "red")) +  # Set legend and colors
  theme_minimal()  # Apply a minimal theme to the plot

#---------------mean, median, mode--------------

# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate statistics for Age
mean_age <- mean(df$Age, na.rm = TRUE)
cat("mean for Age: ", mean_age, "\n")

median_age <- median(df$Age, na.rm = TRUE)
cat("median for Age: ", median_age, "\n")

mode_age <- calculate_mode(df$Age)
cat("mode for Age: ", mode_age, "\n")

skewness_age <- skewness(df$Age, na.rm = TRUE)
cat("Skewness for Age: ", skewness_age, "\n")

# Calculate statistics for Na_to_K
mean_na_to_k <- mean(df$Na_to_K, na.rm = TRUE)
cat("mean for Na_to_K: ", mean_na_to_k, "\n")

median_na_to_k <- median(df$Na_to_K, na.rm = TRUE)
cat("median for Na_to_K: ", median_na_to_k, "\n")

mode_na_to_k <- calculate_mode(df$Na_to_K)
cat("mode for Na_to_K: ", mode_na_to_k, "\n")

skewness_na_to_k <- skewness(df$Na_to_K, na.rm = TRUE)
cat("Skewness for Na_to_K: ", skewness_na_to_k, "\n")

# Plot for Age
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean_age), color = "yellow", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = median_age), color = "green", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mode_age), color = "orange", linetype = "solid", size = 1) +
  labs(
    x = "Age", 
    y = "Frequency",  
    title = "Histogram of Age with Mean, Median, and Mode"
  ) +
  theme_minimal()

# Plot for Na_to_K
ggplot(df, aes(x = Na_to_K)) +
  geom_histogram(binwidth = 1, fill = "gray", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean_na_to_k), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_na_to_k), color = "green", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mode_na_to_k), color = "orange", linetype = "solid", size = 1) +
  labs(
    x = "Na_to_K", 
    y = "Frequency",  
    title = "Histogram of Na_to_K with Mean, Median, and Mode"
  ) +
  theme_minimal()



#----------------------Multivariate Visualization--------------------------

#-----------------------Scatter Matrix---------------------------

#------------------- Scatter Plot for Age by Drug Type -------------------
# Select the numerical column (Age) and the categorical column (Drug)
df_selected <- df[, c("Age", "Drug")]

# Scatter plot of Age by Drug Type
ggplot(df_selected, aes(x = Drug, y = Age, color = Drug)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +  # Jitter to avoid overlap
  labs(
    x = "Drug Type",          # X-axis label
    y = "Age",                # Y-axis label
    title = "Scatter Plot of Age by Drug Type"
  ) +
  theme_minimal() +           # Apply a minimal theme
  theme(legend.position = "none")  # Hide the legend for color

#------------------- Density Plot for Age by Drug Type -------------------
# Density plot of Age by Drug Type
ggplot(df_selected, aes(x = Age, fill = Drug)) +
  geom_density(alpha = 0.5) +  # Adds a density plot with transparency
  labs(
    x = "Age",               # X-axis label
    y = "Density",           # Y-axis label
    title = "Density Plot of Age by Drug Type"
  ) +
  theme_minimal()            # Apply a minimal theme

#------------------- Scatter Plot for Na_to_K by Drug Type -------------------
# Select the numerical column (Na_to_K) and the categorical column (Drug)
df_selected <- df[, c("Na_to_K", "Drug")]

# Scatter plot of Na_to_K by Drug Type
ggplot(df_selected, aes(x = Drug, y = Na_to_K, color = Drug)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +  # Jitter to avoid overlap
  labs(
    x = "Drug Type",          # X-axis label
    y = "Na_to_K",            # Y-axis label
    title = "Scatter Plot of Na_to_K by Drug Type"
  ) +
  theme_minimal() +           # Apply a minimal theme
  theme(legend.position = "none")  # Hide the legend for color

#------------------- Density Plot for Na_to_K by Drug Type -------------------
# Density plot of Na_to_K by Drug Type
ggplot(df_selected, aes(x = Na_to_K, fill = Drug)) +
  geom_density(alpha = 0.5) +  # Adds a density plot with transparency
  labs(
    x = "Na_to_K",            # X-axis label
    y = "Density",            # Y-axis label
    title = "Density Plot of Na_to_K by Drug Type"
  ) +
  theme_minimal()             # Apply a minimal theme






#------------------- Violin Plot -------------------
library(reshape2)


# Reshape data for plotting multiple violin plots
df_long <- melt(df, id.vars = "Drug", measure.vars = c("Age"))

# Create violin plots for both numerical attributes
ggplot(df_long, aes(x = Drug, y = value, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Creates the violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Adds a box plot inside the violin plot
  labs(
    x = "Drug Type",        # X-axis label
    y = "Value",            # Y-axis label
    title = "Violin Plots of Age by Drug Type"
  ) +
  facet_wrap(~variable, scales = "free") +  # Separate plots by attribute
  theme_minimal()  # Apply a minimal theme to the plot




# Reshape data for plotting multiple violin plots
df_long <- melt(df, id.vars = "Drug", measure.vars = c("Na_to_K"))

# Create violin plots for both numerical attributes
ggplot(df_long, aes(x = Drug, y = value, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Creates the violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Adds a box plot inside the violin plot
  labs(
    x = "Drug Type",        # X-axis label
    y = "Value",            # Y-axis label
    title = "Violin Plots of Na_to_K by Drug Type"
  ) +
  facet_wrap(~variable, scales = "free") +  # Separate plots by attribute
  theme_minimal()  # Apply a minimal theme to the plot

#------------------- Radar Chart -------------------

# Aggregate the data by Drug to calculate the average of Age and Na_to_K
data_aggregated <- aggregate(cbind(Age, Na_to_K) ~ Drug, df, mean)

# Transpose the data to fit the radar chart format
radar_data <- as.data.frame(t(data_aggregated[,-1]))  # Remove 'Drug' column and transpose
colnames(radar_data) <- as.character(data_aggregated$Drug)  # Rename columns to Drug types

# Add a row for maximum values (for normalization purposes)
radar_data <- rbind(rep(max(radar_data), ncol(radar_data)), radar_data)

# Add a row for minimum values (set to 0)
radar_data <- rbind(rep(0, ncol(radar_data)), radar_data)

# Now 'radar_data' has 3 rows: minimum, maximum, and the actual data

# Create the radar chart with customized colors and layout
radarchart(radar_data, axistype=1,  # Specify axis type for radar chart
           pcol=rainbow(ncol(radar_data)),  # Colors for the radar lines
           pfcol=rainbow(ncol(radar_data), alpha=0.4),  # Colors for the filled area (with transparency)
           plwd=2,  # Line width
           plty=1,  # Line type
           title="Comparison of Drug Types Based on Age and Na_to_K",  # Chart title
           cglcol="grey", cglty=1, cglwd=0.8,  # Grid line colors and width
           vlcex=0.8)  # Label text size

# Add a legend to explain which color corresponds to which Drug Type
legend(x="topright", legend=colnames(radar_data)[-1],  # Exclude the first column (for min/max values)
       bty="n", fill=rainbow(ncol(radar_data)))



#------------------- Line Graph -------------------

# Calculate mean values for Age and Na_to_K by Drug Type
df_summary <- aggregate(cbind(Age) ~ Drug, data = df, FUN = mean)

# Convert the data to long format for ggplot2
df_long <- melt(df_summary, id.vars = "Drug", measure.vars = c("Age"))

# Create a line graph for both numerical attributes
ggplot(df_long, aes(x = Drug, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +  # Draws the line
  geom_point(size = 3) +  # Adds points to the line graph
  labs(
    x = "Drug Type",        # X-axis label
    y = "Mean Value",       # Y-axis label
    title = "Line Graph of Mean Age by Drug Type"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot



# Calculate mean values for Age and Na_to_K by Drug Type
df_summary <- aggregate(cbind(Na_to_K) ~ Drug, data = df, FUN = mean)

# Convert the data to long format for ggplot2
df_long <- melt(df_summary, id.vars = "Drug", measure.vars = c("Na_to_K"))

# Create a line graph for both numerical attributes
ggplot(df_long, aes(x = Drug, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +  # Draws the line
  geom_point(size = 3) +  # Adds points to the line graph
  labs(
    x = "Drug Type",        # X-axis label
    y = "Mean Value",       # Y-axis label
    title = "Line Graph of Mean Na_to_K by Drug Type"
  ) +
  theme_minimal()  # Apply a minimal theme to the plot

