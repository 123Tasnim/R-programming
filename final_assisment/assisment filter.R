install.packages("readr")
install.packages("ggplot2")
install.packages("fmsb")
install.packages("GGally")
install.packages("corrplot")

# Load necessary libraries
library(readr)   # For reading CSV files
library(ggplot2) # For creating plots
library(fmsb)    # For creating radar charts
library(GGally)  # For scatterplot matrices
library(corrplot)

# Load the dataset
dataPath <- "C:/Users/user/Desktop/DataScience/final_assisment/drug200.csv"
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

# Subset the numerical columns from df (your dataset)
nDatasetV1 <- df[, sapply(df, is.numeric)]

# Calculate the Pearson correlation matrix for numerical variables
pearson_correlation_matrix <- cor(nDatasetV1, method = "pearson")

# Print the Pearson correlation matrix
print(pearson_correlation_matrix)



# Visualize the correlation matrix using the 'corrplot' function
corrplot(pearson_correlation_matrix, method = "color" ,addCoef.col = "black")



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

# Apply ANOVA on a numerical variable against a categorical variable
# Replace 'Na_to_K' with your numerical variable and 'Drug' with your categorical variable
anova_result <- aov(df$Na_to_K ~ df$Drug)
print("ANOVA Results:")
print(summary(anova_result))

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
  geom_boxplot() +x
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
chisq_result <- chisq.test(table(df$Drug, df$Sex))
print("Chi-Squared Test Results:")
print(chisq_result)

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

