# Install and load the dplyr package if you haven't already
library(readxl)       # To read Excel files
library(dplyr)        # For data manipulation
library(ggplot2)      # For data visualization
library(tidyr)        # For data cleaning
library(Amelia)       # For visualizing missing data
library(readr)        # Fast and friendly way to read CSV & TSV file

#-------------Replace Missing Value using Mean, Median, Mode------------------

# Load the dataset
file_pathMMM <- "C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv"
dataMMM <- read_csv(file_pathMMM)

# View the first few rows of the dataset
head(dataMMM)

# Handling missing values

# Replace missing values in numeric columns with mean
dataMMM$Age[is.na(dataMMM$Age)] <- mean(dataMMM$Age, na.rm = TRUE)
dataMMM$`weight(kg)`[is.na(dataMMM$`weight(kg)`)] <- mean(dataMMM$`weight(kg)`, na.rm = TRUE)
dataMMM$Delivery_time[is.na(dataMMM$Delivery_time)] <- mean(dataMMM$Delivery_time, na.rm = TRUE)

# Replace missing values in count columns with median
dataMMM$Delivery_number <- as.numeric(gsub("y", "", dataMMM$Delivery_number))  # Convert non-numeric entries to numeric
print(dataMMM$Delivery_number)
dataMMM$Delivery_number[is.na(dataMMM$Delivery_number)] <- median(dataMMM$Delivery_number, na.rm = TRUE)
dataMMM$Heart[is.na(dataMMM$Heart)] <- median(dataMMM$Heart, na.rm = TRUE)
dataMMM$Caesarian[is.na(dataMMM$Caesarian)] <- median(dataMMM$Caesarian, na.rm = TRUE)

# Replace missing values in categorical columns with mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

dataMMM$Gender[is.na(dataMMM$Gender)] <- get_mode(dataMMM$Gender)
dataMMM$Blood[is.na(dataMMM$Blood)] <- get_mode(dataMMM$Blood)

# Verify if all missing values are handled
sum(is.na(dataMMM))



duplicate_patient_ids <- dataMMM %>%
  group_by(Patient_id) %>%
  filter(n() > 1) %>%
  arrange(Patient_id)

# Print the duplicate Patient_id values
print(duplicate_patient_ids)



# Remove duplicate rows based on the Patient_id column, keeping the first occurrence
filtered_data_no_duplicates <- dataMMM %>%
  distinct(Patient_id, .keep_all = TRUE)

# Print the data after removing duplicates
print(head(filtered_data_no_duplicates))



# Convert any value in the gender column that is not "male" or "female" to "male"
convert_data <- filtered_data_no_duplicates %>%
  mutate(Gender = ifelse(Gender %in% c("male", "female"), Gender, "male"))



# Convert values in Delivery_time column into discrete bins
delivery_time_filtered_data <- convert_data %>%
  mutate(Delivery_time = case_when(
    Delivery_time >= 0 & Delivery_time < 1 ~ 0,
    Delivery_time >= 1 & Delivery_time < 2 ~ 1,
    Delivery_time >= 2 & Delivery_time < 3 ~ 2,
    TRUE ~ Delivery_time
  ))



# Convert age values from float to integer
age_filtered_data <- delivery_time_filtered_data %>%
  mutate(Age = as.integer(Age))

# Convert weight values from float to integer
weight_filtered_data<- age_filtered_data %>%
  mutate(`weight(kg)` = round(`weight(kg)`, 1))

# Function to convert weight values as specified
convert_weight <- function(weight) {
  if (weight %% 1 == 0) {
    return(as.integer(weight))
  } else {
    return(weight)
  }
}

# Apply the function to the weight column
weight_filtered_data$weight.kg. <- sapply(weight_filtered_data$`weight(kg)`, convert_weight)

# Display the updated dataframe
print(head(weight_filtered_data))




#-------------Delete Missing Value------------------


path <- "C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv"

# Read the CSV file into a data frame
data <- read.csv(path)

# Visualize missing values
missmap(data, main = "Missing Values Map")






# Check for missing values

# Load the dataset
dataset <- read_csv("C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv")

# Count missing values in each column
missing_values <- sapply(dataset, function(x) sum(is.na(x)))

# Print the count of missing values for each column
print(missing_values)


# Sum all missing values in the dataset
total_missing_values <- sum(is.na(dataset))

# Print the total number of missing values
print(total_missing_values)





#-------------- All Filters -------------------

# Filter the data 
filtered_data <- data %>%
  filter(Gender %in% c("male", "female")) %>%
  filter(Blood %in% c("high", "normal", "low")) %>%
  filter(Heart %in% c("0", "1")) %>%
  filter(Caesarian %in% c("0", "1")) %>%
  filter(Delivery_number %in% c("1", "2", "3", "4")) %>%
  filter(!is.na(weight.kg.)) %>%
  filter(!is.na(Delivery_time)) %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(Patient_id))






# Find and Remove Duplicate Values
# Find duplicate rows

duplicates <- filtered_data[duplicated(filtered_data$Patient_id) | duplicated(filtered_data$Patient_id, fromLast = TRUE),]

# Count the number of duplicate Patient_id rows
cat("Number of duplicate Patient_id rows:", nrow(duplicates), "\n")

duplicates_clean <- sum(duplicated(duplicates_filtered_data))
cat("Number of duplicate rows after cleaning:", duplicates_clean, "\n")

# Remove duplicate rows
duplicates_filtered_data <- filtered_data[!duplicated(filtered_data),]

# Verify removal of duplicates
duplicates_clean <- sum(duplicated(filtered_data))
cat("Number of duplicate rows after cleaning:", duplicates_clean, "\n")





# Handle Missing Values
# Method 1: Replacing missing values with mean
data_clean_mean <- duplicates_filtered_data
num_cols <- sapply(duplicates_filtered_data, is.numeric)
data_clean_mean[, num_cols] <- lapply(data_clean_mean[, num_cols], function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

# Method 2: Replacing missing values with median
data_clean_median <- duplicates_filtered_data
data_clean_median[, num_cols] <- lapply(data_clean_median[, num_cols], function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))

# Method 3: Replacing missing values with mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_clean_mode <- duplicates_filtered_data
data_clean_mode[, num_cols] <- lapply(data_clean_mode[, num_cols], function(x) replace(x, is.na(x), get_mode(x[!is.na(x)])))

# Choose one method for further processing (e.g., using data_clean_mean)
data_clean <- data_clean_mean

# Check for missing values after cleaning
missing_values_clean <- sum(is.na(data_clean))
cat("Number of missing values after cleaning:", missing_values_clean, "\n")

# Bar graphs of all attributes after removing missing values
bar_graphs_after <- function(df) {
  for (col in colnames(df)) {
    p <- ggplot(df, aes_string(col)) +
      geom_bar(fill = 'lightgreen', color = 'black', alpha = 0.7) +
      ggtitle(paste("Bar graph of", col, "after removing missing values")) +
      theme_minimal()
    print(p)
  }
}

bar_graphs_after(data_clean)

# Calculate and print mean, median, and mode
# Calculate mean, median for continuous attributes
summary_stats <- duplicates_filtered_data %>%
  summarise(across(where(is.numeric), list(mean = mean, median = median)))

print(summary_stats)

modes <- sapply(duplicates_filtered_data %>% select(where(is.numeric)), get_mode)
cat("Modes of numeric attributes:\n")
print(modes)

# Plot mean, median, and mode on a graph
numeric_columns <- duplicates_filtered_data %>% select(where(is.numeric))
stats <- data.frame(Attribute = rep(names(numeric_columns), each = 3),
                    Statistic = rep(c("Mean", "Median", "Mode"), times = ncol(numeric_columns)),
                    Value = c(sapply(numeric_columns, mean),
                              sapply(numeric_columns, median),
                              sapply(numeric_columns, get_mode)))

ggplot(stats, aes(x = Attribute, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  ggtitle("Mean, Median, and Mode of Numeric Attributes")

# Convert Heart and Caesarian columns to categorical attributes
categorical_dataset_after_convert <- duplicates_filtered_data %>%
  mutate(Heart = ifelse(Heart == 1, "Yes", "No"),
         Caesarian = ifelse(Caesarian == 1, "Yes", "No"))




# Min-Max Normalization function
min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply Min-Max normalization to the weight(kg) column
normalized_dataset <- duplicates_filtered_data %>%
  mutate(NormalizedWeight = min_max_normalize(weight.kg.))

# Print the first few rows to see the changes
head(normalized_dataset)





# Plot age data before removing outliers
ggplot(duplicates_filtered_data, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age before Removing Outliers") +
  theme_minimal()

# Remove outliers where age is greater than 40
filtered_data_no_outliers <- duplicates_filtered_data %>%
  filter(Age <= 40)

# Plot age data after removing outliers
ggplot(filtered_data_no_outliers, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age after Removing Outliers") +
  theme_minimal()


# Save the filtered data back to a CSV file
#write.csv(filtered_data, "/path/to/Filtered_Dataset.csv", row.names = FALSE)

