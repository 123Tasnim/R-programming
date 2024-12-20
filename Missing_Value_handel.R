# Load necessary libraries
install.packages("dplyr")
install.packages("readr")
library(dplyr)
library(readr)

# Load the dataset
file_path <- "C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv"
data <- read_csv(file_path)

# View the first few rows of the dataset
head(data)

# Handling missing values

# Replace missing values in numeric columns with mean
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data$`weight(kg)`[is.na(data$`weight(kg)`)] <- mean(data$`weight(kg)`, na.rm = TRUE)
data$Delivery_time[is.na(data$Delivery_time)] <- mean(data$Delivery_time, na.rm = TRUE)

# Replace missing values in count columns with median
data$Delivery_number <- as.numeric(gsub("y", "", data$Delivery_number))  # Convert non-numeric entries to numeric
data$Delivery_number[is.na(data$Delivery_number)] <- median(data$Delivery_number, na.rm = TRUE)
data$Heart[is.na(data$Heart)] <- median(data$Heart, na.rm = TRUE)
data$Caesarian[is.na(data$Caesarian)] <- median(data$Caesarian, na.rm = TRUE)


# Replace missing values in categorical columns with mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


data$Gender[is.na(data$Gender)] <- get_mode(data$Gender)
data$Blood[is.na(data$Blood)] <- get_mode(data$Blood)

# Verify if all missing values are handled
sum(is.na(data))



duplicate_patient_ids <- data %>%
  group_by(Patient_id) %>%
  filter(n() > 1) %>%
  arrange(Patient_id)

# Print the duplicate Patient_id values
print(duplicate_patient_ids)



# Remove duplicate rows based on the Patient_id column, keeping the first occurrence
filtered_data_no_duplicates <- data %>%
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


weight_filtered_data<- age_filtered_data %>%
  mutate(`weight(kg)` = round(`weight(kg)`, 1))


