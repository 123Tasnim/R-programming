library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Amelia)
library(readr)



file_pathMMM <- "C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv"
dataMMM <- read_csv(file_pathMMM)

head(dataMMM)


dataMMM$Age[is.na(dataMMM$Age)] <- mean(dataMMM$Age, na.rm = TRUE)
dataMMM$`weight(kg)`[is.na(dataMMM$`weight(kg)`)] <- mean(dataMMM$`weight(kg)`, na.rm = TRUE)
dataMMM$Delivery_time[is.na(dataMMM$Delivery_time)] <- mean(dataMMM$Delivery_time, na.rm = TRUE)

dataMMM$Delivery_number <- as.numeric(gsub("y", "", dataMMM$Delivery_number))  
dataMMM$Delivery_number[is.na(dataMMM$Delivery_number)] <- median(dataMMM$Delivery_number, na.rm = TRUE)
dataMMM$Heart[is.na(dataMMM$Heart)] <- median(dataMMM$Heart, na.rm = TRUE)
dataMMM$Caesarian[is.na(dataMMM$Caesarian)] <- median(dataMMM$Caesarian, na.rm = TRUE)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

dataMMM$Gender[is.na(dataMMM$Gender)] <- get_mode(dataMMM$Gender)
dataMMM$Blood[is.na(dataMMM$Blood)] <- get_mode(dataMMM$Blood)

sum(is.na(dataMMM))



duplicate_patient_ids <- dataMMM %>%
  group_by(Patient_id) %>%
  filter(n() > 1) %>%
  arrange(Patient_id)

print(duplicate_patient_ids)




filtered_data_no_duplicates <- dataMMM %>%
  distinct(Patient_id, .keep_all = TRUE)

print(head(filtered_data_no_duplicates))


convert_data <- filtered_data_no_duplicates %>%
  mutate(Gender = ifelse(Gender %in% c("male", "female"), Gender, "male"))




delivery_time_filtered_data <- convert_data %>%
  mutate(Delivery_time = case_when(
    Delivery_time >= 0 & Delivery_time < 1 ~ 0,
    Delivery_time >= 1 & Delivery_time < 2 ~ 1,
    Delivery_time >= 2 & Delivery_time < 3 ~ 2,
    TRUE ~ Delivery_time
  ))




age_filtered_data <- delivery_time_filtered_data %>%
  mutate(Age = as.integer(Age))


weight_filtered_data<- age_filtered_data %>%
  mutate(`weight(kg)` = round(`weight(kg)`, 1))

convert_weight <- function(weight) {
  if (weight %% 1 == 0) {
    return(as.integer(weight))
  } else {
    return(weight)
  }
}

weight_filtered_data$weight.kg. <- sapply(weight_filtered_data$`weight(kg)`, convert_weight)


print(head(weight_filtered_data))







path <- "C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv"
data <- read.csv(path)

missmap(data, main = "Missing Values Map")

dataset <- read_csv("C:/Users/user/Desktop/DataScience/Midterm_Project_Dataset_section(A).csv")

missing_values <- sapply(dataset, function(x) sum(is.na(x)))

print(missing_values)
total_missing_values <- sum(is.na(dataset))
print(total_missing_values)

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


duplicates <- filtered_data[duplicated(filtered_data$Patient_id) | duplicated(filtered_data$Patient_id, fromLast = TRUE),]

cat("Number of duplicate Patient_id rows:", nrow(duplicates), "\n")

duplicate <- unique(filtered_data$Patient_id)
cat("Without duplicate Patient_id values:", duplicate, "\n")


duplicates_filtered_data <- filtered_data[!duplicated(filtered_data),]

duplicates_clean <- sum(duplicated(duplicates_filtered_data))
cat("Number of duplicate rows after cleaning:", duplicates_clean, "\n")



data_clean_mean <- duplicates_filtered_data
num_cols <- sapply(duplicates_filtered_data, is.numeric)
data_clean_mean[, num_cols] <- lapply(data_clean_mean[, num_cols], function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

data_clean_median <- duplicates_filtered_data
data_clean_median[, num_cols] <- lapply(data_clean_median[, num_cols], function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_clean_mode <- duplicates_filtered_data
data_clean_mode[, num_cols] <- lapply(data_clean_mode[, num_cols], function(x) replace(x, is.na(x), get_mode(x[!is.na(x)])))

data_clean <- data_clean_mean

missing_values_clean <- sum(is.na(data_clean))
cat("Number of missing values after cleaning:", missing_values_clean, "\n")

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

summary_stats <- duplicates_filtered_data %>%
  summarise(across(where(is.numeric), list(mean = mean, median = median)))

print(summary_stats)

modes <- sapply(duplicates_filtered_data %>% select(where(is.numeric)), get_mode)
cat("Modes of numeric attributes:\n")
print(modes)

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

categorical_dataset_after_convert <- duplicates_filtered_data %>%
  mutate(Heart = ifelse(Heart == 1, "Yes", "No"),
         Caesarian = ifelse(Caesarian == 1, "Yes", "No"))

min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

normalized_dataset <- duplicates_filtered_data %>%
  mutate(NormalizedWeight = min_max_normalize(weight.kg.))

head(normalized_dataset)



ggplot(duplicates_filtered_data, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age before Removing Outliers") +
  theme_minimal()

filtered_data_no_outliers <- duplicates_filtered_data %>%
  filter(Age <= 40)

ggplot(filtered_data_no_outliers, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age after Removing Outliers") +
  theme_minimal()

filtered_data_no_outliers <- duplicates_filtered_data %>%
  filter(Delivery_number >= 3)
print(filtered_data_no_outliers)

