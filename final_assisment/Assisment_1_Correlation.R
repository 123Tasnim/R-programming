library(readr)   
library(ggplot2) 
library(GGally)  
library(corrplot)


dataPath <- "C:/Users/user/Desktop/DataScience/final_assisment/drug200.csv"
df <- read.csv(dataPath)





dataset_dimensions <- dim(df)
print(paste("Total number of rows (data points):", dataset_dimensions[1]))
print(paste("Total number of columns (attributes):", dataset_dimensions[2]))



print("Structure of the dataset:")
str(df)


num_numerical <- sum(sapply(df, is.numeric))
print(paste("Total number of numerical attributes:", num_numerical))



num_categorical <- sum(sapply(df, function(x) is.factor(x) || is.character(x)))
print(paste("Total number of categorical attributes:", num_categorical))



missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)


total_missing_values <- sum(is.na(df))
print(paste("Total number of missing values in the dataset : ", total_missing_values))

nDatasetV1 <- df[, sapply(df, is.numeric)]



pearson_correlation_matrix <- cor(nDatasetV1, method = "pearson")
print(pearson_correlation_matrix)
corrplot(pearson_correlation_matrix, method = "color",addCoef.col = "black")





x_var <- df$Age  
y_var <- df$Na_to_K 

correlation_value <- cor(x_var, y_var, method = "pearson", use = "complete.obs")
print(paste("Pearson correlation coefficient (r):", round(correlation_value, 2)))


ggplot(df, aes(x = Age, y = Na_to_K)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +  
  labs(
    x = "Age",        
    y = "Na_to_K",    
    title = paste("Scatter Plot of Age vs Na_to_K (r =", round(correlation_value, 2), ")")  
  ) +
  theme_minimal()  




anova_result <- aov(df$Na_to_K ~ df$Drug)
print("ANOVA Results:")
print(summary(anova_result))


ggplot(df, aes(x = Drug, y = Na_to_K, fill = Drug)) +
  geom_boxplot() +
  labs(
    x = "Drug",  
    y = "Na_to_K",  
    title = "Box Plot of Na_to_K by Drug"
  ) +
  theme_minimal()


ggplot(df, aes(x = Sex, y = Na_to_K, fill = Sex)) +
  geom_boxplot() +
  labs(
    x = "Sex",  
    y = "Na_to_K", 
    title = "Box Plot of Na_to_K by Sex"
  ) +
  theme_minimal()


ggplot(df, aes(x = BP, y = Na_to_K, fill = BP)) +
  geom_boxplot() +
  labs(
    x = "BP",  
    y = "Na_to_K",  
    title = "Box Plot of Na_to_K by BP"
  ) +
  theme_minimal()


ggplot(df, aes(x = Cholesterol, y = Na_to_K, fill = Cholesterol)) +
  geom_boxplot() +
  labs(
    x = "Cholesterol",  
    y = "Na_to_K",  
    title = "Box Plot of Na_to_K by Cholesterol"
  ) +
  theme_minimal()


ggplot(df, aes(x = Drug, y = Age, fill = Drug)) +
  geom_boxplot() +
  labs(
    x = "Drug",  
    y = "Age", 
    title = "Box Plot of Age by Drug"
  ) +
  theme_minimal()


ggplot(df, aes(x = Sex, y = Age, fill = Sex)) +
  geom_boxplot() +
  labs(
    x = "Sex",  
    y = "Age",  
    title = "Box Plot of Age by Sex"
  ) +
  theme_minimal()


ggplot(df, aes(x = BP, y = Age, fill = BP)) +
  geom_boxplot() +
  labs(
    x = "BP",  
    y = "Age",  
    title = "Box Plot of Age by BP"
  ) +
  theme_minimal()


ggplot(df, aes(x = Cholesterol, y = Age, fill = Cholesterol)) +
  geom_boxplot() +
  labs(
    x = "Cholesterol",  
    y = "Age",  
    title = "Box Plot of Age by Cholesterol"
  ) +
  theme_minimal()




chisq_result <- chisq.test(table(df$Drug, df$Sex))
print("Chi-Squared Test Results:")
print(chisq_result)


ggplot(df, aes(x = Drug, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Drug",  
    y = "Count",  
    title = "Bar Plot of Drug by Sex"
  ) +
  theme_minimal()  


ggplot(df, aes(x = Drug, fill = BP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Drug",  
    y = "Count",  
    title = "Bar Plot of Drug by BP"
  ) +
  theme_minimal()  

ggplot(df, aes(x = Drug, fill = Cholesterol)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Drug", 
    y = "Count",  
    title = "Bar Plot of Drug by Cholesterol"
  ) +
  theme_minimal()  



ggplot(df, aes(x = Sex, fill = Drug)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Sex",  
    y = "Count",  
    title = "Bar Plot of Sex by Drug"
  ) +
  theme_minimal()  


ggplot(df, aes(x = Sex, fill = BP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Sex",  
    y = "Count",  
    title = "Bar Plot of Sex by BP"
  ) +
  theme_minimal()  


ggplot(df, aes(x = Sex, fill = Cholesterol)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Sex",  
    y = "Count", 
    title = "Bar Plot of Sex by Cholesterol"
  ) +
  theme_minimal()  


ggplot(df, aes(x = BP, fill = Drug)) +
  geom_bar(position = "dodge") +
  labs(
    x = "BP",  
    y = "Count",  
    title = "Bar Plot of BP by Drug"
  ) +
  theme_minimal() 


ggplot(df, aes(x = BP, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(
    x = "BP",  
    y = "Count",  
    title = "Bar Plot of BP by Sex"
  ) +
  theme_minimal()  


ggplot(df, aes(x = BP, fill = Cholesterol)) +
  geom_bar(position = "dodge") +
  labs(
    x = "BP",  
    y = "Count",  
    title = "Bar Plot of BP by Cholesterol"
  ) +
  theme_minimal() 



ggplot(df, aes(x = Cholesterol, fill = Drug)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Cholesterol",  
    y = "Count",  
    title = "Bar Plot of Cholesterol by Drug"
  ) +
  theme_minimal()  

ggplot(df, aes(x = Cholesterol, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Cholesterol",  
    y = "Count", 
    title = "Bar Plot of Cholesterol by Sex"
  ) +
  theme_minimal()  


ggplot(df, aes(x = Cholesterol, fill = BP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Cholesterol",  
    y = "Count",  
    title = "Bar Plot of Cholesterol by BP"
  ) +
  theme_minimal()  

