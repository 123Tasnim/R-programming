install.packages("readr")
install.packages("ggplot2")
install.packages("fmsb")
install.packages("GGally")
install.packages("dplyr")
install.packages("e1071")
library(readr)   
library(ggplot2) 
library(fmsb)    
library(GGally)  
library(dplyr)
library(e1071)

library(reshape2)

dataPath <- "C:/Users/user/Desktop/DataScience/final_assisment/drug200.csv"
df <- read.csv(dataPath)



missing_values <- sapply(df, function(x) sum(is.na(x)))


print(missing_values)


total_missing_values <- sum(is.na(df))


print(paste("Total number of missing values in the dataset : ", total_missing_values))




ggplot(df, aes(x = Age, y = Na_to_K)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs Sodium to Potassium Ratio",
       x = "Age",
       y = "Sodium to Potassium Ratio") 
  theme_minimal()


  
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
  
  
  
  
  
ggplot(df, aes(y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age")

ggplot(df, aes(y = Na_to_K)) +
  geom_boxplot() +
  labs(title = "Boxplot of Na_to_K")



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

df_long <- reshape2::melt(df, measure.vars = c("Age"))

ggplot(df_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5, alpha = 0.6, color = "black") + 
  labs(
    x = "Value",  
    y = "Frequency",  
    title = "Histograms of Age"
  ) +
  facet_wrap(~variable, scales = "free_x") +  
  theme_minimal()  



df_long <- reshape2::melt(df, measure.vars = c("Na_to_K"))

ggplot(df_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 5, alpha = 0.6, color = "black") +  
  labs(
    x = "Value",  
    y = "Frequency", 
    title = "Histograms of Na_to_k"
  ) +
  facet_wrap(~variable, scales = "free_x") +  
  theme_minimal()  




age_skewness <- skewness(df$Age, na.rm = TRUE)
na_to_k_skewness <- skewness(df$Na_to_K, na.rm = TRUE)


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




ggplot(df) +
  geom_freqpoly(aes(x = Age, color = "Age"), binwidth = 5, size = 1) +  
  labs(
    x = "Value",  
    y = "Frequency", 
    title = "Line Histograms (Frequency Polygons) of Age and Na_to_K"
  ) +
  scale_color_manual(name = "Attribute", values = c("Age" = "blue")) +  
  theme_minimal() 




ggplot(df) +
  geom_freqpoly(aes(x = Na_to_K, color = "Na_to_K"), binwidth = 5, size = 1) + 
  labs(
    x = "Value",  
    y = "Frequency",  
    title = "Line Histograms (Frequency Polygons) of Age and Na_to_K"
  ) +
  scale_color_manual(name = "Attribute", values = c("Na_to_K" = "blue")) +  
  theme_minimal()  


calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


mean_age <- mean(df$Age, na.rm = TRUE)
cat("mean for Age: ", mean_age, "\n")

median_age <- median(df$Age, na.rm = TRUE)
cat("median for Age: ", median_age, "\n")

mode_age <- calculate_mode(df$Age)
cat("mode for Age: ", mode_age, "\n")

skewness_age <- skewness(df$Age, na.rm = TRUE)
cat("Skewness for Age: ", skewness_age, "\n")


mean_na_to_k <- mean(df$Na_to_K, na.rm = TRUE)
cat("mean for Na_to_K: ", mean_na_to_k, "\n")

median_na_to_k <- median(df$Na_to_K, na.rm = TRUE)
cat("median for Na_to_K: ", median_na_to_k, "\n")

mode_na_to_k <- calculate_mode(df$Na_to_K)
cat("mode for Na_to_K: ", mode_na_to_k, "\n")

skewness_na_to_k <- skewness(df$Na_to_K, na.rm = TRUE)
cat("Skewness for Na_to_K: ", skewness_na_to_k, "\n")


ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean_age), color = "yellow", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = median_age), color = "green", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mode_age), color = "orange", linetype = "solid", size = 1) +
  labs(
    x = "Age", 
    y = "Frequency",  
    title = "Histogram of Age with Mean, Median, and Mode"
  ) +
  theme_minimal()

ggplot(df, aes(x = Na_to_K)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean_na_to_k), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_na_to_k), color = "green", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mode_na_to_k), color = "orange", linetype = "solid", size = 1) +
  labs(
    x = "Na_to_K", 
    y = "Frequency",  
    title = "Histogram of Na_to_K with Mean, Median, and Mode"
  ) +
  theme_minimal()






df_summary <- aggregate(cbind(Age) ~ Drug, data = df, FUN = mean)

df_long <- melt(df_summary, id.vars = "Drug", measure.vars = c("Age"))

ggplot(df_long, aes(x = Drug, y = value, color = variable, group = variable)) +
  geom_line(size = 1) + 
  geom_point(size = 3) + 
  labs(
    x = "Drug Type",        
    y = "Mean Value",       
    title = "Line Graph of Mean Age by Drug Type"
  ) +
  theme_minimal()  



df_summary <- aggregate(cbind(Na_to_K) ~ Drug, data = df, FUN = mean)

df_long <- melt(df_summary, id.vars = "Drug", measure.vars = c("Na_to_K"))

ggplot(df_long, aes(x = Drug, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  labs(
    x = "Drug Type",       
    y = "Mean Value",       
    title = "Line Graph of Mean Na_to_K by Drug Type"
  ) +
  theme_minimal() 









df_selected <- df[, c("Age", "Drug")]

ggplot(df_selected, aes(x = Drug, y = Age, color = Drug)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +  
  labs(
    x = "Drug Type",          
    y = "Age",                
    title = "Scatter Plot of Age by Drug Type"
  ) +
  theme_minimal() +           
  theme(legend.position = "none")  




df_selected <- df[, c("Na_to_K", "Drug")]


ggplot(df_selected, aes(x = Drug, y = Na_to_K, color = Drug)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +  
  labs(
    x = "Drug Type",        
    y = "Na_to_K",            
    title = "Scatter Plot of Na_to_K by Drug Type"
  ) +
  theme_minimal() +           
  theme(legend.position = "none") 


            




df_long <- melt(df, id.vars = "Cholesterol", measure.vars = c("Age"))


ggplot(df_long, aes(x = Cholesterol, y = value, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.7) + 
  geom_boxplot(width = 0.1, color = "blue", alpha = 0.5) +  
  labs(
    x = "Cholesterol",        
    y = "Value",            
    title = "Violin Plots of Age by Cholesterol"
  ) +
  facet_wrap(~variable, scales = "free") +  
  theme_minimal()  




df_long <- melt(df, id.vars = "BP", measure.vars = c("Na_to_K"))

ggplot(df_long, aes(x = BP, y = value, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  
  geom_boxplot(width = 0.1, color = "blue", alpha = 0.5) + 
  labs(
    x = "BP",        
    y = "Value",            
    title = "Violin Plots of Na_to_K by BP"
  ) +
  facet_wrap(~variable, scales = "free") +  
  theme_minimal()  


