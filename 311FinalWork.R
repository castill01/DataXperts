library(readxl)

file_path <- "C:/Users/uppur/Downloads/School/cleaned_dognition_data.xlsx"

dognition_data <- read_excel(file_path)

head(dognition_data)

#Question 1
library(readxl)
library(ggplot2)
library(dplyr)

data_path <- "C:/Users/uppur/Downloads/School/cleaned_dognition_data.xlsx"
dognition_data <- read_excel(data_path)

dog_breed_var <- "Breed"               # Column for dog breed
tests_completed_var <- "Total Tests Completed"  # Column for total tests completed

purebred_data <- dognition_data %>%
  filter(.data[[dog_breed_var]] != "Mixed" & 
           !is.na(.data[[dog_breed_var]]) & 
           !is.na(.data[[tests_completed_var]]))

top_purebred_breeds <- purebred_data %>%
  count(.data[[dog_breed_var]], sort = TRUE) %>%
  top_n(10, n) %>%
  pull(.data[[dog_breed_var]])

filtered_purebred_data <- purebred_data %>%
  filter(.data[[dog_breed_var]] %in% top_purebred_breeds)

summary_data <- filtered_purebred_data %>%
  group_by(.data[[dog_breed_var]]) %>%
  summarize(
    Median = median(.data[[tests_completed_var]], na.rm = TRUE)
  )

# Create a box plot with a strip plot overlay
ggplot(filtered_purebred_data, aes(x = reorder(.data[[dog_breed_var]], .data[[tests_completed_var]], median), y = .data[[tests_completed_var]])) +
  geom_boxplot(outlier.color = NA, fill = "lightblue", alpha = 0.6) + # Box plot without outlier points
  geom_jitter(width = 0.2, color = "darkorange", alpha = 0.7) + # Add individual points
  labs(
    title = "Distribution of Total Tests Completed for Top 10 Purebred Dog Breeds (Excluding Mixed)",
    x = "Dog Breed",
    y = "Total Tests Completed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

colnames(dognition_data)

#Question 2
library(readxl)
library(ggplot2)
library(dplyr)

data_path <- "C:/Users/uppur/Downloads/School/cleaned_dognition_data.xlsx"
dognition_data <- read_excel(data_path)

tests_completed_var <- "Total Tests Completed"   # Dependent variable
engagement_var <- "Sign_in_Count"                # Owner's engagement level (proxy for interaction)

filtered_data <- dognition_data %>%
  filter(!is.na(.data[[tests_completed_var]]) &
           !is.na(.data[[engagement_var]]))

# Plot: Scatter Plot of Test Completions by Engagement Level
engagement_plot <- ggplot(filtered_data, aes(x = .data[[engagement_var]], y = .data[[tests_completed_var]])) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Test Completions by Owner Engagement Level",
    x = "Owner Engagement Level (Sign-in Count)",
    y = "Total Tests Completed"
  ) +
  theme_minimal()

print(engagement_plot)







