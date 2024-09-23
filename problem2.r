# Load required packages
library(dplyr)
library(ggplot2)
library(readr)

# Define a function to read and process each day's data
process_day_data <- function(filename, day_number) {
  data <- read_csv(filename, show_col_types = FALSE)
  data <- data %>%
    mutate(
      day = day_number,
      # Create age_group variable
      age_group = case_when(
        Age < 20 ~ "<20",
        Age >= 20 & Age <= 29 ~ "20-29",
        Age >= 30 & Age <= 39 ~ "30-39",
        Age >= 40 & Age <= 49 ~ "40-49",
        Age >= 50 & Age <= 59 ~ "50-59",
        Age >= 60 & Age <= 69 ~ "60-69",
        Age >= 70 ~ "70+",
        TRUE ~ NA_character_
      ),
      # Calculate Click-Through Rate (CTR)
      CTR = ifelse(Impressions > 0, Clicks / Impressions, NA_real_)
    )
  return(data)
}

# Process each day's data
data_day1 <- process_day_data("nyt1.csv", 1)
data_day2 <- process_day_data("nyt2.csv", 2)
data_day3 <- process_day_data("nyt3.csv", 3)

# Combine all days' data into one DataFrame
nyt_data <- bind_rows(data_day1, data_day2, data_day3)

# Define user segments based on click behavior
nyt_data <- nyt_data %>%
  mutate(
    # Categorize users based on their click behavior
    click_category = case_when(
      Impressions == 0 ~ "No Impressions",
      Impressions > 0 & Clicks == 0 ~ "No Clicks",
      Clicks == 1 ~ "Low Clicks",
      Clicks >= 2 & Clicks <= 3 ~ "Medium Clicks",
      Clicks >= 4 ~ "High Clicks",
      TRUE ~ NA_character_
    ),
    # Label genders
    gender_label = case_when(
      Gender == 0 ~ "Female",
      Gender == 1 ~ "Male",
      TRUE ~ "Unknown"
    ),
    # Label signed-in status
    signed_in_label = case_when(
      Signed_In == 0 ~ "Not Logged In",
      Signed_In == 1 ~ "Logged In",
      TRUE ~ "Unknown"
    )
  )

# Function to plot distribution of Impressions per age group per day
plot_impressions_boxplot <- function(data, day_number) {
  data_day <- data %>% filter(day == day_number)
  
  ggplot(data_day, aes(x = age_group, y = Impressions)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of Impressions per Age Group - Day", day_number),
         x = "Age Group",
         y = "Number of Impressions") +
    theme_minimal()
}

# Function to plot distribution of CTR per age group per day
plot_ctr_boxplot <- function(data, day_number) {
  data_day <- data %>% filter(day == day_number & !is.na(CTR))
  
  ggplot(data_day, aes(x = age_group, y = CTR)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of CTR per Age Group - Day", day_number),
         x = "Age Group",
         y = "CTR") +
    theme_minimal()
}

# Function to plot CTR by gender for a specific age group and day
plot_ctr_by_gender_age <- function(data, age_group_label, day_number) {
  data_filtered <- data %>%
    filter(day == day_number, age_group == age_group_label, !is.na(CTR))
  
  ggplot(data_filtered, aes(x = gender_label, y = CTR)) +
    geom_boxplot() +
    labs(title = paste("CTR by Gender for Age Group", age_group_label, "- Day", day_number),
         x = "Gender",
         y = "CTR") +
    theme_minimal()
}

# Function to plot CTR by signed-in status for a given day
plot_ctr_by_signed_in <- function(data, day_number) {
  data_filtered <- data %>%
    filter(day == day_number, !is.na(CTR))
  
  ggplot(data_filtered, aes(x = signed_in_label, y = CTR)) +
    geom_boxplot() +
    labs(title = paste("CTR by Signed-In Status - Day", day_number),
         x = "Signed-In Status",
         y = "CTR") +
    theme_minimal()
}

# Plot Impressions and CTR distributions per day
for (day in 1:3) {
  print(plot_impressions_boxplot(nyt_data, day))
  print(plot_ctr_boxplot(nyt_data, day))
}

# Explore data and make comparisons
# Compare CTR between genders for age group "<20" for each day
for (day in 1:3) {
  print(plot_ctr_by_gender_age(nyt_data, "<20", day))
}

# Compare CTR by signed-in status for each day
for (day in 1:3) {
  print(plot_ctr_by_signed_in(nyt_data, day))
}

# Extend analysis across days
# Compute mean CTR per age group per day
ctr_summary <- nyt_data %>%
  group_by(day, age_group) %>%
  summarise(
    mean_CTR = mean(CTR, na.rm = TRUE),
    median_CTR = median(CTR, na.rm = TRUE),
    count = n()
  )

# Plot mean CTR per age group over days
ggplot(ctr_summary, aes(x = factor(day), y = mean_CTR, group = age_group, color = age_group)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean CTR per Age Group over Days",
       x = "Day",
       y = "Mean CTR") +
  theme_minimal()
