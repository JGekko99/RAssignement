# Load required packages
# dplyr: for data manipulation
# ggplot2: for data visualization
# readr: for reading csv files efficiently
library(dplyr)
library(ggplot2)
library(readr)

# Define a function to read and process each day's data
# The function takes a filename and a day number as inputs
process_day_data <- function(filename, day_number) {
  
  # Read the dataset from the provided CSV file (suppressing column type messages)
  data <- read_csv(filename, show_col_types = FALSE)
  
  # Data manipulation using dplyr pipeline:
  # 1. Add a new column 'day' to identify the day number of the dataset
  # 2. Create a new variable 'age_group' based on age ranges
  # 3. Calculate Click-Through Rate (CTR) as Clicks / Impressions, but only when Impressions > 0
  data <- data %>%
    mutate(
      day = day_number,  # Assign day number to each row
      
      # Create age_group variable based on age ranges
      age_group = case_when(
        Age < 20 ~ "<20",
        Age >= 20 & Age <= 29 ~ "20-29",
        Age >= 30 & Age <= 39 ~ "30-39",
        Age >= 40 & Age <= 49 ~ "40-49",
        Age >= 50 & Age <= 59 ~ "50-59",
        Age >= 60 & Age <= 69 ~ "60-69",
        Age >= 70 ~ "70+",
        TRUE ~ NA_character_  # Assign NA for any unexpected cases
      ),
      
      # Calculate Click-Through Rate (CTR) if Impressions > 0, otherwise set to NA
      CTR = ifelse(Impressions > 0, Clicks / Impressions, NA_real_)
    )
  
  # Return the processed data for the day
  return(data)
}

# Process data for each day by calling the function for different CSV files
data_day1 <- process_day_data("nyt1.csv", 1)  # Process day 1 data
data_day2 <- process_day_data("nyt2.csv", 2)  # Process day 2 data
data_day3 <- process_day_data("nyt3.csv", 3)  # Process day 3 data

# Combine data from all three days into a single DataFrame
nyt_data <- bind_rows(data_day1, data_day2, data_day3)

# Part of the request: Segment the users based on their click behavior
nyt_data <- nyt_data %>%
  mutate(
    # Categorize users based on their click behavior
    click_category = case_when(
      Impressions == 0 ~ "No Impressions",           # Users who saw no ads
      Impressions > 0 & Clicks == 0 ~ "No Clicks",   # Users who saw ads but didn't click
      Clicks == 1 ~ "Low Clicks",                    # Users who clicked once
      Clicks >= 2 & Clicks <= 3 ~ "Medium Clicks",   # Users who clicked 2 or 3 times
      Clicks >= 4 ~ "High Clicks",                   # Users who clicked 4+ times
      TRUE ~ NA_character_  # Catch any edge cases
    ),
    
    # Label genders (0 = Female, 1 = Male)
    gender_label = case_when(
      Gender == 0 ~ "Female",
      Gender == 1 ~ "Male",
      TRUE ~ "Unknown"  # Handle any unexpected values
    ),
    
    # Label signed-in status (0 = Not Logged In, 1 = Logged In)
    signed_in_label = case_when(
      Signed_In == 0 ~ "Not Logged In",
      Signed_In == 1 ~ "Logged In",
      TRUE ~ "Unknown"  # Handle any unknown values
    )
  )

# Define a function to plot the distribution of Impressions per age group for a given day
plot_impressions_boxplot <- function(data, day_number) {
  
  # Filter the data for the specific day
  data_day <- data %>% filter(day == day_number)
  
  # Create a boxplot of Impressions per Age Group using ggplot2
  ggplot(data_day, aes(x = age_group, y = Impressions)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of Impressions per Age Group - Day", day_number),
         x = "Age Group",
         y = "Number of Impressions") +
    theme_minimal()  # Use a clean, minimal theme for the plot
}

# Define a function to plot the distribution of CTR per age group for a given day
plot_ctr_boxplot <- function(data, day_number) {
  
  # Filter data for the specific day and remove rows where CTR is NA
  data_day <- data %>% filter(day == day_number & !is.na(CTR))
  
  # Create a boxplot of CTR per Age Group using ggplot2
  ggplot(data_day, aes(x = age_group, y = CTR)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of CTR per Age Group - Day", day_number),
         x = "Age Group",
         y = "CTR") +
    theme_minimal()  # Apply a minimal theme
}

# Define a function to plot CTR by gender for a specific age group and day
plot_ctr_by_gender_age <- function(data, age_group_label, day_number) {
  
  # Filter data for the specified day and age group; exclude rows with NA CTR
  data_filtered <- data %>%
    filter(day == day_number, age_group == age_group_label, !is.na(CTR))
  
  # Create a boxplot of CTR by gender for the specific age group
  ggplot(data_filtered, aes(x = gender_label, y = CTR)) +
    geom_boxplot() +
    labs(title = paste("CTR by Gender for Age Group", age_group_label, "- Day", day_number),
         x = "Gender",
         y = "CTR") +
    theme_minimal()  # Apply a clean theme
}

# Define a function to plot CTR based on whether users were signed in
plot_ctr_by_signed_in <- function(data, day_number) {
  
  # Filter data for the specific day and exclude rows with NA CTR
  data_filtered <- data %>%
    filter(day == day_number, !is.na(CTR))
  
  # Create a boxplot of CTR by signed-in status
  ggplot(data_filtered, aes(x = signed_in_label, y = CTR)) +
    geom_boxplot() +
    labs(title = paste("CTR by Signed-In Status - Day", day_number),
         x = "Signed-In Status",
         y = "CTR") +
    theme_minimal()  # Apply a minimal theme
}

# Part of the request: Plot Impressions and CTR distributions per day
# Loop through each day and generate the required boxplots
for (day in 1:3) {
  print(plot_impressions_boxplot(nyt_data, day))  # Plot Impressions distribution per day
  print(plot_ctr_boxplot(nyt_data, day))  # Plot CTR distribution per day
}

# Part of the request: Explore data and make comparisons
# Compare CTR between genders for age group "<20" for each day
for (day in 1:3) {
  print(plot_ctr_by_gender_age(nyt_data, "<20", day))  # Compare CTR by gender for "<20" age group
}

# Part of the request: Compare CTR by signed-in status for each day
for (day in 1:3) {
  print(plot_ctr_by_signed_in(nyt_data, day))  # Compare CTR by signed-in status for each day
}

# Part of the request: Extend analysis across days
# Compute mean CTR per age group per day
ctr_summary <- nyt_data %>%
  group_by(day, age_group) %>%
  summarise(
    mean_CTR = mean(CTR, na.rm = TRUE),  # Calculate mean CTR, ignoring NA values
    median_CTR = median(CTR, na.rm = TRUE),  # Calculate median CTR, ignoring NA values
    count = n()  # Count the number of records per group
  )

# Part of the request: Visualize some metrics and distributions over time
# Plot mean CTR per age group over days
ggplot(ctr_summary, aes(x = factor(day), y = mean_CTR, group = age_group, color = age_group)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean CTR per Age Group over Days",
       x = "Day",
       y = "Mean CTR") +
  theme_minimal()  # Apply a minimal theme to the plot
