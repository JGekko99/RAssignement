## Problem 1

# Load necessary libraries
library(openxlsx)
library(dplyr)
library(ggplot2)

# Set working directory (update the path accordingly)
setwd("/Users/federicamalamisura/Desktop/Assignment R 09:24 /Assignment 1/HW1_F24")

# Read in the datasets for each borough
# Use detectDates = TRUE to attempt to read dates correctly
bronx <- read.xlsx("rollingsales_bronx.xlsx", sheet = 1, startRow = 4, detectDates = TRUE)
brooklyn <- read.xlsx("rollingsales_brooklyn.xlsx", sheet = 1, startRow = 4, detectDates = TRUE)
manhattan <- read.xlsx("rollingsales_manhattan.xlsx", sheet = 1, startRow = 4, detectDates = TRUE)
queens <- read.xlsx("rollingsales_queens.xlsx", sheet = 1, startRow = 4, detectDates = TRUE)
statenisland <- read.xlsx("rollingsales_statenisland.xlsx", sheet = 1, startRow = 4, detectDates = TRUE)

# Add a 'BOROUGH' column to each dataset
bronx$BOROUGH <- "Bronx"
brooklyn$BOROUGH <- "Brooklyn"
manhattan$BOROUGH <- "Manhattan"
queens$BOROUGH <- "Queens"
statenisland$BOROUGH <- "Staten Island"

# Combine all boroughs into one dataset
nyc_sales <- bind_rows(bronx, brooklyn, manhattan, queens, statenisland)

# Clean column names: remove spaces, make lowercase, and replace with dots
colnames(nyc_sales) <- make.names(tolower(colnames(nyc_sales)), unique = TRUE)

# Check the class of 'sale.date'
date_class <- class(nyc_sales$sale.date)
print(paste("The class of 'sale.date' is:", date_class))

# Convert 'SALE.DATE' to Date format
if (is.numeric(nyc_sales$sale.date)) {
  # Convert Excel serial date to R Date
  # Excel's origin date is "1899-12-30"
  nyc_sales$sale.date <- as.Date(nyc_sales$sale.date, origin = "1899-12-30")
} else if (is.character(nyc_sales$sale.date)) {
  # Convert date from character string
  nyc_sales$sale.date <- as.Date(nyc_sales$sale.date, format = "%m/%d/%Y")
} else if (inherits(nyc_sales$sale.date, "POSIXct") | inherits(nyc_sales$sale.date, "POSIXt")) {
  # If already a date-time object, convert to Date
  nyc_sales$sale.date <- as.Date(nyc_sales$sale.date)
} else {
  stop("Unhandled date format in 'sale.date' column.")
}

# Handle missing dates (if any)
nyc_sales <- nyc_sales %>% filter(!is.na(sale.date))

# Ensure numerical columns are numeric: remove commas, dollar signs, and convert to numeric
numeric_columns <- c("residential.units", "commercial.units", "total.units",
                     "land.square.feet", "gross.square.feet", "year.built", "sale.price")

nyc_sales[numeric_columns] <- lapply(nyc_sales[numeric_columns], function(x) as.numeric(gsub("[^0-9]", "", as.character(x))))

# Remove rows with missing or zero 'SALE.PRICE' (unlikely valid sales)
nyc_sales <- nyc_sales %>% filter(!is.na(sale.price) & sale.price > 0)

# Identify and handle outliers in 'SALE.PRICE'
upper_limit <- quantile(nyc_sales$sale.price, 0.99, na.rm = TRUE)
nyc_sales <- nyc_sales %>% filter(sale.price <= upper_limit)

# Ensure 'BUILDING.CLASS.CATEGORY' is a factor
nyc_sales$building.class.category <- as.factor(nyc_sales$building.class.category)

# Select specific residential building categories
selected_categories <- c(
  "01 ONE FAMILY DWELLINGS",
  "02 TWO FAMILY DWELLINGS",
  "03 THREE FAMILY DWELLINGS",
  "09 COOPS - WALKUP APARTMENTS",
  "10 COOPS - ELEVATOR APARTMENTS",
  "12 CONDOS - WALKUP APARTMENTS",
  "13 CONDOS - ELEVATOR APARTMENTS"
)

# Filter data for the selected categories
nyc_residential <- nyc_sales %>%
  filter(building.class.category %in% selected_categories)

# Extract year and month from 'SALE.DATE' using base R functions
nyc_residential$sale.year <- as.numeric(format(nyc_residential$sale.date, "%Y"))
nyc_residential$sale.month <- as.numeric(format(nyc_residential$sale.date, "%m"))

# Exploratory Data Analysis (EDA)
# 1. Boxplot of 'SALE.PRICE' by borough
ggplot(nyc_residential, aes(x = BOROUGH, y = sale.price)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, upper_limit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sale Price Distribution by Borough",
       x = "Borough", y = "Sale Price")

# 2. Histogram of 'SALE.PRICE' for each borough
ggplot(nyc_residential, aes(x = sale.price)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "white") +
  facet_wrap(~ BOROUGH, scales = "free_y") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Histogram of Sale Prices by Borough",
       x = "Sale Price", y = "Count")

# 3. Scatter plot of 'SALE.PRICE' over time by borough
ggplot(nyc_residential, aes(x = sale.date, y = sale.price, color = BOROUGH)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_y_continuous(labels = scales::comma, limits = c(0, upper_limit)) +
  labs(title = "Sale Price Over Time by Borough",
       x = "Sale Date", y = "Sale Price") +
  theme(legend.position = "bottom")

# 4. Boxplot of 'SALE.PRICE' by building class category and borough
ggplot(nyc_residential, aes(x = building.class.category, y = sale.price, fill = BOROUGH)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, upper_limit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sale Price by Building Class Category and Borough",
       x = "Building Class Category", y = "Sale Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Scatter plot of 'GROSS.SQUARE.FEET' vs 'SALE.PRICE' colored by borough
ggplot(nyc_residential, aes(x = gross.square.feet, y = sale.price, color = BOROUGH)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma, limits = c(0, upper_limit)) +
  labs(title = "Gross Square Feet vs Sale Price",
       x = "Gross Square Feet", y = "Sale Price") +
  theme(legend.position = "bottom")

# Summary Statistics
summary_stats <- nyc_residential %>%
  group_by(BOROUGH, building.class.category) %>%
  summarize(
    count = n(),
    mean_price = mean(sale.price, na.rm = TRUE),
    median_price = median(sale.price, na.rm = TRUE),
    sd_price = sd(sale.price, na.rm = TRUE),
    min_price = min(sale.price, na.rm = TRUE),
    max_price = max(sale.price, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)
