## Problem 1

# Ensure necessary packages are installed and loaded
required_packages <- c("openxlsx", "dplyr", "ggplot2", "scales")
installed_packages <- rownames(installed.packages())

for (p in required_packages) {
  if (!(p %in% installed_packages)) {
    install.packages(p)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

# Set working directory (update the path accordingly)
# setwd("/cloud/project")  # Uncomment and set the correct path

# Function to read in a dataset, standardize column types, and add the 'borough_name' column
read_sales_data <- function(file_path, borough_name, start_row) {
  data <- read.xlsx(file_path, sheet = 1, startRow = start_row, colNames = TRUE, detectDates = FALSE)
  
  # Clean column names immediately
  colnames(data) <- make.names(tolower(colnames(data)), unique = TRUE)
  
  # Assign 'borough_name' column after cleaning column names
  data$borough_name <- borough_name
  
  # List of columns to transform to character due to inconsistent data types
  cols_to_character <- c("borough", "neighborhood", "building.class.category", "tax.class.at.present", 
                         "block", "lot", "easement", "building.class.at.present",
                         "address", "apartment.number", "zip.code",
                         "tax.class.at.time.of.sale", "building.class.at.time.of.sale")
  
  # Transform columns to character if they exist in the data
  for (col in cols_to_character) {
    if (col %in% names(data)) {
      data[[col]] <- as.character(data[[col]])
    }
  }
  
  return(data)
}

# Determine the correct 'startRow' by inspecting the Excel files
start_row <- 5  # Adjust this number based on your Excel files

# Read in the datasets for each borough
bronx <- read_sales_data("rollingsales_bronx.xlsx", "Bronx", start_row)
brooklyn <- read_sales_data("rollingsales_brooklyn.xlsx", "Brooklyn", start_row)
manhattan <- read_sales_data("rollingsales_manhattan.xlsx", "Manhattan", start_row)
queens <- read_sales_data("rollingsales_queens.xlsx", "Queens", start_row)
statenisland <- read_sales_data("rollingsales_statenisland.xlsx", "Staten Island", start_row)

# Combine all boroughs into one dataset
nyc_sales <- dplyr::bind_rows(bronx, brooklyn, manhattan, queens, statenisland)

# Verify that 'sale.date' column exists
if (!"sale.date" %in% names(nyc_sales)) {
  cat("Available column names are:\n")
  print(names(nyc_sales))
  stop("The 'sale.date' column does not exist in nyc_sales. Please check the column names.")
}

# Check the class and sample values of 'sale.date'
cat("\nClass of 'sale.date':", class(nyc_sales$sale.date), "\n")
cat("Sample values of 'sale.date':\n")
print(head(nyc_sales$sale.date))

# Convert 'sale.date' to Date format
if (is.numeric(nyc_sales$sale.date)) {
  # Convert Excel serial date numbers to R Date objects
  nyc_sales$sale.date <- as.Date(nyc_sales$sale.date, origin = "1899-12-30")
  cat("Converted 'sale.date' from Excel serial numbers to Date format.\n")
} else if (is.character(nyc_sales$sale.date) || is.factor(nyc_sales$sale.date)) {
  nyc_sales$sale.date <- trimws(as.character(nyc_sales$sale.date))
  nyc_sales$sale.date[nyc_sales$sale.date == ""] <- NA
  possible_formats <- c("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y")
  converted <- FALSE
  for (fmt in possible_formats) {
    temp_dates <- as.Date(nyc_sales$sale.date, format = fmt)
    na_percentage <- sum(is.na(temp_dates)) / length(temp_dates)
    if (na_percentage < 0.1) {
      nyc_sales$sale.date <- temp_dates
      converted <- TRUE
      cat("Dates converted using format:", fmt, "\n")
      break
    }
  }
  if (!converted) {
    stop("Date conversion failed. Please check the date formats in 'sale.date' column.")
  }
} else {
  stop("Unhandled data type for 'sale.date' column.")
}

# Handle missing dates (if any)
nyc_sales <- nyc_sales %>% filter(!is.na(sale.date))

# Data Cleaning: Convert relevant columns to appropriate data types
numeric_columns <- c("residential.units", "commercial.units", "total.units", "land.square.feet",
                     "gross.square.feet", "year.built", "sale.price")

# Ensure numeric columns exist
numeric_columns <- intersect(numeric_columns, names(nyc_sales))

# Clean and convert numeric columns
for (col in numeric_columns) {
  nyc_sales[[col]] <- as.numeric(gsub("[^0-9\\.]", "", as.character(nyc_sales[[col]])))
}

# Remove rows with missing or zero 'sale.price' (unlikely valid sales)
nyc_sales <- nyc_sales %>% filter(!is.na(sale.price) & sale.price > 0)

# Identify and handle outliers in 'sale.price'
upper_limit <- quantile(nyc_sales$sale.price, 0.995, na.rm = TRUE)  # Adjusted to 99.5th percentile
cat("Upper limit for sale.price:", upper_limit, "\n")
nyc_sales <- nyc_sales %>% filter(sale.price <= upper_limit)

# Clean and standardize 'building.class.category'
if ("building.class.category" %in% names(nyc_sales)) {
  nyc_sales$building.class.category <- toupper(trimws(nyc_sales$building.class.category))
} else {
  stop("The 'building.class.category' column does not exist in nyc_sales.")
}

# Define selected categories (also uppercase and trimmed)
selected_categories <- toupper(c(
  "01 ONE FAMILY DWELLINGS",
  "02 TWO FAMILY DWELLINGS",
  "03 THREE FAMILY DWELLINGS",
  "09 COOPS - WALKUP APARTMENTS",
  "10 COOPS - ELEVATOR APARTMENTS",
  "12 CONDOS - WALKUP APARTMENTS",
  "13 CONDOS - ELEVATOR APARTMENTS"
))

# Filter data for the selected categories
nyc_residential <- nyc_sales %>%
  filter(building.class.category %in% selected_categories)

# Check the number of rows in nyc_residential
cat("Number of rows in nyc_residential:", nrow(nyc_residential), "\n")

# Check distribution of 'borough_name'
cat("Distribution of 'borough_name' in nyc_residential:\n")
print(table(nyc_residential$borough_name))

# Check unique values of 'building.class.category' in nyc_residential
cat("Unique 'building.class.category' values in nyc_residential:\n")
print(unique(nyc_residential$building.class.category))

# Extract year and month from 'sale.date' using base R functions
nyc_residential$sale.year <- as.numeric(format(nyc_residential$sale.date, "%Y"))
nyc_residential$sale.month <- as.numeric(format(nyc_residential$sale.date, "%m"))

# Exploratory Data Analysis (EDA)

# Proceed only if nyc_residential is not empty
if (nrow(nyc_residential) > 0) {
  # 1. Boxplot of 'sale.price' by borough
  ggplot(nyc_residential, aes(x = borough_name, y = sale.price)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0, upper_limit)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Sale Price Distribution by Borough",
         x = "Borough", y = "Sale Price")
  
  # 2. Histogram of 'sale.price' for each borough
  ggplot(nyc_residential, aes(x = sale.price)) +
    geom_histogram(binwidth = 50000, fill = "blue", color = "white") +
    facet_wrap(~ borough_name, scales = "free_y") +
    scale_x_continuous(labels = scales::comma) +
    labs(title = "Histogram of Sale Prices by Borough",
         x = "Sale Price", y = "Count")
  
  # 3. Scatter plot of 'sale.price' over time by borough
  ggplot(nyc_residential, aes(x = sale.date, y = sale.price, color = borough_name)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess") +
    scale_y_continuous(labels = scales::comma, limits = c(0, upper_limit)) +
    labs(title = "Sale Price Over Time by Borough",
         x = "Sale Date", y = "Sale Price") +
    theme(legend.position = "bottom")
  
  # 4. Boxplot of 'sale.price' by building class category and borough
  ggplot(nyc_residential, aes(x = building.class.category, y = sale.price, fill = borough_name)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0, upper_limit)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Sale Price by Building Class Category and Borough",
         x = "Building Class Category", y = "Sale Price") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 5. Scatter plot of 'gross.square.feet' vs 'sale.price' colored by borough
  ggplot(nyc_residential, aes(x = gross.square.feet, y = sale.price, color = borough_name)) +
    geom_point(alpha = 0.3) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma, limits = c(0, upper_limit)) +
    labs(title = "Gross Square Feet vs Sale Price",
         x = "Gross Square Feet", y = "Sale Price") +
    theme(legend.position = "bottom")
  
  # Summary Statistics
  summary_stats <- nyc_residential %>%
    group_by(borough_name, building.class.category) %>%
    summarize(
      count = n(),
      mean_price = mean(sale.price, na.rm = TRUE),
      median_price = median(sale.price, na.rm = TRUE),
      sd_price = sd(sale.price, na.rm = TRUE),
      min_price = min(sale.price, na.rm = TRUE),
      max_price = max(sale.price, na.rm = TRUE)
    ) %>%
    arrange(borough_name, building.class.category)
  
  # Print the summary statistics
  print(summary_stats)
} else {
  cat("No data available in nyc_residential after filtering. Please check the data and filters.\n")
}
