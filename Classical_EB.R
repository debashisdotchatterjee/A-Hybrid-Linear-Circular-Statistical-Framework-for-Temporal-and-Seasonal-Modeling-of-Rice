# Install necessary packages if you haven't already
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("forecast") # or install.packages("fpp2") / install.packages("fpp3")
# install.packages("zoo")
# install.packages("xts")
# install.packages("tseries") # For statistical tests like ADF
# Load the required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)
library(xts)
library(tseries)
library(readxl)

# --- 1. Load the Data ---
# Replace 'your_file_path/MONTHLY AVERAGE OF ENGLISH BAZAR.xlsx - ENGLISH BAZAR.csv'
# with the actual path to your CSV file.

data <- read_excel("D:/RICE PRICE FORECASTING OF MALDA/MONTHLY AVERAGE OF ENGLISH BAZAR.xlsx")

# --- 2. Inspect and Preprocess the Data ---
# View the first few rows and structure
print("Original Data Head:")
print(head(data))
print("Original Data Structure:")
str(data)
summary(data)

# Rename columns for easier use (optional, but good practice)
# R typically replaces spaces with dots or you can use backticks ``
# For this example, let's assume column names are `Time Period`, `Monthly Average of Price`, `Variety of Rice`
# If read_csv converted them, they might be `Time.Period`, `Monthly.Average.of.Price`, etc.
# Let's explicitly rename them to avoid issues.
data <- data %>%
  rename(
    Time_Period = `Time Period`,
    Avg_Price = `Monthly Average of Price`,
    Rice_Variety = `Variety of Rice`
  )

print("Renamed Columns Data Head:")
print(head(data))

# Convert 'Time_Period' to a date object
# The format is "Mon'YY", e.g., "Jun'07"
# We'll parse it as the first day of the month.
data <- data %>%
  mutate(
    Date = parse_date_time(Time_Period, orders = "b'y", locale = "en_US.UTF-8")
  )

# Check for NA dates and handle if any (e.g., if parsing failed for some rows)
print(paste("Number of NA dates after parsing:", sum(is.na(data$Date))))
# data <- data %>% filter(!is.na(Date)) # Option to remove rows with NA dates

# Sort data by date, just in case it's not already sorted
data <- data %>% arrange(Date)

# Handle missing values in 'Avg_Price'
# Option 1: Remove rows with NA prices (if few and random)
# data_no_na <- data %>% filter(!is.na(Avg_Price))

# Option 2: Impute missing values (e.g., using mean, median, or time series methods like na.interp)
# For this example, we'll use na.interp from the forecast package later,
# but first, we need to create a time series object.
# Let's print summary of missing values for Avg_Price
print(paste("Number of NA values in Avg_Price:", sum(is.na(data$Avg_Price))))

# Filter for a specific variety of rice if needed, or analyze the aggregate.
# For example, if you want to analyze only 'Fine' rice:
# data_fine_rice <- data %>% filter(Rice_Variety == "Fine")
# Or, if you want to analyze the "Weightage mean..."
# data_weighted_mean <- data %>% filter(grepl("Weightage mean", Rice_Variety))

# For a general example, let's proceed with all data,
# but be aware that mixing different varieties might not be ideal
# without careful consideration or if they represent different series.
# Let's choose the series that appears most complete or is the primary focus.
# Given the data, "Weightage mean of Fine, Coarse & Other" might be a good candidate if it's consistent.
# For this generic script, we will proceed with rows where Avg_Price is not NA.
# If you have a specific variety or aggregated measure to focus on, filter `data` accordingly *before* creating the ts object.

# For demonstration, let's assume we want to analyze all available price data after handling NAs.
# And we will use the full dataset for now, but be mindful of the 'Rice_Variety' column for specific analyses.

# --- 3. Create a Time Series Object ---
# First, ensure there are no NA prices before creating the ts object or use a method that handles them.
# Let's filter out rows where Avg_Price is NA for simplicity here.
# For a more robust approach with imputation, you would handle NAs on the `ts` object itself.
data_for_ts <- data %>% filter(!is.na(Avg_Price))

if(nrow(data_for_ts) == 0) {
  stop("No data available after removing NAs from Avg_Price. Please check your data.")
}

# Determine the start year and month, and frequency (monthly data = 12)
start_year <- year(min(data_for_ts$Date))
start_month <- month(min(data_for_ts$Date))
frequency <- 12

# Create a `ts` object for 'Avg_Price'
# This requires the data to be continuous in time. If there are gaps, `ts` might not be ideal
# or you'd need to fill them. `xts` or `zoo` are more flexible with irregular dates.

# Using xts (more flexible with dates)
price_xts <- xts(data_for_ts$Avg_Price, order.by = data_for_ts$Date)
print("XTS Time Series Object Head:")
print(head(price_xts))

# Using ts (requires data to be regular, so we should ensure it or impute)
# If there are gaps, we might need to impute. `na.approx` (from zoo) or `na.interp` (from forecast)
# can be used. For `ts`, it's often better to have a full sequence.
# Let's try creating a `ts` object directly.
# If there are NAs in Avg_Price, they will be carried into the ts object.
price_ts <- ts(data$Avg_Price, start = c(year(min(data$Date, na.rm=TRUE)), month(min(data$Date, na.rm=TRUE))), frequency = 12)

# Impute NAs in the ts object if any
if(any(is.na(price_ts))){
  print("Missing values found in ts object. Imputing using na.interp...")
  price_ts <- na.interp(price_ts)
}

print("TS Time Series Object (potentially imputed):")
print(price_ts)


# --- 4. Time Series Plots ---

# Plot 1: Basic Time Series Plot
print("Generating Basic Time Series Plot...")
autoplot(price_ts) +
  ggtitle("Monthly Average Price (EB_Data) Over Time (Imputed)") +
  xlab("Year") +
  ylab("Average Price") +
  theme_minimal()
# For xts object:
# plot(price_xts, main = "Monthly Average Price Over Time", ylab = "Average Price", xlab = "Date")

# Plot 2: Seasonal Subseries Plot
print("Generating Seasonal Subseries Plot...")
ggsubseriesplot(price_ts) +
  ggtitle("Seasonal Subseries Plot of Average Price") +
  ylab("Average Price") +
  theme_minimal()

# Plot 3: Autocorrelation Function (ACF) Plot
print("Generating ACF Plot...")
ggAcf(price_ts) +
  ggtitle("Autocorrelation Function (ACF) for Average Price") +
  theme_minimal()

# Plot 4: Partial Autocorrelation Function (PACF) Plot
print("Generating PACF Plot...")
ggPacf(price_ts) +
  ggtitle("Partial Autocorrelation Function (PACF) for Average Price") +
  theme_minimal()

# Plot 5: Time Series Decomposition Plot
# Using classical decomposition (stl is often preferred for more robustness)
print("Generating Classical Decomposition Plot...")
decomp_classical <- decompose(price_ts, type = "multiplicative") # or "additive"
autoplot(decomp_classical) +
  ggtitle("Classical Decomposition of Average Price (Multiplicative)") +
  theme_minimal()

# Using STL decomposition (Seasonal and Trend decomposition using Loess)
print("Generating STL Decomposition Plot...")
decomp_stl <- stl(price_ts, s.window = "periodic") # s.window can be an integer or "periodic"
autoplot(decomp_stl) +
  ggtitle("STL Decomposition of Average Price") +
  theme_minimal()

# Plot 6: Lag Plot
print("Generating Lag Plot...")
gglagplot(price_ts, lags=12) + # Show lags up to 12 months
  ggtitle("Lag Plot of Average Price") +
  theme_minimal()

# Plot 7: Box Plot by Season/Cycle
print("Generating Seasonal Box Plot...")
ggseasonplot(price_ts, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Average Price") +
  ggtitle("Seasonal Plot: Average Price by Year") +
  theme_minimal()
# --- 5. Fit Seasonal ARIMA Model ---
# Based on your text: ARIMA(2,1,1)(1,1,1)[12]
# It's good practice to split data for robust accuracy metrics.
n_total <- length(price_ts)
n_test <- min(12, floor(n_total * 0.2)) # Use max 12 months or 20% of data for test

if (n_total - n_test < 2 * frequency && n_total >= 2 * frequency) {
  # Adjust n_test if training data becomes too small, but ensure there's enough for at least 2 seasons
  n_test <- n_total - (2 * frequency)
  if (n_test < 0) n_test <- 0 # Ensure n_test is not negative
} else if (n_total < 2 * frequency) {
  n_test <- 0 # If total data is less than 2 seasons, no meaningful split
}

if (n_test > 0) {
  train_data <- window(price_ts, end = time(price_ts)[n_total - n_test])
  test_data <- window(price_ts, start = time(price_ts)[n_total - n_test + 1])
  print(paste("Training ARIMA on", length(train_data), "observations, testing on", length(test_data), "observations."))
} else {
  train_data <- price_ts
  test_data <- NULL
  print("Not enough data for train/test split or data too short. Fitting ARIMA on full dataset.")
}

print("Fitting ARIMA(2,1,1)(1,1,1)[12] model...")
fit_arima <- Arima(train_data, order=c(2,1,1), seasonal=c(1,1,1), include.drift = FALSE)
print(summary(fit_arima))

# --- 6. Residual Plot (Rplot 9_EB.jpg) and Residual Data ---
print("Generating Residual Plot (Rplot 9_EB.jpg)...")
autoplot(residuals(fit_arima)) +
  ggtitle("Residual Plot: ARIMA Model for English Bazar Market") +
  ylab("Residuals") +
  xlab("Time") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal()
ggsave("Rplot 9_EB.jpg", width = 8, height = 5)

print("--- Residual Data from ARIMA Model (first 10 values) ---")
print(head(residuals(fit_arima), 10))
print("--- Summary Statistics of Residuals ---")
print(summary(residuals(fit_arima)))
print(paste("Standard Deviation of Residuals:", round(sd(residuals(fit_arima), na.rm = TRUE), 2)))

# --- 7. Forecast Accuracy Metrics ---
if (!is.null(test_data) && length(test_data) > 0) {
  print("Calculating Forecast Accuracy Metrics (Out-of-sample)...")
  forecast_test <- forecast(fit_arima, h = length(test_data))
  accuracy_metrics <- accuracy(forecast_test, test_data)
} else {
  print("Calculating Forecast Accuracy Metrics (In-sample, as no test set was created)...")
  accuracy_metrics <- accuracy(fit_arima) # In-sample accuracy if no test_data
}

print("--- Forecast Accuracy Metrics ---")
print(accuracy_metrics)

# Extract individual metrics for your LaTeX table
# Note: accuracy() output changes slightly for in-sample vs. out-of-sample.
# For consistency with your provided table structure, we'll use the Training set row for RMSE, MAE, MAPE
# and the Test set row for Theil's U if a test set exists.
rmse_val <- round(accuracy_metrics[1, "RMSE"], 2)
mae_val <- round(accuracy_metrics[1, "MAE"], 2)
mape_val <- round(accuracy_metrics[1, "MAPE"], 2)

theil_u_val <- if (!is.null(test_data) && length(test_data) > 0) {
  round(accuracy_metrics[2, "Theil's U"], 3)
} else {
  NA # Not applicable for purely in-sample
}

cat("\n\n--- Values to populate your LaTeX Forecast Accuracy Table (Table 5) ---\n")
cat(sprintf("RMSE: %.2f\n", rmse_val))
cat(sprintf("MAE: %.2f\n", mae_val))
cat(sprintf("MAPE: %.2f%%\n", mape_val))
cat(sprintf("Theil's U Statistic: %s\n", ifelse(is.na(theil_u_val), "N/A (In-sample)", as.character(theil_u_val))))

# --- 5. Time Series Tables ---

# Table 1: Summary Statistics of the Time Series
print("--- Summary Statistics for Average Price ---")
summary_stats <- summary(coredata(price_ts)) # coredata extracts numeric values from ts
print(summary_stats)
summary_stats

# Additional descriptive statistics
print(paste("Standard Deviation:", sd(price_ts, na.rm = TRUE)))
print(paste("Variance:", var(price_ts, na.rm = TRUE)))

# Table 2: Components from Decomposition (STL example)
print("--- STL Decomposition Components (First 6 values) ---")
stl_components <- as.data.frame(decomp_stl$time.series)
print(head(stl_components))

# Table 3: Results of a Stationarity Test (e.g., Augmented Dickey-Fuller Test)
print("--- Augmented Dickey-Fuller Test for Stationarity ---")
# Null hypothesis: the series is non-stationary
adf_test_result <- adf.test(price_ts, alternative = "stationary")
print(adf_test_result)

# If you want to analyze different rice varieties separately:
# You would loop through unique varieties or filter and run the above analyses.
# Example for one variety:
# unique_varieties <- unique(data$Rice_Variety[!is.na(data$Rice_Variety)])
# if (length(unique_varieties) > 0) {
#   first_variety <- unique_varieties[1]
#   data_variety_subset <- data %>%
#     filter(Rice_Variety == first_variety, !is.na(Avg_Price)) %>%
#     arrange(Date)
#
#   if(nrow(data_variety_subset) > 1) { # Need at least 2 points for a series
#     price_ts_variety <- ts(data_variety_subset$Avg_Price,
#                            start = c(year(min(data_variety_subset$Date)), month(min(data_variety_subset$Date))),
#                            frequency = 12)
#     price_ts_variety <- na.interp(price_ts_variety) # Impute NAs
#
#     print(paste("--- Analysis for Rice Variety:", first_variety, "---"))
#     autoplot(price_ts_variety) + ggtitle(paste("Price for", first_variety))
#     print(summary(price_ts_variety))
#     # ... and other plots/tables as above
#   } else {
#     print(paste("Not enough data points for variety:", first_variety))
#   }
# }


# --- End of Script ---
# Remember to save your plots if needed, e.g., using ggsave() after a ggplot object.
# For example:
# my_plot <- autoplot(price_ts) + ggtitle("My Time Series Plot")
# ggsave("my_time_series_plot.png", plot = my_plot)