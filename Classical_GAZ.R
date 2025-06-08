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
library(forecast) # Using forecast for autoplot, decomposition, etc.
library(zoo)      # For as.yearmon and na.approx
library(xts)      # For xts time series objects
library(tseries)  # For adf.test
library(readxl)

# --- 1. Load the Data ---
# Replace with the actual path if the file is not in your working directory.

gaz_data <- read_excel("D:/RICE PRICE FORECASTING OF MALDA/MONTHLY AVERAGE OF GAZOLE.xlsx")


# --- 2. Inspect and Preprocess the Data ---
print("Original GAZOLE Data Head:")
print(head(gaz_data))
print("Original GAZOLE Data Structure:")
str(gaz_data)
print("Summary of GAZOLE Data (including NAs):")
summary(gaz_data)

# Rename columns for easier use
gaz_data <- gaz_data %>%
  rename(
    Time_Period = `Time Period`,
    Avg_Price = `Monthly Average of Price`,
    Rice_Variety = `Variety of Rice`
  )

print("Renamed Columns GAZOLE Data Head:")
print(head(gaz_data))

# Convert 'Time_Period' to a date object
# The format is "Mon'YY", e.g., "Jun'07". Parsing as the first day of the month.
gaz_data <- gaz_data %>%
  mutate(
    Date = parse_date_time(Time_Period, orders = "b'%y", locale = "en_US.UTF-8")
  )

# Check for NA dates and handle if any
print(paste("Number of NA dates after parsing:", sum(is.na(gaz_data$Date))))
# gaz_data <- gaz_data %>% filter(!is.na(Date)) # Option: remove rows with NA dates

# Sort data by date
gaz_data <- gaz_data %>% arrange(Date)

# Handle missing values in 'Avg_Price'
# The head() output showed leading NAs. Let's see how many NAs overall.
print(paste("Number of NA values in Avg_Price before handling:", sum(is.na(gaz_data$Avg_Price))))

# For creating a 'ts' object, we typically need a continuous series or NAs that can be imputed.
# Option 1: Filter out rows where Avg_Price is NA before creating ts object.
# This is simpler if leading/trailing NAs are extensive and not to be imputed.
# gaz_data_no_na_price <- gaz_data %>% filter(!is.na(Avg_Price))

# Option 2: Keep NAs and impute them after creating the ts object (preferred for internal NAs).
# We will use this approach.

# Note on 'Rice_Variety':
# Like before, if you need to analyze specific rice varieties, filter `gaz_data`
# For example: gaz_data_filtered <- gaz_data %>% filter(Rice_Variety == "Specific Variety")
# The current script will attempt to create a single time series from 'Avg_Price'.
# If different varieties have significantly different price series, consider analyzing them separately.

# --- 3. Create a Time Series Object ---

# Ensure there's at least some non-NA data to work with
if(all(is.na(gaz_data$Avg_Price))) {
  stop("All Avg_Price values are NA. Cannot proceed with time series analysis.")
}

# Find the first and last date with non-NA price data to set up the 'ts' object correctly
first_valid_date_info <- gaz_data %>%
  filter(!is.na(Avg_Price)) %>%
  filter(Date == min(Date, na.rm = TRUE)) %>%
  slice(1) # Ensure only one row if multiple entries for the first date

last_valid_date_info <- gaz_data %>%
  filter(!is.na(Avg_Price)) %>%
  filter(Date == max(Date, na.rm = TRUE)) %>%
  slice(1)

if(nrow(first_valid_date_info) == 0) {
  stop("No non-NA Avg_Price data found to determine start date.")
}

start_year <- year(first_valid_date_info$Date)
start_month <- month(first_valid_date_info$Date)
frequency <- 12 # Monthly data

# Create a full date sequence from the overall min to max date in the file
# This helps in creating a `ts` object that respects all potential time slots.
full_date_range <- seq(min(gaz_data$Date, na.rm = TRUE), max(gaz_data$Date, na.rm = TRUE), by = "month")
full_ts_data <- tibble(Date = full_date_range) %>%
  left_join(gaz_data %>% select(Date, Avg_Price), by = "Date")

# Create a `ts` object for 'Avg_Price'
# This object will have NAs where price was missing in the original data or for interpolated dates.
price_ts <- ts(full_ts_data$Avg_Price,
               start = c(year(min(full_ts_data$Date)), month(min(full_ts_data$Date))),
               frequency = frequency)

print("Raw TS Time Series Object (with NAs):")
# print(price_ts) # Can be long, print summary instead
summary(price_ts)
print(paste("Number of NAs in initial ts object:", sum(is.na(price_ts))))

# Impute NAs in the ts object
# `na.interp` from forecast is good for this.
# Other options: `na.approx` (zoo), `na.kalman` (imputeTS)
if(any(is.na(price_ts))){
  print("Missing values found in ts object. Imputing using na.interp...")
  price_ts_imputed <- na.interp(price_ts)
} else {
  price_ts_imputed <- price_ts
}

# Check if imputation resulted in all NAs (e.g., if all input was NA)
if(all(is.na(price_ts_imputed))) {
  stop("Imputation resulted in all NAs. Please check the input Avg_Price data. There might be too few non-NA values or they are at the very start/end.")
}
# Check if there are any non-finite values after imputation (NaN, Inf)
if (!all(is.finite(price_ts_imputed))) {
  stop("Time series contains non-finite values (NaN or Inf) after imputation. Check data and imputation method.")
}


print("Imputed TS Time Series Object:")
summary(price_ts_imputed)

# Create an XTS object (more flexible with irregular dates, good for original non-NA data)
# This uses only rows where Avg_Price was not initially NA
gaz_data_no_na_price <- gaz_data %>% filter(!is.na(Avg_Price))
if(nrow(gaz_data_no_na_price) > 0) {
  price_xts <- xts(gaz_data_no_na_price$Avg_Price, order.by = gaz_data_no_na_price$Date)
  print("XTS Time Series Object Head (from non-NA original data):")
  print(head(price_xts))
} else {
  print("No non-NA Avg_Price data to create XTS object.")
  price_xts <- NULL # Set to NULL if no data
}


# --- 4. Time Series Plots (using the imputed ts object: price_ts_imputed) ---

# Plot 1: Basic Time Series Plot
print("Generating Basic Time Series Plot...")
autoplot(price_ts_imputed) +
  ggtitle("Monthly Average Price (Gaz_Data) Over Time (Imputed)") +
  xlab("Year") +
  ylab("Average Price") +
  theme_minimal()
# ggsave("gaz_price_timeseries.png")

# Plot 2: Seasonal Subseries Plot
print("Generating Seasonal Subseries Plot...")
if(length(price_ts_imputed) >= 2 * frequency) { # Needs at least 2 full cycles
  ggsubseriesplot(price_ts_imputed) +
    ggtitle("Seasonal Subseries Plot of Average Price (Gaz_Data)") +
    ylab("Average Price") +
    theme_minimal()
  # ggsave("gaz_price_subseries.png")
} else {
  print("Not enough data for a meaningful seasonal subseries plot (need at least 2 full seasons).")
}

# Plot 3: Autocorrelation Function (ACF) Plot
print("Generating ACF Plot...")
ggAcf(price_ts_imputed, lag.max = min(36, length(price_ts_imputed)-1)) + # Adjust lag.max if series is short
  ggtitle("ACF for Average Price (Gaz_Data)") +
  theme_minimal()
# ggsave("gaz_price_acf.png")

# Plot 4: Partial Autocorrelation Function (PACF) Plot
print("Generating PACF Plot...")
ggPacf(price_ts_imputed, lag.max = min(36, length(price_ts_imputed)-1)) + # Adjust lag.max
  ggtitle("PACF for Average Price (Gaz_Data)") +
  theme_minimal()
# ggsave("gaz_price_pacf.png")

# Plot 5: Time Series Decomposition Plot
# Using classical decomposition (stl is often preferred for more robustness)
print("Generating Classical Decomposition Plot...")
# Original line that might cause an error if price_ts has NAs:
# decomp_classical <- decompose(price_ts, type = "multiplicative") # or "additive"

# Corrected line using the imputed time series:
# First, ensure price_ts_imputed exists and is suitable
if(exists("price_ts_imputed") && length(price_ts_imputed) >= 2 * frequency && all(is.finite(price_ts_imputed))) {
  print("Generating Classical Decomposition Plot using imputed data...")
  decomp_classical <- decompose(price_ts_imputed, type = "multiplicative") # or "additive"
  autoplot(decomp_classical) +
    ggtitle("Classical Decomposition of Average Price (Gaz_Data)") +
    theme_minimal()
  # ggsave("gaz_price_classical_decomposition.png")
} else {
  if (!exists("price_ts_imputed")) {
    print("Classical decomposition skipped: 'price_ts_imputed' does not exist.")
  } else if (length(price_ts_imputed) < 2 * frequency) {
    print("Classical decomposition skipped: Not enough data for classical decomposition (need at least 2 full seasons in 'price_ts_imputed').")
  } else {
    print("Classical decomposition skipped: 'price_ts_imputed' contains non-finite values.")
  }
  decomp_classical <- NULL # Ensure it's NULL if not computed
}

# Using STL decomposition (Seasonal and Trend decomposition using Loess)
print("Generating STL Decomposition Plot...")
decomp_stl <- stl(price_ts_imputed, s.window = "periodic") # s.window can be an integer or "periodic"
autoplot(decomp_stl) +
  ggtitle("STL Decomposition of Average Price") +
  theme_minimal()

# Plot 6: Lag Plot
print("Generating Lag Plot...")
gglagplot(price_ts_imputed, lags = min(12, length(price_ts_imputed)-1)) + # Show lags up to 12 months or less
  ggtitle("Lag Plot of Average Price (Gaz_Data)") +
  theme_minimal()
# ggsave("gaz_price_lagplot.png")

# Plot 7: Box Plot by Season/Cycle
print("Generating Seasonal Box Plot...")
if(length(price_ts_imputed) >= frequency) { # Needs at least 1 full cycle
  ggseasonplot(price_ts_imputed, year.labels = TRUE, year.labels.left = TRUE) +
    ylab("Average Price") +
    ggtitle("Seasonal Plot: Average Price (Gaz_Data) by Year") +
    theme_minimal()
  # ggsave("gaz_price_seasonplot_year.png")
  
  # Polar seasonal plot
  # ggseasonplot(price_ts_imputed, polar = TRUE) +
  #   ylab("Average Price") +
  #   ggtitle("Polar Seasonal Plot: Average Price (Gaz_Data)") +
  #   theme_minimal()
} else {
  print("Not enough data for a seasonal plot (need at least 1 full season).")
}


# --- 5. Fit Seasonal ARIMA Model ---
# Based on your text: ARIMA(2,1,1)(1,1,1)[12]
# It's good practice to split data for robust accuracy metrics.
# Let's use a train/test split for forecast accuracy.
n_total <- length(price_ts_imputed)
# Use a reasonable test set size, e.g., last 12 or 24 months
n_test <- min(12, floor(n_total * 0.2)) # Use max 12 months or 20% of data for test
if (n_total - n_test < 2 * frequency) { # Ensure enough data for training after splitting
  n_test <- floor(n_total * 0.1) # Reduce test set if training data becomes too small
  if (n_total - n_test < 2 * frequency) { # If still too small, use full data for fitting
    n_test <- 0
  }
}

if (n_test > 0) {
  train_data <- window(price_ts_imputed, end = time(price_ts_imputed)[n_total - n_test])
  test_data <- window(price_ts_imputed, start = time(price_ts_imputed)[n_total - n_test + 1])
  print(paste("Training ARIMA on", length(train_data), "observations, testing on", length(test_data), "observations."))
} else {
  train_data <- price_ts_imputed
  test_data <- NULL
  print("Not enough data for train/test split. Fitting ARIMA on full dataset.")
}

# Fit the ARIMA model on the training data
# Using your specified model for demonstration: ARIMA(2,1,1)(1,1,1)[12]
# You might use auto.arima(train_data, seasonal = TRUE) to find optimal orders
print("Fitting ARIMA(2,1,1)(1,1,1)[12] model...")
fit_arima <- Arima(train_data, order=c(2,1,1), seasonal=c(1,1,1), include.drift = FALSE)
print(summary(fit_arima))


# --- 6. Residual Plot (Rplot 9_Gaz.jpg) and Residual Data ---
print("Generating Residual Plot (Rplot 9_Gaz.jpg)...")
# Plot of residuals from the fitted ARIMA model
autoplot(residuals(fit_arima)) +
  ggtitle("Residual Plot: ARIMA Model for Gazole Market") +
  ylab("Residuals") +
  xlab("Time") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + # Add a line at zero for reference
  theme_minimal()
ggsave("Rplot 9_Gaz.jpg", width = 8, height = 5) # Save the plot as JPG

# Provide the residual data
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
rmse_val <- round(accuracy_metrics[1, "RMSE"], 2) # Use [1,] for in-sample, [2,] for test set if it exists
mae_val <- round(accuracy_metrics[1, "MAE"], 2)
mape_val <- round(accuracy_metrics[1, "MAPE"], 2)
# Theil's U is typically for out-of-sample forecasts, so check if test_data was used
theil_u_val <- if (!is.null(test_data) && length(test_data) > 0) {
  round(accuracy_metrics[2, "Theil's U"], 3)
} else {
  NA # Not applicable or not directly calculated for in-sample
}

cat("\n\n--- Values to populate your LaTeX Forecast Accuracy Table (Table 4) ---\n")
cat(sprintf("RMSE: %.2f\n", rmse_val))
cat(sprintf("MAE: %.2f\n", mae_val))
cat(sprintf("MAPE: %.2f%%\n", mape_val))
cat(sprintf("Theil's U Statistic: %s\n", ifelse(is.na(theil_u_val), "N/A (In-sample)", as.character(theil_u_val))))



# --- 5. Time Series Tables ---

# Table 1: Summary Statistics of the Imputed Time Series
print("--- Summary Statistics for Imputed Average Price (Gaz_Data) ---")
summary_stats <- summary(coredata(price_ts_imputed))
print(summary_stats)

print(paste("Standard Deviation (Imputed):", sd(price_ts_imputed, na.rm = TRUE))) # na.rm = TRUE is good practice though imputation should have handled NAs
print(paste("Variance (Imputed):", var(price_ts_imputed, na.rm = TRUE)))

# Table 2: Components from STL Decomposition (if performed)
if(!is.null(decomp_stl)) {
  print("--- STL Decomposition Components (First 6 values) ---")
  stl_components <- as.data.frame(decomp_stl$time.series)
  print(head(stl_components))
} else {
  print("STL decomposition was not performed due to insufficient data.")
}

# Table 3: Results of a Stationarity Test (e.g., Augmented Dickey-Fuller Test on imputed series)
print("--- Augmented Dickey-Fuller Test for Stationarity (Imputed Series) ---")
# Check for sufficient observations for adf.test (it can fail on very short series)
if(length(price_ts_imputed) > 10) { # Heuristic threshold, actual requirement might vary
  adf_test_result <- adf.test(price_ts_imputed, alternative = "stationary")
  print(adf_test_result)
} else {
  print("Not enough data points for ADF test after imputation and cleaning.")
}

# Table 4: Cross-validation/Accuracy metrics if doing forecasting (Example placeholder)
# print("--- Forecasting Model Accuracy (Example for later) ---")
# If you build a model, e.g., ETS or ARIMA:
# fit <- ets(price_ts_imputed)
# print(accuracy(fit))


# --- Optional: Analysis for a specific Rice Variety ---
# unique_varieties_gaz <- unique(gaz_data$Rice_Variety[!is.na(gaz_data$Rice_Variety)])
# if (length(unique_varieties_gaz) > 0) {
#   first_variety_gaz <- unique_varieties_gaz[1]
#   print(paste("--- Analyzing specific variety (example):", first_variety_gaz, "---"))
#
#   gaz_data_variety_subset <- gaz_data %>%
#     filter(Rice_Variety == first_variety_gaz, !is.na(Avg_Price)) %>%
#     arrange(Date)
#
#   if(nrow(gaz_data_variety_subset) > 1) {
#     price_ts_variety_gaz <- ts(gaz_data_variety_subset$Avg_Price,
#                                start = c(year(min(gaz_data_variety_subset$Date)), month(min(gaz_data_variety_subset$Date))),
#                                frequency = 12)
#     price_ts_variety_gaz <- na.interp(price_ts_variety_gaz) # Impute NAs if any within this subset
#
#     autoplot(price_ts_variety_gaz) + ggtitle(paste("Price for", first_variety_gaz, "(Gaz_Data)"))
#     # ggsave(paste0("gaz_price_",gsub(" ", "_", first_variety_gaz),".png"))
#     print(summary(price_ts_variety_gaz))
#     # ... and other plots/tables as above for this specific variety
#   } else {
#     print(paste("Not enough data points for variety:", first_variety_gaz))
#   }
# }

# --- End of Script ---
# To save plots, uncomment the ggsave() lines or use them after each plot object is created.
# For example:
# p <- autoplot(price_ts_imputed) + ggtitle("My Time Series Plot (Gaz_Data)")
# ggsave("my_gaz_time_series_plot.png", plot = p)