# -----------------------------------------------------------
# 1. LOAD NECESSARY LIBRARIES
# -----------------------------------------------------------
library(readr)       # For reading CSV files (replaced readxl)
library(dplyr)       # For data manipulation
library(circular)    # For circular operations and statistics
library(lubridate)   # For date manipulation
library(ggplot2)     # For plotting
library(readxl)

# est.kappa() comes from CircStats package—install/load if necessary.
if (!require(CircStats)) {
  install.packages("CircStats")
  library(CircStats)
}

# -----------------------------------------------------------
# 2. READ THE DATA & PREPROCESS TIME INFORMATION
# -----------------------------------------------------------
# Read the CSV file (ensure the file is in your working directory)
# Replace "YOUR_FILE_PATH/MONTHLY AVERAGE OF SAMSI.xlsx - SAMSI.csv" with the actual path to your CSV file.
file_path <- "MONTHLY AVERAGE OF SAMSI.xlsx - SAMSI.csv" 

data <- read_excel("D:/RICE PRICE FORECASTING OF MALDA/MONTHLY AVERAGE OF SAMSI.xlsx")


# Filter out rows with missing monthly price values
data <- data %>% filter(!is.na(`Monthly Average of Price`))

# Define ordered month abbreviations
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Process the "Time Period" field to extract month and year.
data <- data %>%
  mutate(
    # E.g., if Time Period is "Jun'07", extract "Jun"
    MonthAbbr = sapply(`Time Period`, function(x) unlist(strsplit(x, "'"))[1]),
    MonthNum  = match(MonthAbbr, month_levels),
    # Extract two-digit year, then convert to full year (assumes 2000s)
    Year = as.numeric(paste0("20", substring(`Time Period`, 
                                             regexpr("'", `Time Period`) + 1, 
                                             regexpr("'", `Time Period`) + 2))),
    # Create a Date column (using the first day of the month)
    Date = as.Date(paste(Year, MonthNum, "01", sep = "-"), format = "%Y-%m-%d")
  ) %>%
  arrange(Date) %>%
  mutate(TimeIndex = row_number()) # TimeIndex for trend

# Compute the seasonal (cyclic) angle for each month.
data <- data %>%
  mutate(
    SeasonAngle = 2 * pi * (MonthNum - 1) / 12,  # 0 for Jan, 2π/12 for Feb, etc.
    CosAngle    = cos(SeasonAngle),
    SinAngle    = sin(SeasonAngle)
  )

# -----------------------------------------------------------
# 3. ROSE DIAGRAM + CIRCULAR KERNEL DENSITY
# -----------------------------------------------------------
# Convert the seasonal angle to degrees for ggplot2 compatibility
data_for_plot <- data.frame(
  angle = as.numeric(data$SeasonAngle) * 180 / pi  # radians to degrees
)

# Wrap angles to [0, 360)
data_for_plot$angle <- data_for_plot$angle %% 360

# Create the rose diagram with kernel density overlay
# Ensure there's data to plot
if(nrow(data_for_plot) > 0) {
  p_rose <- ggplot(data_for_plot, aes(x = angle)) +
    geom_histogram(
      aes(y = after_stat(count)), # Use after_stat(count) for y-axis
      binwidth = 30,              # 12 bins = 360° / 12 = 30°
      fill = "lightblue",
      color = "black",
      boundary = 0                # Start bins at 0, 30, 60...
    ) +
    geom_density(
      aes(y = after_stat(count) * 30), # Scale density to match histogram counts
      adjust = 1.5,
      color = "darkgreen",
      linewidth = 1.2 # Replaced 'size' with 'linewidth' for clarity in newer ggplot2
    ) +
    coord_polar(start = 0, direction = -1) + # Jan at top, clockwise
    scale_x_continuous(
      limits = c(0, 360),
      breaks = seq(0, 330, by = 30), # Breaks for each month
      labels = month.abb # Use standard month abbreviations
    ) +
    labs(title = "Rose Diagram with Kernel Density Overlay_SAM",
         x = "Month", y = "Frequency") +
    theme_minimal()
  print(p_rose)
} else {
  message("No data available for Rose Diagram after processing.")
}


# -----------------------------------------------------------
# 4. FIT THE HARMONIC REGRESSION MODEL (CYCLICAL TIME SERIES WITH TREND)
# -----------------------------------------------------------
# Model rice price as a function of TimeIndex (for trend) and seasonal cosine and sine.
model_combined <- lm(`Monthly Average of Price` ~ TimeIndex + CosAngle + SinAngle, data = data)
cat("---- Harmonic Regression Model with Trend Summary ----\n")
print(summary(model_combined))

# Compute fitted values for observed (training) data.
data <- data %>%
  mutate(Fitted = predict(model_combined, newdata = data))

# -----------------------------------------------------------
# 5. FORECASTING THE NEXT 12 MONTHS
# -----------------------------------------------------------
forecast_horizon <- 12
last_date <- max(data$Date)
last_time_index <- max(data$TimeIndex)

# Generate forecast dates: next 12 months after the last observed date.
forecast_dates <- seq(last_date %m+% months(1), by = "month", length.out = forecast_horizon)

# Build forecast data frame.
forecast_df <- data.frame(Date = forecast_dates) %>%
  mutate(
    MonthNum = month(Date),
    TimeIndex = seq(last_time_index + 1, by = 1, length.out = forecast_horizon), # Add TimeIndex for forecast
    SeasonAngle = 2 * pi * (MonthNum - 1) / 12,
    CosAngle = cos(SeasonAngle),
    SinAngle = sin(SeasonAngle)
  )
forecast_df$Forecast <- predict(model_combined, newdata = forecast_df)

# -----------------------------------------------------------
# 6. TIME SERIES PLOT: OBSERVED, FITTED, & FORECASTED REGIONS
# -----------------------------------------------------------
# Ensure plot limits accommodate all data
y_min <- min(c(data$`Monthly Average of Price`, data$Fitted, forecast_df$Forecast), na.rm = TRUE)
y_max <- max(c(data$`Monthly Average of Price`, data$Fitted, forecast_df$Forecast), na.rm = TRUE)
x_min_date <- min(data$Date, na.rm=TRUE) # Renamed to avoid conflict with x_min in polar plot section
x_max_date <- max(forecast_df$Date, na.rm=TRUE) # Renamed

if(nrow(data) > 0 && nrow(forecast_df) > 0 && !all(is.na(y_min), is.na(y_max))) {
  plot(data$Date, data$`Monthly Average of Price`, type = "p", col = "red", pch = 19,
       xlab = "Date", ylab = "Rice Price", 
       main = "Time Series Plot of Rice Prices_SAM",
       ylim = c(y_min, y_max), xlim = c(x_min_date, x_max_date))
  
  lines(data$Date, data$Fitted, col = "blue", lwd = 2)
  
  plot_usr_coords <- par("usr")
  rect(xleft = last_date + days(1), 
       xright = max(forecast_df$Date) + days(15), 
       ybottom = plot_usr_coords[3],
       ytop = plot_usr_coords[4],
       col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)
  
  lines(forecast_df$Date, forecast_df$Forecast, col = "green", lwd = 2, lty = 2)
  points(forecast_df$Date, forecast_df$Forecast, col = "green", pch = 17)
  
  abline(v = last_date + days(1), col = "darkgray", lty = 3, lwd = 2)
  
  legend("topleft", legend = c("Observed Data", "Fitted Model (with Trend)", "Forecast (with Trend)"),
         col = c("red", "blue", "green"), pch = c(19, NA, 17),
         lty = c(NA, 1, 2), lwd = c(NA, 2, 2))
} else {
  message("Not enough valid data to generate time series plot.")
}

# -----------------------------------------------------------
# 7. CIRCULAR (POLAR) PLOT: OBSERVED, FITTED & FORECASTED IN THE CYCLIC DOMAIN
# -----------------------------------------------------------
# (a) Cartesian coordinates for the Average Seasonal Cycle (at mean trend)
grid_angles_seasonal_cycle <- seq(0, 2 * pi, length.out = 100) # Angles for the smooth cycle
mean_time_index <- mean(data$TimeIndex, na.rm = TRUE)

if (!is.na(mean_time_index)) {
  grid_pred_data_seasonal_cycle <- data.frame(
    TimeIndex = mean_time_index, 
    CosAngle = cos(grid_angles_seasonal_cycle),
    SinAngle = sin(grid_angles_seasonal_cycle)
  )
  # Predict radius for the average seasonal cycle
  radius_seasonal_cycle <- predict(model_combined, newdata = grid_pred_data_seasonal_cycle)
  # Convert to Cartesian coordinates
  x_seasonal_cycle <- radius_seasonal_cycle * cos(grid_angles_seasonal_cycle)
  y_seasonal_cycle <- radius_seasonal_cycle * sin(grid_angles_seasonal_cycle)
} else {
  message("Mean TimeIndex is NA, cannot compute average seasonal cycle for polar plot.")
  x_seasonal_cycle <- y_seasonal_cycle <- numeric(0) # Empty vectors if cannot compute
}


# (b) Cartesian coordinates for Observed Data (chronological path)
# Radii are the actual observed prices. Angles are their seasonal angles.
x_obs_polar <- data$`Monthly Average of Price` * cos(data$SeasonAngle)
y_obs_polar <- data$`Monthly Average of Price` * sin(data$SeasonAngle)

# (c) Cartesian coordinates for Time-Series Fitted Data (chronological path)
# Radii are the fitted prices (trend + season). Angles are their seasonal angles.
x_fitted_ts_polar <- data$Fitted * cos(data$SeasonAngle)
y_fitted_ts_polar <- data$Fitted * sin(data$SeasonAngle)

# (d) Cartesian coordinates for Forecasted Data (chronological path)
# Radii are the forecasted prices. Angles are their seasonal angles.
x_forecast_polar <- forecast_df$Forecast * cos(forecast_df$SeasonAngle)
y_forecast_polar <- forecast_df$Forecast * sin(forecast_df$SeasonAngle)

# Determine plot limits based on all components to be plotted
all_x_polar <- c(x_obs_polar, x_fitted_ts_polar, x_seasonal_cycle, x_forecast_polar)
all_y_polar <- c(y_obs_polar, y_fitted_ts_polar, y_seasonal_cycle, y_forecast_polar)

# Ensure there's valid data for limits
if(length(all_x_polar[!is.na(all_x_polar)]) > 0 && length(all_y_polar[!is.na(all_y_polar)]) > 0) {
  xlim_polar <- range(all_x_polar, na.rm = TRUE)
  ylim_polar <- range(all_y_polar, na.rm = TRUE)
  max_abs_val_polar <- max(abs(c(xlim_polar, ylim_polar)), na.rm = TRUE)
  sym_lim_polar <- c(-max_abs_val_polar, max_abs_val_polar)
  
  # Open a new graphics window if in interactive session
  if (exists("dev.new") && is.function(dev.new) && interactive()) {
    dev.new(title = "Circular Plot: Observed, Fitted & Forecasted Paths")
  }
  
  # Initialize the plot
  plot(NULL, NULL, # Start with an empty plot
       xlab = "X (Price * cos(angle))", ylab = "Y (Price * sin(angle))",
       main = "Circular Seasonal Plot of Rice Price Time Series_SAM",
       asp = 1, # Ensures circular appearance
       xlim = sym_lim_polar, 
       ylim = sym_lim_polar)
  
  # 1. Plot Average Seasonal Cycle (reference shape)
  if(length(x_seasonal_cycle) > 0) {
    lines(x_seasonal_cycle, y_seasonal_cycle, col = "blue", lwd = 2, lty = 3) # Dotted blue line
  }
  
  # 2. Plot Observed Data (Path and Points)
  if(length(x_obs_polar) > 0) {
    lines(x_obs_polar, y_obs_polar, col = "red", lwd = 1.5) # Path
    points(x_obs_polar, y_obs_polar, col = "red", pch = 19, cex = 0.8) # Points
  }
  
  # 3. Plot Time-Series Fitted Data (Path)
  if(length(x_fitted_ts_polar) > 0) {
    lines(x_fitted_ts_polar, y_fitted_ts_polar, col = "purple", lwd = 2) # Path
  }
  
  # 4. Plot Forecasted Data (Path and Points)
  if(length(x_forecast_polar) > 0) {
    lines(x_forecast_polar, y_forecast_polar, col = "darkgreen", lwd = 1.5, lty = 2) # Path
    points(x_forecast_polar, y_forecast_polar, col = "darkgreen", pch = 17, cex = 0.8) # Points
  }
  
  # Add month labels around the circle
  label_radius_polar <- max_abs_val_polar * 1.05 
  month_label_angles_polar <- seq(0, 2 * pi * 11/12, length.out = 12) # SeasonAngles for Jan to Dec
  text_angles_polar <- pi/2 - month_label_angles_polar # Convert to plot's angle system (0=East, pi/2=North)
  text(label_radius_polar * cos(text_angles_polar), 
       label_radius_polar * sin(text_angles_polar), 
       month.abb, cex = 0.8)
  
  # Add legend
  legend("topright", 
         legend = c("Observed (Path & Points)", 
                    "Fitted Time Series (Path)", 
                    "Average Seasonal Cycle", 
                    "Forecast (Path & Points)"),
         col = c("red", "purple", "blue", "darkgreen"), 
         lty = c(1, 1, 3, 2), # Line types
         pch = c(19, NA, NA, 17), # Point characters
         lwd = c(1.5, 2, 2, 1.5), # Line widths
         cex = 0.7, bg="white")
} else {
  message("Not enough valid data to generate the circular time series plot.")
}


# -----------------------------------------------------------
# 8. CIRCULAR SUMMARY STATISTICS & RAYLEIGH TEST
# -----------------------------------------------------------
if(nrow(data) > 0) {
  month_circular <- circular(data$SeasonAngle, 
                             type = "angles", units = "radians", 
                             modulo = "2pi", zero = 0, rotation = "counter")
  
  mean_direction    <- mean(month_circular)
  median_direction  <- median(month_circular)
  resultant_length  <- rho.circular(month_circular)
  circular_variance <- 1 - resultant_length
  kappa_est <- NA
  if(resultant_length > 1e-6 && resultant_length < (1 - 1e-6) && length(month_circular) > 0) {
    kappa_est <- est.kappa(month_circular)
  } else {
    message("Resultant length is too close to 0 or 1, or no data, kappa estimation might be unstable or fail.")
  }
  
  stats_table <- data.frame(
    Statistic = c("Mean Direction (radians)",
                  "Median Direction (radians)",
                  "Resultant Length (rho)",
                  "Circular Variance (1-rho)",
                  "Estimated Kappa (Concentration)"),
    Value = c(mean_direction, median_direction, resultant_length, circular_variance, kappa_est)
  )
  cat("\n---- Circular Summary Statistics ----\n")
  print(stats_table, row.names = FALSE)
  
  cat("\n---- Rayleigh Test for Uniformity ----\n")
  if(length(month_circular) >= 3) {
    rayleigh_result <- rayleigh.test(month_circular)
    print(rayleigh_result)
  } else {
    message("Not enough data points (need at least 3) for Rayleigh test.")
  }
} else {
  message("No data available for circular summary statistics.")
}

cat("\nNote: If the circular plot's 'fitted' line (seasonal cycle) still doesn't capture the pattern well,\n")
cat("you might consider adding more harmonic terms (e.g., for semi-annual patterns) to the model:\n")
cat("`model_combined <- lm(\`Monthly Average of Price\` ~ TimeIndex + CosAngle + SinAngle + cos(2*SeasonAngle) + sin(2*SeasonAngle), data = data)`\n")

