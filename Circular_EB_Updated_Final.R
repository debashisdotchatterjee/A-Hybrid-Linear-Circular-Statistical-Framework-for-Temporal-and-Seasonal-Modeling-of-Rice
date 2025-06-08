# -----------------------------------------------------------
# 1. LOAD NECESSARY LIBRARIES
# -----------------------------------------------------------
library(readr)       # For reading CSV files (replaced readxl)
library(dplyr)       # For data manipulation
library(circular)    # For circular operations and statistics
library(lubridate)   # For date manipulation
library(ggplot2)     # For plotting

# est.kappa() comes from CircStats package—install/load if necessary.
if (!require(CircStats)) {
  install.packages("CircStats")
  library(CircStats)
}

# -----------------------------------------------------------
# 2. READ THE DATA & PREPROCESS TIME INFORMATION
# -----------------------------------------------------------
# Read the CSV file (ensure the file is in your working directory)
# Replace "YOUR_FILE_PATH/MONTHLY AVERAGE OF ENGLISH BAZAR.xlsx - ENGLISH BAZAR.csv" with the actual path to your CSV file.
# For example, if the file is in your R working directory:
# data <- read_csv("MONTHLY AVERAGE OF ENGLISH BAZAR.xlsx - ENGLISH BAZAR.csv")
# Please ensure the column names match those used in the script (e.g., "Monthly Average of Price", "Time Period")
# If they are different, you'll need to adjust the column names in the script accordingly.
# For demonstration, I'll use a placeholder for the file name. You MUST change this.
file_path <- "MONTHLY AVERAGE OF ENGLISH BAZAR.xlsx - ENGLISH BAZAR.csv" 
# Check if file exists before attempting to read

data  <- read_excel("D:/RICE PRICE FORECASTING OF MALDA/MONTHLY AVERAGE OF ENGLISH BAZAR.xlsx")


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
      # after_stat(count) for geom_density is density * n.
      # Multiplying by binwidth (30) scales it to be comparable to histogram counts.
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
    labs(title = "Rose Diagram with Kernel Density Overlay_ENGB",
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
# Plot the observed price data.
# Ensure plot limits accommodate all data
y_min <- min(c(data$`Monthly Average of Price`, data$Fitted, forecast_df$Forecast), na.rm = TRUE)
y_max <- max(c(data$`Monthly Average of Price`, data$Fitted, forecast_df$Forecast), na.rm = TRUE)
x_min <- min(data$Date, na.rm=TRUE)
x_max <- max(forecast_df$Date, na.rm=TRUE)

if(nrow(data) > 0 && nrow(forecast_df) > 0) {
  plot(data$Date, data$`Monthly Average of Price`, type = "p", col = "red", pch = 19,
       xlab = "Date", ylab = "Rice Price", 
       main = "Time Series Plot of Rice Prices_ENGB",
       ylim = c(y_min, y_max), xlim = c(x_min, x_max))
  
  # Overlay the fitted values.
  lines(data$Date, data$Fitted, col = "blue", lwd = 2)
  
  # Add a shaded forecast region.
  # Ensure par("usr") is valid, which it should be after plot()
  plot_usr_coords <- par("usr")
  rect(xleft = last_date + 1, # Start of forecast period
       xright = max(forecast_df$Date) + days(15), # Extend slightly beyond last forecast point for visuals
       ybottom = plot_usr_coords[3],
       ytop = plot_usr_coords[4],
       col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)
  
  # Overlay the forecasted curve and forecast points.
  lines(forecast_df$Date, forecast_df$Forecast, col = "green", lwd = 2, lty = 2)
  points(forecast_df$Date, forecast_df$Forecast, col = "green", pch = 17)
  
  # Separate observed and forecast regions with a vertical dashed line.
  abline(v = last_date + days(1), col = "darkgray", lty = 3, lwd = 2) # Use days(1) for precision
  
  legend("topleft", legend = c("Observed Data", "Fitted Model (with Trend)", "Forecast (with Trend)"),
         col = c("red", "blue", "green"), pch = c(19, NA, 17),
         lty = c(NA, 1, 2), lwd = c(NA, 2, 2))
} else {
  message("Not enough data to generate time series plot.")
}

# -----------------------------------------------------------
# 7. CIRCULAR (POLAR) PLOT: OBSERVED, FITTED & FORECASTED IN THE CYCLIC DOMAIN
# -----------------------------------------------------------
# (a) Create a grid of angles (0 to 2π) to compute a smooth cyclic (fitted) curve.
# This fitted curve will represent the seasonal component at the average TimeIndex.
grid_angles <- seq(0, 2 * pi, length.out = 100)
mean_time_index <- mean(data$TimeIndex, na.rm = TRUE)

# Create data for predicting the seasonal cycle at the mean TimeIndex
grid_pred_data_polar <- data.frame(
  TimeIndex = mean_time_index, # Use mean TimeIndex for the representative seasonal cycle
  CosAngle = cos(grid_angles),
  SinAngle = sin(grid_angles)
)
grid_pred_polar_radius <- predict(model_combined, newdata = grid_pred_data_polar)

# Convert grid predictions (fitted seasonal cycle at mean trend) to Cartesian coordinates.
x_grid <- grid_pred_polar_radius * cos(grid_angles)
y_grid <- grid_pred_polar_radius * sin(grid_angles)

# (b) Convert observed data into polar (Cartesian) coordinates.
# Radii are the actual observed prices (which include trend).
x_obs <- data$`Monthly Average of Price` * cos(data$SeasonAngle)
y_obs <- data$`Monthly Average of Price` * sin(data$SeasonAngle)

# (c) Convert forecasted data to polar coordinates using their seasonal angle.
# Radii are the forecasted prices (which include trend and seasonal components).
x_forecast <- forecast_df$Forecast * cos(forecast_df$SeasonAngle)
y_forecast <- forecast_df$Forecast * sin(forecast_df$SeasonAngle)

# Determine plot limits based on all components
xlim_polar <- range(c(x_obs, x_grid, x_forecast), na.rm = TRUE)
ylim_polar <- range(c(y_obs, y_grid, y_forecast), na.rm = TRUE)
# Ensure aspect ratio is 1 by making xlim and ylim symmetric around 0 and equal in range if possible
max_abs_val <- max(abs(c(xlim_polar, ylim_polar)))
sym_lim <- c(-max_abs_val, max_abs_val)


# Open a new graphics window (or use dev.new() if needed)
if (exists("dev.new") && is.function(dev.new) && interactive()) {
  dev.new(title = "Circular Plot: Observed, Fitted & Forecasted")
}

if(length(x_obs) > 0 && length(x_grid) > 0 && length(x_forecast) > 0) {
  plot(x_obs, y_obs, col = "red", pch = 19,
       xlab = "X (Price * cos(angle))", ylab = "Y (Price * sin(angle))",
       main = "Circular Plot of Rice Prices_ENGB (Trend Included)",
       asp = 1, # Ensures circular appearance
       xlim = sym_lim, # Use symmetric limits for better circular representation
       ylim = sym_lim)
  
  # Overlay the fitted cyclic pattern (seasonal component at mean trend).
  lines(x_grid, y_grid, col = "blue", lwd = 2)
  
  # Overlay forecasted points.
  points(x_forecast, y_forecast, col = "green", pch = 17, cex = 1.2)
  
  # Add month labels around the circle
  # Radius for labels (adjust as needed based on plot scale)
  label_radius <- max_abs_val * 1.05 
  month_label_angles <- seq(0, 2 * pi * 11/12, length.out = 12)
  # Adjust start: 0 rad is top (North), positive angles clockwise (due to direction=-1 in rose)
  # For text plotting, standard math angles (0=East, CCW) are often easier.
  # Let's map our SeasonAngle (0=Jan=North, CW) to standard plot angles.
  # Our SeasonAngle: Jan=0, Feb=pi/6, ...
  # Standard plot angle for text: Jan (North) = pi/2, Feb = pi/2 - pi/6, etc.
  # Or, use SeasonAngle directly with x = r*cos(theta), y = r*sin(theta) and adjust text alignment.
  # For simplicity, let's use the SeasonAngle convention (0=Jan at +Y axis)
  # However, plot() uses standard cartesian, so cos(0) is +X.
  # Our SeasonAngle needs mapping for labels if we want Jan at top.
  # x_label = label_radius * sin(month_label_angles) # sin for x if 0 angle is y-axis
  # y_label = label_radius * cos(month_label_angles) # cos for y if 0 angle is y-axis
  
  # Correct label placement for plot() where 0 angle is +X axis:
  # Jan (SeasonAngle=0) should be at the top. Plot's 0 is East. So Jan is at pi/2.
  # Angle mapping: plot_angle = pi/2 - SeasonAngle
  text_angles <- pi/2 - month_label_angles
  text(label_radius * cos(text_angles), label_radius * sin(text_angles), month.abb, cex = 0.8)
  
  
  legend("topright", legend = c("Observed (with Trend)", "Fitted Seasonal Cycle (at Mean Trend)", "Forecast (with Trend)"),
         col = c("red", "blue", "green"), pch = c(19, NA, 17),
         lty = c(NA, 1, NA), lwd = c(NA, 2, NA), cex=0.8)
} else {
  message("Not enough data to generate circular plot.")
}


# -----------------------------------------------------------
# 8. CIRCULAR SUMMARY STATISTICS & RAYLEIGH TEST
# -----------------------------------------------------------
if(nrow(data) > 0) {
  # Create a circular object from the observed season angles.
  month_circular <- circular(data$SeasonAngle, 
                             type = "angles", units = "radians", 
                             modulo = "2pi", zero = 0, rotation = "counter") # rotation refers to input angles if they are measured differently
  
  # Compute key circular statistics.
  mean_direction    <- mean(month_circular)        # Mean direction (radians)
  median_direction  <- median(month_circular)      # Median direction (radians)
  resultant_length  <- rho.circular(month_circular) # Concentration measure (rho)
  circular_variance <- 1 - resultant_length        # Circular variance
  # est.kappa can fail if resultant_length is very small or very close to 1.
  kappa_est <- NA
  if(resultant_length > 1e-6 && resultant_length < (1 - 1e-6)) { # Basic check
    kappa_est <- est.kappa(month_circular)       # Estimated concentration (kappa)
  } else {
    message("Resultant length is too close to 0 or 1, kappa estimation might be unstable or fail.")
  }
  
  
  # Build and print the summary table.
  stats_table <- data.frame(
    Statistic = c("Mean Direction (radians)",
                  "Median Direction (radians)",
                  "Resultant Length (rho)",
                  "Circular Variance (1-rho)",
                  "Estimated Kappa (Concentration)"),
    Value = c(mean_direction, median_direction, resultant_length, circular_variance, kappa_est)
  )
  cat("\n---- Circular Summary Statistics ----\n")
  print(stats_table, row.names = FALSE) # Print without row numbers for cleaner look
  
  # Perform the Rayleigh test to check for uniformity (seasonal non-uniformity).
  cat("\n---- Rayleigh Test for Uniformity ----\n")
  # Rayleigh test requires at least 3 data points.
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

