packages <- c("tidyquant", "quantmod", "tidyverse", "TTR", "PerformanceAnalytics", 
              "lubridate", "fredr", "WDI", "Quandl", "dplyr", "caret", "tidyr")

install.packages(packages)

library(tidyquant)
library(quantmod)
library(tidyverse)
library(TTR)
library(PerformanceAnalytics)
library(lubridate)
library(fredr)
library(WDI)
library(Quandl)
library(dplyr)
library(caret)  
library(tidyr)

library(zoo)
library(MASS)   
library(pls)     
library(ggplot2) 


# Set parameters
start_date <- as.Date("2010-10-01")
end_date <- Sys.Date() 

# Get Tesla Data
tesla <- getSymbols("TSLA", from = start_date, to = end_date, auto.assign = FALSE)
tesla_weekly <- to.weekly(tesla)

tesla_data <- data.frame(
  date = as.Date(index(tesla_weekly)),
  price = as.numeric(Cl(tesla_weekly)),
  volume = as.numeric(Vo(tesla_weekly)),
  high = as.numeric(Hi(tesla_weekly)),
  low = as.numeric(Lo(tesla_weekly))
)

# Calculate Tesla technical indicators
tesla_data$weekly_return <- ROC(tesla_data$price)  # Target variable
tesla_data$volatility <- runSD(tesla_data$price, n = 4)
tesla_data$rsi <- RSI(tesla_data$price, n = 14)
tesla_data$ma_4 <- SMA(tesla_data$price, n = 4)
tesla_data$ma_12 <- SMA(tesla_data$price, n = 12)
tesla_data$volume_ma4 <- SMA(tesla_data$volume, n = 4)
tesla_data$volume_ratio <- tesla_data$volume/tesla_data$volume_ma4
tesla_data$high_low_ratio <- tesla_data$high/tesla_data$low
tesla_data$momentum <- ROC(tesla_data$price, n = 4)

# Get Market Index Data
cat("\nGetting market indices data...\n")
# S&P 500
sp500 <- getSymbols("^GSPC", from = start_date, to = end_date, auto.assign = FALSE)
sp500_weekly <- to.weekly(sp500)
sp500_returns <- periodReturn(Cl(sp500_weekly), period = "weekly", type = "log")
sp500_df <- data.frame(
  date = as.Date(index(sp500_returns)),
  SP500_return = as.numeric(sp500_returns)
)

# NASDAQ
nasdaq <- getSymbols("^IXIC", from = start_date, to = end_date, auto.assign = FALSE)
nasdaq_weekly <- to.weekly(nasdaq)
nasdaq_returns <- periodReturn(Cl(nasdaq_weekly), period = "weekly", type = "log")
nasdaq_df <- data.frame(
  date = as.Date(index(nasdaq_returns)),
  NASDAQ_return = as.numeric(nasdaq_returns)
)

# Dow Jones
dow <- getSymbols("^DJI", from = start_date, to = end_date, auto.assign = FALSE)
dow_weekly <- to.weekly(dow)
dow_returns <- periodReturn(Cl(dow_weekly), period = "weekly", type = "log")
dow_df <- data.frame(
  date = as.Date(index(dow_returns)),
  DJI_return = as.numeric(dow_returns)
)

# Merge market indices
index_data <- merge(sp500_df, nasdaq_df, by = "date")
index_data <- merge(index_data, dow_df, by = "date")

# Get Competitor Returns
cat("\nGetting competitor data...\n")
competitors <- c("F", "GM", "TM")
comp_returns_list <- list()

for(comp in competitors) {
  cat("Processing", comp, "...\n")
  data <- getSymbols(comp, from = start_date, to = end_date, auto.assign = FALSE)
  weekly_prices <- to.weekly(data)
  returns <- periodReturn(Cl(weekly_prices), period = "weekly", type = "log")
  
  comp_returns_list[[comp]] <- data.frame(
    date = as.Date(index(returns)),
    returns = as.numeric(returns)
  )
  names(comp_returns_list[[comp]])[2] <- paste0(comp, "_return")
}

# Merge competitor returns
comp_data <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), comp_returns_list)

# Get FRED Economic Data
cat("\nGetting FRED economic data...\n")
fredr_set_key("1689e1d6f8de50490073b29757aaf2fb")

fred_indicators <- c(
  "UNRATE",     # Unemployment
  "DCOILWTICO", # Oil
  "DGS10",      # 10Y Treasury
  "VIXCLS"      # VIX
)

fred_data_list <- list()
for(indicator in fred_indicators) {
  cat("Getting", indicator, "...\n")
  data <- fredr(
    series_id = indicator,
    observation_start = start_date,
    observation_end = end_date,
    frequency = "m"  # monthly frequency
  )
  
  clean_data <- data.frame(
    date = as.Date(data$date),
    value = data$value
  )
  names(clean_data)[2] <- indicator
  
  # Convert to weekly
  weekly_dates <- seq.Date(from = min(clean_data$date),
                           to = max(clean_data$date),
                           by = "week")
  weekly_data <- data.frame(date = weekly_dates)
  weekly_data <- merge(weekly_data, clean_data, by = "date", all.x = TRUE)
  weekly_data[,2] <- na.locf(weekly_data[,2], na.rm = FALSE)
  
  fred_data_list[[indicator]] <- weekly_data
}

# Merge FRED data
fred_data <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), fred_data_list)

# Merge All Data
cat("\nMerging all data sources...\n")
final_data <- merge(tesla_data, index_data, by = "date", all = FALSE)
final_data <- merge(final_data, comp_data, by = "date", all = FALSE)
final_data <- merge(final_data, fred_data, by = "date", all = FALSE)
final_data <- final_data[order(final_data$date), ]

# Create Lagged Variables
cat("\nCreating lagged variables...\n") 
cols_to_lag <- setdiff(names(final_data), c("date", "weekly_return", "price"))
for(col in cols_to_lag) {
  for(lag in 1:10) {
    final_data[[paste0(col, "_lag", lag)]] <- lag(final_data[[col]], lag)
  }
} 

# Add time-based features
final_data$week_num <- as.numeric(difftime(final_data$date, min(final_data$date), units="weeks"))
final_data$year <- year(final_data$date)
final_data$month <- month(final_data$date)
final_data$quarter <- quarter(final_data$date)
final_data$day_of_year <- yday(final_data$date)
final_data$week_of_year <- week(final_data$date)

# Create cyclical features for seasonal patterns
final_data$month_sin <- sin(2 * pi * final_data$month/12)
final_data$month_cos <- cos(2 * pi * final_data$month/12)
final_data$week_sin <- sin(2 * pi * final_data$week_of_year/52)
final_data$week_cos <- cos(2 * pi * final_data$week_of_year/52)

# Optional: Add trend features
final_data$week_num_squared <- final_data$week_num^2
final_data$log_week_num <- log(final_data$week_num + 1)

# Clean and prepare final matrices (updated version)
final_data_clean <- final_data[final_data$date >= as.Date("2011-01-01"), ]
final_data_clean <- na.omit(final_data_clean)

# Create response vector and design matrix
y <- final_data_clean$weekly_return

# Update cols_to_exclude to keep time features
cols_to_exclude <- c("date", "weekly_return", "price")
X <- as.matrix(final_data_clean[, !names(final_data_clean) %in% cols_to_exclude])

# Print summary of new features
cat("\nTime-based features added:\n")
time_features <- c("week_num", "year", "month", "quarter", "day_of_year", "week_of_year", 
                   "month_sin", "month_cos", "week_sin", "week_cos", 
                   "week_num_squared", "log_week_num")
for(feat in time_features) {
  if(feat %in% colnames(X)) {
    cat(sprintf("\n%s summary:\n", feat))
    print(summary(X[,feat]))
  }
}

# Save results
save(X, y, file = "tesla_weekly_prediction_data.RData")

# Print summary
cat("\nDataset Summary:\n")
cat("Date range:", as.character(range(final_data_clean$date)), "\n")
cat("Number of weeks:", length(y), "\n")
cat("Number of predictors:", ncol(X), "\n")
cat("\nFirst few predictor names:\n")
print(head(colnames(X), 10))
cat("\nDimensions of X:", dim(X)[1], "x", dim(X)[2], "\n")
cat("\nMissing values in X:", sum(is.na(X)), "\n")
cat("Missing values in y:", sum(is.na(y)), "\n")

# Show basic statistics
cat("\nSummary of response variable (y):\n")
print(summary(y))

# Verify saved data
load("tesla_weekly_prediction_data.RData")
cat("\nVerifying saved data:\n")
cat("X dimensions:", dim(X)[1], "x", dim(X)[2], "\n")
cat("y length:", length(y), "\n")

# Optional: Show correlation with target
cat("\nTop 10 correlations with target:\n")
correlations <- cor(X, y)
top_correlations <- head(sort(abs(correlations), decreasing = TRUE), 10)
print(top_correlations)

# Check the structure of X and y
dim(X)
length(y)

n <- nrow(X)  # number of observations
m <- ncol(X)   # number of predictors

# Print dimensions and data range
print("Data Summary:")
print(paste("Date range:", min(final_data_clean$date), "to", max(final_data_clean$date)))
print(paste("Number of weeks (rows):", n))
print(paste("Number of predictors (columns):", m))
print(paste("Number of base predictors:", length(cols_to_lag)))
print(paste("Number of lags per predictor:", n_lags))


# Verify dimensions match
stopifnot(length(y) == n)
head(colnames(X), 10)

# Give names to X columns (X1, X2, ..., Xm)
colnames(X) <- paste0("X", 1:m)

K = 10; 
initial_size <- floor(0.6 * n)
step_size = floor((n - initial_size)/K) ;

pca_mse <- matrix(0, nrow = m, ncol = K)
plsr_mse <- matrix(0, nrow = m, ncol = K)
ols_mse <- numeric(K)
stepwise_mse <- numeric(K)
stepwise_p <- numeric(K) # Number of selected predictors (not intercept)
 
# K Rolling-Forward Splits
for(i in 1:K) { 
  
  # Split into training and test data for split i
  print(paste("######################## Training Size = ", initial_size + step_size*i - 1))
  # Get training data
  
  train_idx <- 1:(initial_size + step_size*i - 1)
  # test_idx <- (initial_size + step_size*i) : m
  
  X_train <- X[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, , drop = FALSE]
  y_test <- y[-train_idx]
  
  # Full OLS Regression
  print('OLS: ')
  lm_fit <- lm(y_train ~ ., data = as.data.frame(X_train))
  yhat  <- predict(lm_fit,as.data.frame(X_test))
  print(ols_mse[i] <- mean((y_test - yhat)^2))
  print(' ------------------------------------ ')
  
  # Stepwise Regression 
  print("Stepwise: ")
  stepwise_model <- stepAIC(lm_fit, direction = "both", trace = TRUE)
  stepwise_coefs <- coef(stepwise_model)
  print(stepwise_p[i] <- length(names(stepwise_coefs)) - 1)  
  print(stepwise_mse[i] <- mean((y_test - predict(stepwise_model,as.data.frame(X_test)))^2))
  print(' ------------------------------------ ')
  
  # PCA Regression
  print("Best PCA: ")
  pcr_model <- pcr(y_train ~ X_train, ncomp = m, scale = TRUE, validation = "none")
  pca_mse[,i] <- colMeans((y_test - predict(pcr_model, X_test, ncomp = 1:m))^2)
  print(which.min(pca_mse[,i]))
  print(min(pca_mse[,i]))
  print(' ------------------------------------ ')
  
  # PLS Regression
  print('Best PLS: ')
  plsr_model <- plsr(y_train ~ X_train, ncomp = m, scale = TRUE, validation = "none")
  plsr_mse[,i] <- colMeans((y_test - predict(plsr_model, X_test, ncomp = 1:m))^2)
  print(which.min(plsr_mse[,i]))
  print(min(plsr_mse[,i]))
  print(' ------------------------------------ ')
}

# Visualize ###################################################################

# Combine the PLSR and PCAR MSE values into a matrix 
mse_matrix <- cbind(rowMeans(plsr_mse), rowMeans(pca_mse)) 

# Use matplot to plot the multiple MSE series
matplot(1:m, mse_matrix, type = "b", pch = 1:2, col = c("blue", "red"), lty = 1:2,
        xlab = "Number of Components/Predictors", ylab = "Mean Squared Error (MSE)",
        main = "OOS MSE for OLS, PLSR, PCAR and Stepwise Regression with Financial Data")

# One point for the average performance of OLS performance 
points(m, mean(ols_mse), col = "black", pch = 5, cex = 1.5)

# One point for the average performance of Stepwise regression 
points(mean(stepwise_p), mean(stepwise_mse), col = "darkgreen", pch = 8, cex = 1.5)

# Add a legend
legend("topright", legend = c("PLSR", "PCAR", "OLS", "Stepwise"),
       col = c("blue", "red",  "black", "darkgreen"), pch = c(1, 2, 5, 8), lty = c(1, 2, NA, NA))

# Summarize ####################################################################
print (data.frame(
  Method = c("Full OLS", "Stepwise","PCAR", "PLSR"),
  Mean_MSE = c(mean(ols_mse), mean(stepwise_mse), min(rowMeans(pca_mse)), min(rowMeans(plsr_mse))), 
  Components = c(m, mean(stepwise_p), which.min(rowMeans(pca_mse)), which.min(rowMeans(plsr_mse)))
  # For Stepwise Regression: Components is the average number of selected predictors in K splits.
))