library(Matrix)
library(glmnet)
library(GA)
library(MASS)

# Generate fictitious data for simulation######################################
set.seed(1)
n_train <- 10000    # Size of training set
n_test <- 1000      # Size of test set
m <- 100           # Number of predictors + 1 (intercept)

# Simulating design matrices for the training and test sets:
X_train <- cbind(1, matrix(rnorm(n_train * (m-1)), nrow = n_train, ncol = m-1))
X_test <- cbind(1, matrix(rnorm(n_test * (m-1)), nrow = n_test, ncol = m-1))

colnames(X_train) <- c("(Intercept)", paste0("V", 1:(ncol(X_train) - 1)))
colnames(X_test) <- c("(Intercept)", paste0("V", 1:(ncol(X_test) - 1)))

# Simulating sparse coefficients:
true_beta <- rep(c(10, -2, 4, 0, 3, 0, 0, 0, 1.5, 0),m/10)

# response values for training and test data:
y_train <- X_train %*% true_beta + rnorm(n_train)
y_test <- X_test %*% true_beta + rnorm(n_test)


#Function for Comparative Analysis between the methods ########################
calculate_mse <- function(beta, X_test, y_test) {
  # Check if X_test and beta are compatible
  if (ncol(X_test) != length(beta)) {
    stop("Dimension mismatch between X_test and beta.")
  }
  
  # Compute predictions
  y_pred <- X_test %*% beta
  
  # Calculate MSE
  mse <- mean((y_test - y_pred)^2)
  
  return(mse)
}


# Full Regression Model with All Predictors #################################
time_full <- system.time({
  full_model <- lm(y_train ~ . -1, data = as.data.frame(X_train))
})

time_full["elapsed"]

# Extract the coefficients from the full model
full_beta <- coef(full_model)

# Calculate MSE for Full Regression:
full_mse = calculate_mse(full_beta, X_test, y_test)
cat("FULL coefficients:", round(full_beta,2), "\n")
cat("FULL MSE:", full_mse, "\n")


# Stepwise Regression ##############################
time_stepwise <- system.time({
  stepwise_model <- stepAIC(lm(y_train ~ ., data = as.data.frame(X_train)), 
                            direction = "backward")
})

time_stepwise["elapsed"]

# Extract the coefficients from the stepwise model
stepwise_columns <- coef(stepwise_model)
stepwise_beta <- numeric(m)
names(stepwise_beta) <- colnames(X_train)
stepwise_beta[names(stepwise_columns)] <- stepwise_columns 

# Calculate MSE for Stepwise Regression:
stepwise_mse = calculate_mse(stepwise_beta, X_test, y_test)
cat("Stepwise coefficients:", round(stepwise_beta,2), "\n")
cat("Stepwise MSE:", stepwise_mse, "\n")


# Lasso regression using cross-validation (min OOS) ###########################

time_lasso <- system.time({
  lasso_cv <- cv.glmnet(X_train[,-1], 
                        y_train, alpha = 1, 
                        intercept = TRUE, 
                        standardize = FALSE)
})

time_lasso["elapsed"]

lasso_beta <- as.vector(coef(lasso_cv, s = "lambda.min"))

# Calculate MSE for Lasso:
lasso_mse <- calculate_mse(lasso_beta, X_test, y_test)
cat("Lasso coefficients:", round(lasso_beta,2), "\n")
cat("LASSO MSE:", lasso_mse, "\n")


# Solve BIP to minimize OOS using Genetic Algorithm ###########################

z_initial <- rep(TRUE, m) # Initialize z with all variables included

# Define the objective function for minimizing OOS error
objective_ga <- function(z) {
  z <- as.logical(z)   # Ensure z is treated as a binary vector
  
  # Subset the training and test matrices based on the selected variables
  X_z <- X_train[, z, drop = FALSE]
  T_z <- X_test[, z, drop = FALSE]
  
  # Calculate beta_z using the selected predictors
  if (ncol(X_z) == 0) return(Inf)
  beta_z <- solve(t(X_z) %*% X_z) %*% t(X_z) %*% y_train
  
  y_pred_z <- T_z %*% beta_z
  mse_z <- mean((y_test - y_pred_z)^2)
  return(-mse_z)
}

# The Genetic Algorithm

time_ga <- system.time({
  ga_model <- ga(
    type = "binary",
    fitness = objective_ga,
    nBits = m, 
    maxiter = 1000, 
    run = 50, 
    popSize = 50 
  )
})

time_ga["elapsed"]

# Get the best solution
z_optimal_ga <- as.logical(round(ga_model@solution[1,]))

# Calculate the optimal beta_z and MSE using the selected variables
X_z_ga <- X_train[, c(z_optimal_ga), drop = FALSE]  # Include intercept 
beta_z_ga <- solve(t(X_z_ga) %*% X_z_ga) %*% t(X_z_ga) %*% y_train

# Place computed values into their respective positions
ga_beta <- numeric(m)
ga_beta[z_optimal_ga] <- beta_z_ga

# Calculate MSE for GA
ga_mse <- calculate_mse(ga_beta , X_test, y_test)
cat("GA selected variables:", z_optimal_ga, "\n")
cat("GA coefficients:", round(ga_beta,2), "\n")
cat("GA MSE:", ga_mse, "\n")


# Results comparison ##########################################################
print (data.frame(
  Method = c("Full", "Stepwise","Lasso", "GA"),
  MSE = c(full_mse, stepwise_mse, lasso_mse, ga_mse),
  Elapsed = c(time_full["elapsed"], 
  			  time_stepwise["elapsed"], 
              time_lasso["elapsed"], 
              time_ga["elapsed"])
))


cbind(true_beta, 
      round(full_beta),
      round(stepwise_beta,2), 
      round(lasso_beta,2), 
      round(ga_beta,2)) 
