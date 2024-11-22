library(caret)   # To Create Folds
library(MASS)    # For stepwise regression
library(pls)     # For PLSR
library(ggplot2) # For plots

# Generate fake data with strong multicollinearity
set.seed(1)
n <- 1000  # number of observations
m <- 100   # number of predictors

X <- matrix(rnorm(n * m), nrow = n, ncol = m)

# Make Multicollinearity:
X[, 2] <- X[, 1] + rnorm(n, sd = 1)  
X[, 3] <- X[, 1] + X[, 6] + rnorm(n, sd= 1)  
X[, 5] <- X[, 4] + rnorm(n, sd= 1)  

# Generate coefficients (intercept = 15)
true_beta <- rep(c(1.5, 0, 4, 0, -.5, 0, 0, 0, -2, 0),m/10) 
y <- 15 + X %*% true_beta + rnorm(n, sd = 1)

# Give names to X columns (X1, X2, ..., Xm)
colnames(X) <- paste0("X", 1:m)

# Create K folds
K <- 10
folds <- createFolds(y, k = K, list = TRUE)

pca_mse <- matrix(0, nrow = m, ncol = K)
plsr_mse <- matrix(0, nrow = m, ncol = K)
ols_mse <- numeric(K)
stepwise_mse <- numeric(K)
stepwise_p <- numeric(K) # Number of selected predictors (not intercept)

# Evaluate the models for each fold
for (i in 1:K) {
    # Split into training and test data for fold i
    print(paste("##################################### i=",i))
    test_indices <- folds[[i]]
    X_test <- X[test_indices, , drop = FALSE]
    y_test <- y[test_indices]
    X_train <- X[-test_indices, , drop = FALSE]
    y_train <- y[-test_indices]
    
    # Full OLS Regression
    print('OLS: ')
    lm_fit <- lm(y_train ~ ., data = as.data.frame(X_train))
    yhat  <- predict(lm_fit,as.data.frame(X_test))
    print(ols_mse[i] <- mean((y_test - yhat)^2))
    print(' ------------------------------------ ')
    
    # Stepwise Regression 
    print("Stepwise: ")
    stepwise_model <- stepAIC(lm_fit, direction = "both", trace = FALSE)
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

# Combine the PLSR and PCAR MSE values into a matrix
mse_matrix <- cbind(rowMeans(plsr_mse), rowMeans(pca_mse)) 

# Visualize ###################################################################
par(mfrow=c(1,2))

# Use matplot to plot the multiple MSE series
matplot(1:m, mse_matrix, type = "b", pch = 1:2, col = c("blue", "red"), lty = 1:2,
        xlab = "Number of Components/Predictors", ylab = "Mean Squared Error (MSE)",
        main = "OOS MSE for OLS, PLSR, PCAR and Stepwise Regression with Simulated Data")

# One point for the average performance of OLS performance 
points(m, mean(ols_mse), col = "black", pch = 5, cex = 1.5)

# One point for the average performance of Stepwise regression
points(mean(stepwise_p), mean(stepwise_mse), col = "darkgreen", pch = 8, cex = 1.5)

# Add a legend
legend("topright", legend = c("PLSR", "PCAR", "OLS", "Stepwise"),
       col = c("blue", "red",  "black", "darkgreen"), pch = c(1, 2, 5, 8), lty = c(1, 2, NA, NA))


# Zooming in to the optimal region
matplot(1:m, rowMeans(plsr_mse), type = "b", pch = 1, col = "blue", lty = 1,
        ylim = c(min(stepwise_mse), max(stepwise_mse)), 
        xlim = c(which.min(rowMeans(plsr_mse)), max(stepwise_p)),
        xlab = "Number of Components/Predictors", ylab = "Mean Squared Error (MSE)",
        main = "Zooming for PLSR and Stepwise")

# K points for the performance of Stepwise regression on each fold
points(stepwise_p, stepwise_mse, col = "darkgreen", pch = 8, cex = 1.5)

# Add a legend
legend("topleft", legend = c("PLSR", "Stepwise"),
       col = c("blue", "darkgreen"), pch = c(1, 8), lty = c(1, NA))


# Summarize ####################################################################
print (data.frame(
  Method = c("Full OLS", "Stepwise","PCAR", "PLSR"),
  Mean_MSE = c(mean(ols_mse), mean(stepwise_mse), min(rowMeans(pca_mse)), min(rowMeans(plsr_mse))), 
  Components = c(m, mean(stepwise_p), which.min(rowMeans(pca_mse)), which.min(rowMeans(plsr_mse)))
  # For Stepwise Regression: Components is the average number of selected predictors in K folds.
))
