# ============================================================
# Analyze simulation data across imputation methods
# ============================================================
library(refund)

source("simulation.R")
source("pace.R")
source("missforest.R")
source("mice.R")

# Set seed for reproducibility
set.seed(111)

# Simulation parameters
n <- 500
m <- 52
sparsity_rate <- 0.75
w <- 1.0

# Run simulation
cat("Running simulation...\n")
sim_data <- simulate_fda_mi_paper(
  n = n,
  m = m,
  s = sparsity_rate,
  w = w,
  response = "gaussian",
  model = "linear"
)

# Create exploratory plots
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Sample latent curves
plot(sim_data$tgrid, sim_data$X_true[1, ], type = "l", col = "blue",
    xlab = "t", ylab = "X(t)", main = "Sample Latent Curves",
    ylim = range(sim_data$X_true[1:5, ]))
for (i in 2:5) lines(sim_data$tgrid, sim_data$X_true[i, ], col = "blue")

# 2. Observed vs latent for one curve
i_sample <- 1
plot(sim_data$tgrid, sim_data$X_true[i_sample, ], type = "l", lwd = 2,
    xlab = "t", ylab = "X(t)", main = "Curve 1: True vs Observed",
    ylim = range(c(sim_data$X_true[i_sample, ], sim_data$X_obs[i_sample, ]), na.rm = TRUE))
obs_idx <- which(!is.na(sim_data$X_obs[i_sample, ]))
points(sim_data$tgrid[obs_idx], sim_data$X_obs[i_sample, obs_idx], 
      col = "red", pch = 16)
legend("topright", legend = c("True", "Observed"), 
      lty = c(1, NA), pch = c(NA, 16), col = c("black", "red"), cex = 0.8)

# 3. Beta function
beta_true <- w * sin(2 * pi * sim_data$tgrid)
plot(sim_data$tgrid, beta_true, type = "l", lwd = 2, col = "black",
    xlab = "t", ylab = expression(beta(t)), main = "True Beta Function")
grid()

# 4. Y distribution
hist(sim_data$Y, breaks = 30, col = rgb(0, 0, 1, 0.3), border = "blue",
    xlab = "Y", main = "Response Distribution", probability = TRUE)
curve(dnorm(x, mean = mean(sim_data$Y), sd = sd(sim_data$Y)), 
      add = TRUE, col = "blue", lwd = 2)

par(mfrow = c(1, 1))

par(mfrow = c(1, 1))

# ============================================================
# Helper functions
# ============================================================

# Calculate reconstruction error
calc_rmse <- function(X_true, X_imputed, missing_mask) {
  errors <- (X_true - X_imputed)[missing_mask]
  sqrt(mean(errors^2))
}

# Estimate beta from imputed data using functional linear regression
estimate_beta <- function(X_imputed, Y, t_grid, w_true) {
  # Use refund's pfr for scalar-on-function regression
  fit <- pfr(Y ~ fpc(X_imputed))
  #fit <- pfr(Y ~ lf(X_imputed, bs = "ps", k=20))
  #fit<- pfr(Y ~ fpc(X_imputed))
  
  # Extract beta(t)
  beta_coef <- coef(fit)
  beta_hat <- beta_coef$value
  
  # Predict Y
  Y_pred <- predict(fit)
  
  list(
    beta_hat = beta_hat,
    rmse_beta = sqrt(mean((beta_hat - w_true * sin(2 * pi * t_grid))^2)),
    Y_pred = Y_pred,
    rmse_Y = sqrt(mean((Y_pred - Y)^2))
  )
} 

# ============================================================
# Method 1: PACE imputation
# ============================================================

cat("\n=== PACE IMPUTATION ===\n")

X_pace <- pace_imputation(
  X_incomplete = sim_data$X_obs,
  t_grid = sim_data$tgrid,
  K = 5
)

rmse_impute_pace <- calc_rmse(sim_data$X_noisy, X_pace, sim_data$missing_mask)
cat("Imputation RMSE:", round(rmse_impute_pace, 4), "\n")

result_pace <- estimate_beta(X_pace, sim_data$Y, sim_data$tgrid, w)
cat("Beta estimation RMSE:", round(result_pace$rmse_beta, 4), "\n")
cat("Y prediction RMSE:", round(result_pace$rmse_Y, 4), "\n")

# ============================================================
# Method 2: MissForest imputation
# ============================================================

cat("\n=== MissForest IMPUTATION ===\n")

X_mf <- missforest_imputation(
  X_incomplete = sim_data$X_obs,
  t_grid = sim_data$tgrid,
  ntree = 100,
  maxiter = 10
)

rmse_impute_mf <- calc_rmse(sim_data$X_noisy, X_mf, sim_data$missing_mask)
cat("Imputation RMSE:", round(rmse_impute_mf, 4), "\n")

result_mf <- estimate_beta(X_mf, sim_data$Y, sim_data$tgrid, w)
cat("Beta estimation RMSE:", round(result_mf$rmse_beta, 4), "\n")
cat("Y prediction RMSE:", round(result_mf$rmse_Y, 4), "\n")

# ============================================================
# Method 3: MICE imputation
# ============================================================

cat("\n=== MICE IMPUTATION ===\n")

X_mice <- mice_imputation(
  X_incomplete = sim_data$X_obs,
  t_grid = sim_data$tgrid,
  m_imp = 5,
  maxit = 15
)

rmse_impute_mice <- calc_rmse(sim_data$X_noisy, X_mice, sim_data$missing_mask)
cat("Imputation RMSE:", round(rmse_impute_mice, 4), "\n")

result_mice <- estimate_beta(X_mice, sim_data$Y, sim_data$tgrid, w)
cat("Beta estimation RMSE:", round(result_mice$rmse_beta, 4), "\n")
cat("Y prediction RMSE:", round(result_mice$rmse_Y, 4), "\n")

# ============================================================
# Method 4: MissForest with binning
# ============================================================

cat("\n=== MissForest IMPUTATION (BINNED) ===\n")

X_mf_binned <- missforest_imputation_binned(
  X_incomplete = sim_data$X_obs,
  t_grid = sim_data$tgrid,
  n_bins = 10,
  ntree = 100,
  maxiter = 10
)

rmse_impute_mf_binned <- calc_rmse(sim_data$X_noisy, X_mf_binned, sim_data$missing_mask)
cat("Imputation RMSE:", round(rmse_impute_mf_binned, 4), "\n")

result_mf_binned <- estimate_beta(X_mf_binned, sim_data$Y, sim_data$tgrid, w)
cat("Beta estimation RMSE:", round(result_mf_binned$rmse_beta, 4), "\n")
cat("Y prediction RMSE:", round(result_mf_binned$rmse_Y, 4), "\n")

# ============================================================
# Method 5: MICE with binning
# ============================================================

cat("\n=== MICE IMPUTATION (BINNED) ===\n")

X_mice_binned <- mice_imputation_binned(
  X_incomplete = sim_data$X_obs,
  t_grid = sim_data$tgrid,
  n_bins = 10,
  m_imp = 5,
  maxit = 15
)

rmse_impute_mice_binned <- calc_rmse(sim_data$X_noisy, X_mice_binned, sim_data$missing_mask)
cat("Imputation RMSE:", round(rmse_impute_mice_binned, 4), "\n")

result_mice_binned <- estimate_beta(X_mice_binned, sim_data$Y, sim_data$tgrid, w)
cat("Beta estimation RMSE:", round(result_mice_binned$rmse_beta, 4), "\n")
cat("Y prediction RMSE:", round(result_mice_binned$rmse_Y, 4), "\n")

# ============================================================
# Plot PACE results
# ============================================================

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# PACE: Imputed curve vs true
i_plot <- 1
plot(sim_data$tgrid, sim_data$X_true[i_plot, ], type = "l", lwd = 2, col = "black",
    xlab = "t", ylab = "X(t)", main = "PACE Imputation: Curve 1",
    ylim = range(c(sim_data$X_true[i_plot, ], sim_data$X_noisy[i_plot, ], X_pace[i_plot, ])))
lines(sim_data$tgrid, sim_data$X_noisy[i_plot, ], col = "gray", lwd = 2, lty = 1)
lines(sim_data$tgrid, X_pace[i_plot, ], col = "blue", lwd = 2, lty = 2)
obs_idx <- which(!is.na(sim_data$X_obs[i_plot, ]))
points(sim_data$tgrid[obs_idx], sim_data$X_obs[i_plot, obs_idx], 
      col = "red", pch = 16)
legend("topright", legend = c("True (no noise)", "True (with noise)", "PACE", "Observed"), 
      col = c("black", "gray", "blue", "red"), lty = c(1, 1, 2, NA), 
      pch = c(NA, NA, NA, 16), lwd = 2, cex = 0.7)
grid()

# PACE: Beta estimation
plot(sim_data$tgrid, beta_true, type = "l", lwd = 3, col = "black",
    xlab = "t", ylab = expression(beta(t)),
    main = "PACE Beta Estimation",
    ylim = range(c(beta_true, result_pace$beta_hat)))
lines(sim_data$tgrid, result_pace$beta_hat, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("True", "PACE Estimated"), 
      col = c("black", "blue"), lty = c(1, 2), lwd = c(3, 2), cex = 0.8)
grid()

par(mfrow = c(1, 1))

# ============================================================
# Plot MissForest and MissForest Binned results
# ============================================================

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# MissForest: Imputed curve vs true
plot(sim_data$tgrid, sim_data$X_true[i_plot, ], type = "l", lwd = 2, col = "black",
    xlab = "t", ylab = "X(t)", main = "MissForest Imputation: Curve 1",
    ylim = range(c(sim_data$X_true[i_plot, ], sim_data$X_noisy[i_plot, ], X_mf[i_plot, ])))
lines(sim_data$tgrid, sim_data$X_noisy[i_plot, ], col = "gray", lwd = 2, lty = 1)
lines(sim_data$tgrid, X_mf[i_plot, ], col = "darkgreen", lwd = 2, lty = 2)
obs_idx <- which(!is.na(sim_data$X_obs[i_plot, ]))
points(sim_data$tgrid[obs_idx], sim_data$X_obs[i_plot, obs_idx], 
      col = "red", pch = 16)
legend("topright", legend = c("True (no noise)", "True (with noise)", "MissForest", "Observed"), 
      col = c("black", "gray", "darkgreen", "red"), lty = c(1, 1, 2, NA), 
      pch = c(NA, NA, NA, 16), lwd = 2, cex = 0.7)
grid()

# MissForest: Beta estimation
plot(sim_data$tgrid, beta_true, type = "l", lwd = 3, col = "black",
    xlab = "t", ylab = expression(beta(t)),
    main = "MissForest Beta Estimation",
    ylim = range(c(beta_true, result_mf$beta_hat)))
lines(sim_data$tgrid, result_mf$beta_hat, col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c("True", "MissForest Estimated"), 
      col = c("black", "darkgreen"), lty = c(1, 2), lwd = c(3, 2), cex = 0.8)
grid()

# MissForest Binned: Imputed curve vs true
plot(sim_data$tgrid, sim_data$X_true[i_plot, ], type = "l", lwd = 2, col = "black",
    xlab = "t", ylab = "X(t)", main = "MissForest Binned: Curve 1",
    ylim = range(c(sim_data$X_true[i_plot, ], sim_data$X_noisy[i_plot, ], X_mf_binned[i_plot, ])))
lines(sim_data$tgrid, sim_data$X_noisy[i_plot, ], col = "gray", lwd = 2, lty = 1)
lines(sim_data$tgrid, X_mf_binned[i_plot, ], col = "orange", lwd = 2, lty = 2)
obs_idx <- which(!is.na(sim_data$X_obs[i_plot, ]))
points(sim_data$tgrid[obs_idx], sim_data$X_obs[i_plot, obs_idx], 
      col = "red", pch = 16)
legend("topright", legend = c("True (no noise)", "True (with noise)", "MF Binned", "Observed"), 
      col = c("black", "gray", "orange", "red"), lty = c(1, 1, 2, NA), 
      pch = c(NA, NA, NA, 16), lwd = 2, cex = 0.7)
grid()

# MissForest Binned: Beta estimation
plot(sim_data$tgrid, beta_true, type = "l", lwd = 3, col = "black",
    xlab = "t", ylab = expression(beta(t)),
    main = "MissForest Binned Beta Estimation",
    ylim = range(c(beta_true, result_mf_binned$beta_hat)))
lines(sim_data$tgrid, result_mf_binned$beta_hat, col = "orange", lwd = 2, lty = 2)
legend("topright", legend = c("True", "MF Binned Estimated"), 
      col = c("black", "orange"), lty = c(1, 2), lwd = c(3, 2), cex = 0.8)
grid()

par(mfrow = c(1, 1))

# ============================================================
# Plot MICE and MICE Binned results
# ============================================================

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# MICE: Imputed curve vs true
plot(sim_data$tgrid, sim_data$X_true[i_plot, ], type = "l", lwd = 2, col = "black",
    xlab = "t", ylab = "X(t)", main = "MICE Imputation: Curve 1",
    ylim = range(c(sim_data$X_true[i_plot, ], sim_data$X_noisy[i_plot, ], X_mice[i_plot, ])))
lines(sim_data$tgrid, sim_data$X_noisy[i_plot, ], col = "gray", lwd = 2, lty = 1)
lines(sim_data$tgrid, X_mice[i_plot, ], col = "purple", lwd = 2, lty = 2)
obs_idx <- which(!is.na(sim_data$X_obs[i_plot, ]))
points(sim_data$tgrid[obs_idx], sim_data$X_obs[i_plot, obs_idx], 
      col = "red", pch = 16)
legend("topright", legend = c("True (no noise)", "True (with noise)", "MICE", "Observed"), 
      col = c("black", "gray", "purple", "red"), lty = c(1, 1, 2, NA), 
      pch = c(NA, NA, NA, 16), lwd = 2, cex = 0.7)
grid()

# MICE: Beta estimation
plot(sim_data$tgrid, beta_true, type = "l", lwd = 3, col = "black",
    xlab = "t", ylab = expression(beta(t)),
    main = "MICE Beta Estimation",
    ylim = range(c(beta_true, result_mice$beta_hat)))
lines(sim_data$tgrid, result_mice$beta_hat, col = "purple", lwd = 2, lty = 2)
legend("topright", legend = c("True", "MICE Estimated"), 
      col = c("black", "purple"), lty = c(1, 2), lwd = c(3, 2), cex = 0.8)
grid()

# MICE Binned: Imputed curve vs true
plot(sim_data$tgrid, sim_data$X_true[i_plot, ], type = "l", lwd = 2, col = "black",
    xlab = "t", ylab = "X(t)", main = "MICE Binned: Curve 1",
    ylim = range(c(sim_data$X_true[i_plot, ], sim_data$X_noisy[i_plot, ], X_mice_binned[i_plot, ])))
lines(sim_data$tgrid, sim_data$X_noisy[i_plot, ], col = "gray", lwd = 2, lty = 1)
lines(sim_data$tgrid, X_mice_binned[i_plot, ], col = "brown", lwd = 2, lty = 2)
obs_idx <- which(!is.na(sim_data$X_obs[i_plot, ]))
points(sim_data$tgrid[obs_idx], sim_data$X_obs[i_plot, obs_idx], 
      col = "red", pch = 16)
legend("topright", legend = c("True (no noise)", "True (with noise)", "MICE Binned", "Observed"), 
      col = c("black", "gray", "brown", "red"), lty = c(1, 1, 2, NA), 
      pch = c(NA, NA, NA, 16), lwd = 2, cex = 0.7)
grid()

# MICE Binned: Beta estimation
plot(sim_data$tgrid, beta_true, type = "l", lwd = 3, col = "black",
    xlab = "t", ylab = expression(beta(t)),
    main = "MICE Binned Beta Estimation",
    ylim = range(c(beta_true, result_mice_binned$beta_hat)))
lines(sim_data$tgrid, result_mice_binned$beta_hat, col = "brown", lwd = 2, lty = 2)
legend("topright", legend = c("True", "MICE Binned Estimated"), 
      col = c("black", "brown"), lty = c(1, 2), lwd = c(3, 2), cex = 0.8)
grid()

par(mfrow = c(1, 1))


