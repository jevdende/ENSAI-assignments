# ============================================================
# PACE Imputation Method
# ============================================================
library(fdapace)

#' PACE imputation method
#' 
#' @param X_incomplete n x m matrix with missing values
#' @param t_grid time points (length m)
#' @param K number of principal components to use
#' @return n x m matrix with imputed values
pace_imputation <- function(X_incomplete, t_grid, K = 5) {
  
  n <- nrow(X_incomplete)
  m <- ncol(X_incomplete)
  
  # Step 1: Convert to fdapace format (list of vectors)
  # fdapace expects:
  # - Ly: list of vectors with observed values per subject
  # - Lt: list of vectors with time points per subject
  Ly <- list()
  Lt <- list()
  
  for (i in 1:n) {
    obs_idx <- which(!is.na(X_incomplete[i, ]))
    if (length(obs_idx) > 0) {
      Ly[[i]] <- X_incomplete[i, obs_idx]
      Lt[[i]] <- t_grid[obs_idx]
    } else {
      # If all missing, provide at least one dummy observation
      Ly[[i]] <- 0
      Lt[[i]] <- mean(t_grid)
    }
  }
  
  # Step 2: Run PACE to get functional principal components
  fpca_result <- fdapace::FPCA(Ly, Lt)
  
  # Step 3: Get predicted curves
  mu_hat <- fpca_result$mu  # mean function
  phi_hat <- fpca_result$phi  # eigenfunctions (on output grid)
  xi_hat <- fpca_result$xiEst  # estimated scores
  t_out <- fpca_result$workGrid  # output time grid
  
  # Reconstruct curves on the working grid
  X_reconstructed <- matrix(mu_hat, nrow = n, ncol = length(t_out), byrow = TRUE)
  
  K_used <- ncol(xi_hat)
  for (k in 1:K_used) {
    X_reconstructed <- X_reconstructed + outer(xi_hat[, k], phi_hat[, k])
  }
  
  # Step 4: Interpolate to original grid if needed
  if (length(t_out) != m || !all(abs(t_out - t_grid) < 1e-10)) {
    X_imputed <- matrix(NA, nrow = n, ncol = m)
    for (i in 1:n) {
      X_imputed[i, ] <- approx(t_out, X_reconstructed[i, ], 
                               xout = t_grid, rule = 2)$y
    }
  } else {
    X_imputed <- X_reconstructed
  }
  
  # Step 5: Return the full PACE reconstruction (denoised + imputed)
  # PACE should smooth/denoise observed points AND fill in missing values
  return(X_imputed)
}
