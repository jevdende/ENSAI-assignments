# ============================================================
# MICE Imputation Method
# ============================================================
library(mice)

#' MICE imputation method (single imputation)
#' 
#' @param X_incomplete n x m matrix with missing values
#' @param t_grid time points (length m) - not used but kept for consistency
#' @param m_imp number of imputations (default: 5)
#' @param maxit maximum iterations (default: 15)
#' @param method imputation method (default: "mean")
#' @return n x m matrix with imputed values
mice_imputation <- function(X_incomplete, t_grid, m_imp = 5, maxit = 15, method = "mean") {
  
  # MICE expects a data frame
  X_df <- as.data.frame(X_incomplete)
  
  # Run MICE
  mice_result <- mice(X_df, m = m_imp, maxit = maxit, method = method, printFlag = FALSE)
  
  # Pool multiple imputations by averaging
  imputed_list <- lapply(1:m_imp, function(i) {
    as.matrix(mice::complete(mice_result, action = i))
  })
  
  # Average across all imputations
  X_imputed <- Reduce("+", imputed_list) / m_imp
  
  return(X_imputed)
}

#' MICE imputation with binning for dimension reduction
#' 
#' @param X_incomplete n x m matrix with missing values
#' @param t_grid time points (length m)
#' @param n_bins number of bins to use (default: 10)
#' @param m_imp number of imputations (default: 5)
#' @param maxit maximum iterations (default: 15)
#' @param method imputation method (default: "mean")
#' @return n x m matrix with imputed values
mice_imputation_binned <- function(X_incomplete, t_grid, n_bins = 10, m_imp = 5, maxit = 15, method = "mean") {
  
  n <- nrow(X_incomplete)
  m <- ncol(X_incomplete)
  
  # Create bins: first bin = first point, last bin = last point, middle bins divide rest equally
  bin_idx <- integer(m)
  bin_idx[1] <- 1  # First bin has only first point
  bin_idx[m] <- n_bins  # Last bin has only last point
  
  # Divide middle points (2 to m-1) into bins 2 to (n_bins-1)
  if (m > 2) {
    middle_points <- 2:(m-1)
    n_middle_bins <- n_bins - 2
    middle_bin_size <- length(middle_points) / n_middle_bins
    bin_idx[middle_points] <- ceiling(seq_len(length(middle_points)) / middle_bin_size) + 1
  }
  
  # Bin the data: average within each bin
  X_binned <- matrix(NA, nrow = n, ncol = n_bins)
  for (b in 1:n_bins) {
    bin_cols <- which(bin_idx == b)
    X_binned[, b] <- rowMeans(X_incomplete[, bin_cols, drop = FALSE], na.rm = TRUE)
    # Set to NA if all values in bin are NA
    X_binned[rowSums(!is.na(X_incomplete[, bin_cols, drop = FALSE])) == 0, b] <- NA
  }
  
  # Apply MICE to binned data
  X_binned_imputed <- mice_imputation(X_binned, seq(0, 1, length.out = n_bins), 
                                       m_imp = m_imp, maxit = maxit, method = method)
  
  # Expand back to original grid using B-spline interpolation
  X_imputed <- matrix(NA, nrow = n, ncol = m)
  bin_centers <- sapply(1:n_bins, function(b) mean(t_grid[bin_idx == b]))
  
  for (i in 1:n) {
    # Use splinefun for B-spline interpolation
    spline_fn <- splinefun(x = bin_centers, y = X_binned_imputed[i, ], method = "natural")
    X_imputed[i, ] <- spline_fn(t_grid)
  }
  
  return(X_imputed)
}
