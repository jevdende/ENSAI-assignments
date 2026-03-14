## ------------------------------------------------------------
## Simulation code for "Modern multiple imputation with functional data"
## Rao & Reimherr (2021) - functional covariate, sparse observation, linear/nonlinear response
## Using fields package for Matérn covariance
## ------------------------------------------------------------
library(fields)

#' Matérn covariance function using fields package
#' Uses modified Bessel function of the second kind for general nu
matern_cov <- function(t, rho = 0.5, sigma2 = 1, nu = 2.5) {
  # t: vector of time points
  # rho: range parameter
  # sigma2: marginal variance (sill)
  # nu: smoothness parameter (default 2.5 = 5/2)
  r <- abs(outer(t, t, "-"))
  sigma2 * fields::Matern(r, range = rho, smoothness = nu)
}

#' Trapezoidal integration on an equally spaced grid
trapz <- function(x, t) {
  # x: vector values on grid t
  sum((x[-1] + x[-length(x)]) * diff(t) / 2)
}

#' Simulate functional data + sparse noisy observations + response
#'
#' @param n sample size
#' @param m number of grid points in [0,1]
#' @param s proportion of time points to set as missing PER CURVE (e.g., 0.5)
#' @param rho Matérn range parameter (paper: 0.5)
#' @param sigma2 GP marginal variance (paper: 1)
#' @param sigma_delta2 measurement error variance (paper: 0.3)
#' @param alpha intercept (paper: 0)
#' @param sigma_eps2 error variance for Gaussian response (paper: 1)
#' @param w signal weight in beta(t)=w*sin(2*pi*t) (paper: not specified, keep as argument)
#' @param response one of: "gaussian", "binary"
#' @param model one of: "linear", "nonlinear"
#' @param include_latent_eps_in_logit if TRUE, add N(0,sigma_eps2) noise inside logit (NOT stated in paper);
#'                                  default FALSE uses standard Bernoulli-logit with linear predictor only.
simulate_fda_mi_paper <- function(
    n = 500, m = 52, s = 0.5,
    rho = 0.5, sigma2 = 1,
    sigma_delta2 = 0.3,
    alpha = 0, sigma_eps2 = 1,
    w = 1,
    response = c("gaussian", "binary"),
    model = c("linear", "nonlinear"),
    include_latent_eps_in_logit = FALSE
) {
  response <- match.arg(response)
  model <- match.arg(model)
  
  # Grid in [0,1]
  tgrid <- seq(0, 1, length.out = m)
  
  # GP covariance matrix using Matérn with nu = 2.5 (5/2)
  Sigma <- matern_cov(tgrid, rho = rho, sigma2 = sigma2, nu = 2.5)
  # Small jitter for numerical stability
  L <- chol(Sigma + 1e-10 * diag(m))
  
  # Simulate latent curves X: n x m
  Z <- matrix(rnorm(n * m), nrow = n, ncol = m)
  X_true <- Z %*% L  # mean 0 GP
  
  # Add measurement error to get dense noisy measurements
  X_noisy <- X_true + matrix(rnorm(n * m, sd = sqrt(sigma_delta2)), nrow = n, ncol = m)
  
  # Define linear predictor eta_i depending on model
  if (model == "linear") {
    beta <- w * sin(2 * pi * tgrid)  # beta(t) = w sin(2pi t)
    # Approximate integral ∫ X_i(t) beta(t) dt by trapezoid on grid
    eta <- apply(X_true * matrix(beta, nrow = n, ncol = m, byrow = TRUE), 1, trapz, t = tgrid)
  } else {
    # Nonlinear case: f(X(t),t) = 5 * sin(X(t)^2 * t^2)
    # Paper states "All simulation parameters are the same as before, except response computed using Eq (2)"
    # and f defined as above.
    fmat <- 5 * sin((X_true^2) * matrix(tgrid^2, nrow = n, ncol = m, byrow = TRUE))
    eta <- apply(fmat, 1, trapz, t = tgrid)
  }
  
  # Generate response
  if (response == "gaussian") {
    eps <- rnorm(n, mean = 0, sd = sqrt(sigma_eps2))
    Y <- alpha + eta + eps
  } else {
    if (include_latent_eps_in_logit) {
      eps <- rnorm(n, mean = 0, sd = sqrt(sigma_eps2))
      linpred <- alpha + eta + eps
    } else {
      linpred <- alpha + eta
    }
    p <- 1 / (1 + exp(-linpred))
    Y <- rbinom(n, size = 1, prob = p)
  }
  
  # Impose sparsity: per curve, set s proportion of time points as unobserved
  # Ensure at least 2 points remain observed for each curve (can adjust if needed)
  missing_mask <- matrix(FALSE, nrow = n, ncol = m) # TRUE = missing
  n_miss <- floor(s * m)
  n_keep_min <- 2
  if (m - n_miss < n_keep_min) {
    stop("s too high: not enough observed points per curve. Reduce s or increase m.")
  }
  
  for (i in seq_len(n)) {
    miss_idx <- sample.int(m, size = n_miss, replace = FALSE)
    missing_mask[i, miss_idx] <- TRUE
  }
  
  X_obs <- X_noisy
  X_obs[missing_mask] <- NA_real_
  
  # Return both functional objects and a "long" representation often used in FDA
  long_df <- do.call(
    rbind,
    lapply(seq_len(n), function(i) {
      data.frame(
        id = i,
        t = tgrid[!missing_mask[i, ]],
        x = X_obs[i, !missing_mask[i, ]],
        y = Y[i]
      )
    })
  )
  
  list(
    tgrid = tgrid,
    X_true = X_true,       # latent GP curves (dense)
    X_noisy = X_noisy,     # latent + measurement error (dense)
    X_obs = X_obs,         # sparse observed (NA for missing)
    missing_mask = missing_mask,
    Y = Y,
    eta = alpha + eta,     # linear predictor excluding eps for Gaussian case
    settings = list(
      n = n, m = m, s = s,
      rho = rho, nu = 2.5, sigma2 = sigma2,
      sigma_delta2 = sigma_delta2,
      alpha = alpha, sigma_eps2 = sigma_eps2,
      w = w,
      response = response, model = model,
      include_latent_eps_in_logit = include_latent_eps_in_logit
    ),
    long = long_df
  )
}
