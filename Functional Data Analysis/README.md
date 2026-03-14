# Functional Data Analysis

## Assignment Summary

This assignment compares imputation strategies for sparse functional data.
It combines simulation and modeling to evaluate how different imputations affect downstream functional regression.

## What The Work Does

- Simulates noisy and incomplete functional trajectories.
- Applies multiple imputation methods:
  - PACE (`fdapace`-based FPCA reconstruction)
  - MissForest
  - MICE
  - Binned variants of tree and chained-equation imputations
- Compares methods with reconstruction RMSE and functional regression quality (beta estimation and response prediction).

## Main Files

- `compare_methods.R`: end-to-end comparison pipeline with diagnostics and plots.
- `pace.R`: PACE imputation routine.
- `missforest.R`: MissForest-based imputation routine.
- `mice.R`: MICE-based imputation routine.
- `simulation_fields.R`: simulation helpers for functional data generation.

## Typical Output

- Method-wise imputed curves.
- Quantitative performance metrics.
- Comparative plots of curve reconstruction and estimated coefficient functions.
