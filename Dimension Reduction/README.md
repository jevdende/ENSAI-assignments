# Dimension Reduction

## Assignment Summary

This lab studies matrix completion for movie recommendation.
The goal is to recover missing entries in a sparse user-movie rating matrix under a low-rank assumption.

## What The Work Does

- Formalizes matrix completion as a constrained low-rank optimization problem.
- Implements and compares three algorithms:
  - Singular Value Projection (SVP)
  - Singular Value Thresholding (SVT)
  - ADMiRA
- Evaluates methods on MovieLens-style data using reconstruction quality and computation time.

## Main Files

- `Dimension_reduction_lab.ipynb`: full report notebook with theory, implementation, and experiments.

## Typical Output

- Reconstructed rating matrices.
- Error curves/metrics across methods.
- Runtime comparison between low-rank solvers.
