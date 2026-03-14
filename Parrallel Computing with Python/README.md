# Parrallel Computing with Python

## Assignment Summary

This course folder contains two labs focused on practical parallel and distributed computation in Python.

## Lab 1: Multiprocessing Basics

### What It Does

- Implements sequential and parallel CPU-bound tasks.
- Uses Python multiprocessing pools to split workloads across cores.
- Benchmarks speedup for Monte Carlo-style simulations and numerical tasks.

### Main Files

- `Lab1/monte_carlo_dice_par.py`
- `Lab1/greyscale_seq.py`
- `Lab1/greyscale_seq_1d.py`
- `Lab1/parallel_regression.py`

## Lab 2: Dask For Scalable Data Processing

### What It Does

- Generates synthetic boat datasets.
- Compares Pandas workflows with Dask-based processing.
- Explores scaling behavior with increasing data volumes.

### Main Files

- `Lab2/dask_gen_small_data.ipynb`
- `Lab2/dask_process_small_data.ipynb`
- `Lab2/dask_gen_data_large.ipynb`
- `Lab2/dask_process_data_large.ipynb`
- `Lab2/hands-on-dask.ipynb`
- `Lab2/boats_small*.csv`

## Typical Output

- Runtime comparisons between sequential and parallel/distributed approaches.
- Practical understanding of when parallel overhead is beneficial.
- Processed data artifacts and notebook-based analysis outputs.
