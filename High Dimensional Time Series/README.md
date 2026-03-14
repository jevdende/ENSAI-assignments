# High Dimensional Time Series

## Assignment Summary

This assignment analyzes and clusters air-quality sensor time series from Taiwan AirBox data.
It includes preprocessing, seasonal adjustment, distance construction, and clustering model comparison.

## What The Work Does

- Loads sensor and location data for a month-long hourly panel.
- Performs exploratory analysis and outlier inspection.
- Applies STL-based deseasonalization to remove daily periodic patterns.
- Builds feature spaces and dissimilarities (ACF-based, GCC, CC, AR-related, and Euclidean variants).
- Compares clustering approaches (K-means and hierarchical clustering) using silhouette and CH criteria.
- Visualizes cluster assignments on a Taiwan map.

## Main Files

- `descriptive_analysis.R`: EDA, preprocessing, seasonal decomposition, and exported cleaned data.
- `clustering_analysis.R`: clustering benchmark with multiple distance definitions.
- `clustering_analysis2.R`: additional clustering experiments/variants.
- `TaiwanAirBox032017.csv`, `locations032017.csv`: source datasets.
- `plots_part1_stl/`, `plots_clustering/`: generated figures.

## Typical Output

- Cleaned and deseasonalized multivariate time-series matrix.
- Cluster labels under different methods/distances.
- Map-based and diagnostic visual comparisons.
