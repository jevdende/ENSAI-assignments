# French Gas Price Prediction with Spark

A comprehensive machine learning project analyzing and predicting gas prices across France (2022-2024) using PySpark. This project processes over 12 million records to forecast fuel prices using both Linear Regression and Random Forest models.

## 📊 Project Overview

This project demonstrates end-to-end data science workflow including:
- Large-scale data processing with Apache Spark
- Feature engineering and time series analysis
- Multiple ML model development and comparison
- Geographic visualization with interactive maps
- Statistical analysis and model interpretation

## 📈 Dataset

- **Source**: French gas price database (2022-2024)
- **Size**: 12+ million records
- **Fuel Types**: Gazole (Diesel), SP95, SP98, E10
- **Features**: Date, location (lat/lon, department), station type, fuel type, price
- **Temporal Coverage**: 156 weeks across 3 years

### Data Sources
- Gas price data: Historical daily prices from French gas stations
- Geographic data: French department boundaries (GeoJSON)
- Station information: Location and service data

## 👤 Author

**Jesse van der Ende**
- ENSAI Student
- Course: IT Tools 2 - Spark
