# TPL Data Cleaning and Merging

## Description

This project focuses on cleaning and merging the main dataset from the Trust for Public Land (TPL) with additional datasets that include population, income, weather, elections, and housing prices. The main tasks include cleaning the original dataset, ensuring county names match with GeoFIPS codes, and performing various merges to create a comprehensive dataset.

## Project Details

### Data Sources

1. **Main Dataset**: Trust for Public Land (TPL) data on conservation funding votes.
2. **Population and Income Data**: CAINC1 - Income dataset from 1969 to 2022.
3. **Weather Data**: Precipitation data from the National Climatic Data Center.
4. **Local Election Results**: Candidate-level election results dataset.
5. **Presidential Election Results**: County-level presidential election returns from 2000 to 2020.
6. **Housing Prices**: Fair Market Rent (FMR) data from 1983 to 2024.

### Process

1. **Cleaning the TPL Dataset**:
   - Filter for counties only.
   - Clean and adjust county names to ensure they match with GeoFIPS codes.

2. **Merging Population and Income Data**:
   - Read and clean the income dataset.
   - Split and clean the GeoName column.
   - Select relevant columns and filter data from 1988 onwards.
   - Separate population and income data and pivot them to long format.
   - Merge with TPL dataset using matching county names and years.

3. **Merging Weather Data**:
   - Read and clean the precipitation data.
   - Assign column names and filter data from 1987 onwards.
   - Calculate average precipitation and merge with TPL dataset using GeoFIPS codes.

4. **Merging Local Election Results**:
   - Read and prepare local election results dataset.
   - (Note: This dataset contains multiple records per county which may require special handling.)

5. **Merging Presidential Election Results**:
   - Read and merge presidential election results with TPL dataset using GeoFIPS codes and years.

6. **Merging Housing Prices**:
   - Read and clean the Fair Market Rent (FMR) dataset.
   - Pivot the data to long format and merge with TPL dataset using GeoFIPS codes and years.

### Requirements

- R
- dplyr
- tidyverse
- stringr
- readr

### Installation

1. Clone the repository:
   ```sh
   git clone https://github.com/EtienneRicardez/TPL-Data-Cleaning-Merging.git
2. Navigate to the project directory:
   ```sh
   cd TPL-Data-Cleaning-Merging
3.- Install required packages in R:
   ```sh
    install.packages(c("dplyr", "tidyverse", "stringr", "readr"))
   ```

## Usage
## Installation

Run the main script to start the data processing and interpolation:
  ```sh
  source("main.R")
  ```
