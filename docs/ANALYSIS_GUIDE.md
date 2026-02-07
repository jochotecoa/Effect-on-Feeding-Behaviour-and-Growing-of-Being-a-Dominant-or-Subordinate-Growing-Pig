# Analysis Guide

This guide explains how to reproduce the analysis for the "Effect on Feeding Behaviour" project.

## Prerequisites
- **R** (latest version recommended)
- **Tidyverse** package (`install.packages("tidyverse")`)

## Step 1: Data Preparation
The analysis starts with `r/load_data.R`. This script performs the following:
1.  **Reads Raw Data**: Imports `Registros_animales_Juan.txt` (registry) and `Pesos_Juan.txt` (weights).
2.  **Weight Interpolation**: Since weights are not taken daily, the script uses linear interpolation (`approx`) to estimate daily weights for every animal.
3.  **Merging**: Combines the daily weight estimates with daily consumption data (`Output_consumo_from_script_comparar_pesos.txt`).
4.  **Scaling**: Calculates consumption per kg of metabolic weight if needed.
5.  **Grouping**: Assigns time periods (initial, first, second phase) based on dates.

## Step 2: Consumption Modeling
The main statistical modeling happens in `r/ranking_diff_consumo.R`:
1.  **Model Definition**: A linear model (`lm`) is fitted to the **Control** group data:
    ```r
    lm(Co ~ Pe_app + edad*Pe_app)
    ```
    - `Co`: Consumption
    - `Pe_app`: Approximate Weight
    - `edad`: Age
2.  **Residual Calculation**: The model predicts expected consumption based on weight and age. The difference between actual and predicted consumption is the **Residual Consumption (`Co_rsdl`)**.
    - Positive residual = Ate more than expected (inefficient/high appetite).
    - Negative residual = Ate less than expected (efficient/low appetite).
3.  **Grouping**: Median residuals are calculated per animal to classify them.

## Step 3: Hierarchy Analysis
Scripts in `r/hierarchy/` (if applicable) or sections of the main script use these residuals to correlate feeding efficiency with social rank (Dominant vs. Subordinate).

## Outputs
Results are typically saved to the `output/` directory, organized by the specific analysis run (e.g., `output/ranking_diff_consumo_without_modelling`).
