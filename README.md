# Effect on Feeding Behaviour and Growing of Being a Dominant or Subordinate Growing Pig

Public code and analysis for the paper: **"Effect on Feeding Behaviour and Growing of Being a Dominant or Subordinate Growing Pig and Its Relationship with the Faecal Microbiota"**.

**Published in:** [Animals (MDPI)](https://www.mdpi.com/2076-2615/14/13/1906)

## Project Overview
This repository contains the R scripts used to analyze the relationship between social hierarchy (dominance/subordination) and feeding behavior/growth in pigs. The analysis involves:
1.  **Data Loading:** Merging daily feed consumption records with interpolated weight data.
2.  **Modeling:** Using linear models to calculate residual feed consumption (correcting for weight and age).
3.  **Hierarchy Analysis:** Classifying animals and comparing performance between groups.

## Directory Structure
- **`r/`**: Core scripts for data loading, modeling, and visualization.
    - `load_data.R`: Main data loading script that prepares and merges the raw datasets.
    - `ranking_diff_consumo.R`: Main analysis entry point for consumption differences.
    - **`modeling/`**: Linear mixed models and ad-hoc modeling scripts.
    - **`visualization/`**: Plotting and diagram generation.
    - **`preprocessing/`**: Raw data cleaning and weight interpolation.
    - **`exploratory/`**: Historical and legacy analysis scripts.
- **`output/`**: Stores generated results and figures.
- **`docs/`**: Documentation and analysis guides.

## Data Requirements
To run these scripts, the following raw data files must be present in the root directory (Note: these may not be included in the repo for privacy/size reasons):
- `Registros_animales_Juan.txt`
- `Pesos_Juan.txt`
- `Output_consumo_from_script_comparar_pesos.txt`

## Reproducibility
**Important:** Always set your R working directory to the **Project Root** (where the `.Rproj` file is) before running any scripts. Do not change the working directory to subfolders (like `r/modeling`), as this will break file paths.

## Getting Started
See [docs/ANALYSIS_GUIDE.md](docs/ANALYSIS_GUIDE.md) for detailed instructions on running the analysis.