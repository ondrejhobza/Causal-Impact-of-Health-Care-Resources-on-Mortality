# Thesis Scripts (Final)

This folder contains the final R scripts used for empirical analysis in the bachelor thesis.
The scripts focus on panel-data estimation of health-system inputs (e.g., doctors and health expenditure) and mortality outcomes.

## Folder Contents

- `Tests.R`  
  Data preparation, sample filtering, panel diagnostics, missingness checks, and export of filtered analysis data.

- `TWFE.R`  
  Main first-difference two-way fixed effects (FD-TWFE) model specifications and related output/visualization steps.

- `DML.R`  
  Double Machine Learning (DML) estimation using multiple learners (lasso, elastic net, random forest, neural network) and result comparison.

## Required R Packages

Install these packages before running the scripts:

- `dplyr`
- `fixest`
- `ggplot2`
- `ggrepel`
- `lmtest`
- `mlr3`
- `mlr3learners`
- `mlr3torch`
- `patchwork`
- `plm`
- `readr`
- `sandwich`
- `scales`
- `tidyr`
- `xtdml`

## Expected Project Structure

The scripts assume they are run from a thesis project root like:

- `Data/Final Data/Final_Thesis_Data_2000_2021.csv`
- `Data/Final Data/Filtered_Thesis_Data.csv` (created by `Tests.R`)
- `Output/Final/...` (created/used by scripts for saved tables and figures)

## Recommended Execution Order

1. Run `Tests.R` to prepare and validate the analysis sample.
2. Run `TWFE.R` for baseline FD-TWFE estimations.
3. Run `DML.R` for DML estimations and robustness comparison.

## Notes

- Scripts use project-relative path detection (`.` or `..`) with no hardcoded personal directories.
- You can run scripts either from thesis root or from `Scripts Final`.
