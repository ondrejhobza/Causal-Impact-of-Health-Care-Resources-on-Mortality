# Scripts

This folder contains the final R scripts used for empirical analysis in the bachelor thesis.
The scripts focus on panel-data estimation of health-system inputs (e.g., physicians and health expenditure) and mortality outcomes.

## Folder Contents

- `Tests.R`  
  Data preparation, sample filtering, panel diagnostics, missingness checks, and export of filtered analysis data.

- `TWFE.R`  
  Main first-difference two-way fixed effects (FD-TWFE) model specifications and related output/visualization steps.

- `DML.R`  
  Double/Debiased Machine Learning (DML) estimation using multiple learners (lasso, elastic net, random forest, neural network) and result comparison.

## Recommended Execution Order

1. Run `Tests.R` to prepare and validate the analysis sample.
2. Run `TWFE.R` for baseline FD-TWFE estimations.
3. Run `DML.R` for DML estimations and robustness comparison.

## Notes

- Before running on another machine, update `setwd(...)` to match your local thesis project path.
