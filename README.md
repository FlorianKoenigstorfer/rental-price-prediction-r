# Rental Price Prediction in R — Course Teaching Material

Annotated R scripts that walk students through a complete data science workflow — exploratory data analysis, data cleaning, and linear-regression modelling — using German rental-apartment data.

These are teaching materials I created and used as the instructor for the course **"Data Driven Decision Support 2 – Business Analytics"** at the University of Graz (Karl-Franzens-Universität Graz). They are shared here as a teaching reference, not as a personal research or modelling project: the code is deliberately simple and heavily commented so that students new to R and to data science can follow each step.

---

## What this is (and isn't)

- **It is** the instructor-facing R scripts for a hands-on, introductory course unit. Each script accompanies a lecture slide deck and demonstrates one stage of the data science workflow on a single worked example (the city of Mittelsachsen_Kreis).
- **It is not** a polished end-to-end modelling project. The methods are kept at an introductory level — single and multivariate OLS, 3-fold cross-validation, basic diagnostics — because the goal is teaching the workflow, not maximising predictive performance.

The commentary addresses students directly ("I will show you how to do it…", "so we all have the same datasets"). This is the original teaching voice and has been kept on purpose. Two assumption checks in script 03 are also annotated with `# Caveat:` notes that flag their limitations for students.

---

## Course context

- **Course:** Data Driven Decision Support 2 – Business Analytics
- **Institution:** University of Graz (Karl-Franzens-Universität Graz)
- **Role:** Instructor
- **Workflow taught:** exploratory data analysis → data cleaning → regression and prediction

**Data:** The analysis uses `Dataset_Teachers.csv`, the course dataset. It contains German rental-apartment listings originally scraped from the real-estate platform Immoscout24 (2019–2020); each row is one listing, with attributes such as rent, living space, number of rooms, heating type, construction year, and location.

---

## Repository structure

```
├── slides/
│   ├── 01_data_visualization.pdf
│   ├── 02_data_cleaning_and_management.pdf
│   └── 03_regression_analysis.pdf
├── code/
│   ├── 01_exploratory_data_analysis.R
│   ├── 02_data_cleaning.R
│   └── 03_regressions_and_predictions.R
├── data/
│   └── dataset_teachers.zip
├── .gitignore
├── install_packages.R
└── README.md
```

| Directory / File | Description |
|---|---|
| `slides/` | Lecture slide decks (PDF) accompanying each script |
| `code/` | R scripts, numbered in execution order |
| `data/` | Zipped course dataset (`Dataset_Teachers.csv` inside) |
| `install_packages.R` | One-time script to install all required R packages |

---

## Getting started

### Prerequisites

- [R](https://cran.r-project.org/) (≥ 4.0)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/FlorianKoenigstorfer/rental-price-prediction-r.git
   cd rental-price-prediction-r
   ```

2. Install the required packages:
   ```r
   source("install_packages.R")
   ```

3. Unzip the dataset so that `data/Dataset_Teachers.csv` is available:
   ```bash
   unzip data/dataset_teachers.zip -d data/
   ```

4. Run the scripts in order, from the project root:
   ```r
   source("code/01_exploratory_data_analysis.R")
   source("code/02_data_cleaning.R")
   source("code/03_regressions_and_predictions.R")
   ```

Script 02 writes the cleaned dataset to `output/Cleaned_Dataset_Mittelsachsen_Kreis.csv`, which script 03 then reads — so the scripts must be run in order. If the `output/` folder does not exist, create it first (`dir.create("output")`).

---

## The scripts

### `01_exploratory_data_analysis.R`

Accompanies `slides/01_data_visualization.pdf`. Introduces visual data exploration on the raw dataset: histograms and box plots for `totalRent` (single and grouped), a pie chart of `heatingType`, scatter plots of `livingSpace` vs. `totalRent`, a pairwise scatter-plot matrix via `pairs()`, and faceted histograms of `totalRent` by heating type.

### `02_data_cleaning.R`

Accompanies `slides/02_data_cleaning_and_management.pdf`. Cleans the raw dataset in five steps: redundant data (variable selection, duplicate removal), missing values (standardising `"no_information"` to `NA`, dropping incomplete rows), outliers (z-score and IQR methods), inconsistencies (filtering rows where `totalRent ≠ baseRent + serviceCharge`), and feature engineering (recoding `yearConstructed` into building-type categories, consolidating rare districts in `regio3`). Exports the cleaned dataset to `output/Cleaned_Dataset_Mittelsachsen_Kreis.csv`.

### `03_regressions_and_predictions.R`

Accompanies `slides/03_regression_analysis.pdf`. Covers the modelling stage: an 80/20 train-test split, correlation analysis, three OLS models with `stargazer` output, prediction with MAPE and Sales-Ratio evaluation, 3-fold cross-validation, OLS assumption checks (linearity, multicollinearity via VIF, homoscedasticity, normality of residuals), a robustness comparison of models trained on clean vs. raw data, and influence diagnostics (Cook's distance, leverage).

---

## Dependencies

Install everything in one step via `install_packages.R`. The individual packages used:

| Package | Purpose |
|---|---|
| `tidyverse` | Data manipulation and visualisation |
| `caTools` | Train/test splitting |
| `caret` | k-fold cross-validation |
| `stargazer` | Formatted regression-output tables |
| `car` | Regression diagnostics (VIF, outlier tests) |
| `MASS` | Supporting functions used in the diagnostics section of script 03 |
