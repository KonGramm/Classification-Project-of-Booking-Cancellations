# Hotel Booking Cancellation Classification

A comparison of six classification algorithms for predicting whether a hotel booking will be **cancelled or not**, based on a dataset of 2,000 bookings with 17 variables (guest counts, stay length, meal/room type, lead time, price, special requests, etc.). Completed as the first project for the *Statistical Machine Learning* course, MSc in Statistics, Athens University of Economics and Business (AUEB).

## Repository Contents

| File | Description |
|---|---|
| `project_data.xlsx` | Raw dataset — 2,000 hotel bookings, 17 variables including the target `booking.status`. |
| `Classification_Project_Code.R` | Full R script: data cleaning, exploratory analysis, variable selection, and model fitting/evaluation. |
| `Classification_Project_Code_In_Txt.txt` | Plain-text copy of the same R script (for easy viewing without an R environment). |
| `Classification_Project_Report.pdf` | Written report with narrative interpretation, figures, and result tables. |

## Dataset

17 variables covering booking details (adults, children, weekend/week nights, meal type, room type, market segment, lead time, average price, special requests, prior cancellation history) with the binary target `booking.status` (canceled / not canceled). No missing values were found; rare factor levels in `market.segment.type`, `room.type`, and `type.of.meal` were grouped into an `Other` category to avoid train/test split issues.

## Analysis Overview

- **Exploratory analysis**: distributions of categorical/numeric variables, cancellation rate (~34%), Pearson correlation matrix, and pairwise comparisons (boxplots, stacked bar plots) against booking status.
- **Variable selection**: repeated (100x) Lasso regression to identify consistently uninformative predictors (`P.C`, `P.not.C` shrunk to zero in ~99% of runs).
- **Models fitted** (each evaluated over 100 random 80/20 train-test splits, unless noted):
  1. **Logistic Regression** — mean test accuracy ≈ 0.79, AUC ≈ 0.85
  2. **Decision Tree** (Gini index) — mean test accuracy ≈ 0.81
  3. **Random Forest** (500 trees, mtry = 3, 30 iterations) — mean test accuracy ≈ 0.84 (best model)
  4. **Naïve Bayes Classifier** (with variable selection) — mean test accuracy ≈ 0.76
  5. **Linear Discriminant Analysis (LDA)** — mean test accuracy ≈ 0.78
  6. **Support Vector Machine (SVM)**, radial kernel, grid-searched cost/gamma — mean test accuracy ≈ 0.79

## Key Findings

- **Random Forest** was the best-performing model overall (mean test accuracy ≈ 0.84), correctly classifying the largest proportion of bookings.
- **Lead time** is consistently the most important predictor across models — longer lead times are associated with higher cancellation likelihood (Pearson r ≈ 0.41).
- **Number of special requests** is negatively correlated with cancellation (r ≈ -0.25) — bookings with 3+ special requests were almost never cancelled.
- Prior cancellation/non-cancellation counts (`P.C`, `P.not.C`) and number of children contributed little predictive value and were dropped from several models.

## Requirements

Built with R, using packages including:

```r
glmnet, tree, randomForest, e1071, caTools, MASS, pROC, ggplot2, dplyr
```

## Usage

1. Clone the repository.
2. Open `Classification_Project_Code.R` in R or RStudio.
3. Update the data import path to point to your local copy of `project_data.xlsx`.
4. Run the script to reproduce the exploratory analysis, variable selection, and all six classification models with their evaluation metrics.

## Author

Konstantinos Grammenos — MSc in Statistics, AUEB
Supervisor: Prof. D. Karlis
