# Fallstudien 1 – TU Dortmund (Winter Semester 2024/25)

This repository contains five group projects completed as part of the **Fallstudien 1** course at **TU Dortmund**. Each project was conducted in a team of four students. The work included both **collaborative analysis** and **individual written reporting**, with one project (Project 2) presented during the seminar session.

## Projects Overview

### 1. Descriptive Analysis of Demographic Data (`Projekt1`)
- **Goal**: Explore and describe distributions of demographic features for the year 2022.
- **Tasks**:
  - Describe frequency distributions and compare between genders.
  - Explore bivariate relationships between features.
  - Analyze differences between subregions.
  - Compare developments between 2002 and 2022.
- **Methods**: Summary statistics, boxplots, histograms, scatter plots, grouped comparisons.

---

### 2. Comparison of Two Distributions (`Projekt2`)  
_This project was presented in class._

- **Goal**: Examine concentration test results (GU vs. UG versions) and effects of repeated testing.
- **Tasks**:
  - Compare initial concentration scores between test versions.
  - Investigate performance improvement due to repetition.
  - Compare repeated testing with same vs. different test versions.
- **Methods**:
  - Normality assessment using QQ-plots and Kolmogorov–Smirnov test.
  - Variance comparison with F-test.
  - Hypothesis testing: paired and unpaired t-tests.
    
---

### 3. Comparison of Multiple Distributions – Cuckoo Egg Length (`Projekt3`)
- **Goal**: Analyze whether cuckoo eggs found in different host species' nests vary in length.
- **Tasks**:
  - Test overall differences between host bird species.
  - Identify which species pairs show significant differences.
- **Methods**:
  - Normality assessment using QQ-plots.
  - Analysis of Variance (ANOVA).
  - Multiple comparison procedures: final test principle (Abschlusstestverfahren), Bonferroni-Holm correction.

---

### 4. Contingency Table Analysis – Olympic Medal Table (`Projekt4`)
- **Goal**: Explore categorical associations between sports, medal types, and countries.
- **Tasks**:
  - Investigate the relationship between sport and country in total medal counts.
  - Analyze medal color vs. country for each sport.
  - Explore medal color vs. sport for each country.
- **Methods**:
  - Contingency tables.
  - Chi-squared tests of independence and Fisher’s exact test (used when assumptions for chi-squared are not met).
  - Bonferroni-Holm adjustment for multiple testing.

---

### 5. Logistic Regression Study (`Projekt5`)
- **Goal**: Build and evaluate logistic regression models to predict a binary target variable.
- **Tasks**:
  - Model `Leading_Candidate` using available predictors.
  - Perform variable selection and interpret model coefficients with confidence intervals.
  - Compare full and reduced models using cross-validated ROC and AUC.
- **Methods**:
  - Logistic regression, stepwise variable selection, and coefficient interpretation with confidence intervals.
  - ROC/AUC analysis and cross-validation for model validation
    
---

## Tools and Packages
- **Language**: R
- **Packages**: `ggplot2`, `readxl`, `pROC`, `caret`
