# ACATH Dataset Analysis – Cardiovascular Diseases

## Description
This project analyzes the **acath** dataset from Duke University's Cardiovascular Disease Database using **R** for statistical analysis.  
The focus is on clinical data from 3,501 patients (after removing missing values) to model the probability of **significant and severe coronary artery disease**.

## Objectives
- Analyze the distribution of key clinical and demographic variables.  
- Evaluate relationships between variables through univariate and bivariate analysis.  
- Build logistic regression models to estimate the probability of:  
  - Significant coronary artery disease (**sigdz**)  
  - Severe coronary artery disease (**tvdlm**) conditional on the presence of significant disease  
- Assess model fit and predictive performance.

## Dataset
- **Name:** acath  
- **Source:** Duke University – Cardiovascular Disease Database  
- **Sample:** 3,501 patients (2,332 for the second model)  

### Main Variables
| Variable | Description |
|----------|-------------|
| sex      | sex (0=male, 1=female) |
| age      | age in years |
| cad.dur  | symptom duration (days) |
| sigdz    | significant coronary artery disease (0=absent, 1=present) |
| tvdlm    | severe coronary artery disease (0=absent, 1=present) |

## Tools and Libraries
- **Software:** R ([r-project.org](https://www.r-project.org))  
- **Packages:** base R, descriptive statistics, logistic regression, parametric and non-parametric tests  
- **Significance level:** 0.05

## Exploratory Analysis

### Univariate Analysis
- Frequency distributions for qualitative variables (**sex, sigdz, tvdlm**) with proportion tests  
- Descriptive statistics for quantitative variables (**age, cad.dur**) with boxplots, histograms, Q-Q plots  
- Normality test: Shapiro-Wilk  

### Bivariate Analysis
- **sex vs sigdz/tvdlm:** Chi-square, Odds Ratio  
- **age/cad.dur vs sigdz/tvdlm:** t-test, Mann-Whitney, Welch  
- Correlation age vs cad.dur: Pearson and Spearman  
- Key result: disease is more frequent in males, probability increases with age

## Statistical Modeling

### Model 1: Significant Coronary Artery Disease (sigdz)
- **Type:** Bernoulli logistic regression (logit)  
- **Covariates:** sex, age  
- **Estimated formula:**  

logit(π_i) = -2.575 - 1.937sex + 0.077age

- **Key interpretations:** Higher probability in males; increases with age  
- **Performance:**  
- Accuracy: 74.9%  
- Sensitivity: 88.5%  
- Specificity: 52.1%  
- ROC AUC: 0.774  

### Model 2: Severe Coronary Artery Disease (tvdlm | sigdz=1)
- **Type:** Bernoulli logistic regression (logit)  
- **Covariates:** sex, age, cad.dur  
- **Estimated formula:**  


logit(π_i) = -2.079 - 0.546sex + 0.034age + 0.006*cad.dur

- **Key interpretations:** Higher probability in males; increases with age and symptom duration  
- **Performance:** evaluated via AUC, residual analysis, and comparison with intercept-only model

## Conclusions
- Males and older patients have a higher probability of significant and severe coronary artery disease  
- cad.dur affects the probability of severe disease but not significant disease  
- Logistic regression is a useful predictive tool, though some residuals and specificity need attention  
- Bivariate analysis and correlations confirm trends consistent with clinical literature

## Author 
- Edoardo Lovato  
- **Degree Program:** Statistics for Technologies and Sciences  
- **Department:** Statistical Sciences, University of Padua
