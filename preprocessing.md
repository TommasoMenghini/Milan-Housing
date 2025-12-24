Pre-Processing 
================

This document describes the **data pre-processing pipeline** used to prepare the Milan housing dataset for modeling.
The workflow follows the implementation in `Pre_Processing_Training.R` and the accompanying helper functions in `FunzioniUtili.R`. 

The main goals are:

- extract structured information from the raw `other_features` field (feature engineering);
- fix obvious inconsistencies and standardize categories;
- handle missing values using a principled imputation strategy;
- export a cleaned dataset (`cleaned_training.csv`) ready for model fitting. 


Loading the Data
=============

First set the working directory where `training.csv` is placed. Once this has been done, clean the workspace, and load the data. The training set contains **8000 observations**, each corresponding to a housing unit located within the **municipal boundaries of Milan**. 
For each record, the dataset provides **16 covariates** describing structural and contextual characteristics of the property, plus the **response variable** `selling_price`, which represents the market price of the house. 

``` r
rm(list = ls())

library(dplyr)
library(MASS)
library(ggplot2)
source("FunzioniUtili.R")

data <- read.csv("training.csv", header = TRUE)
```

Other Features
=============

The variable `other_features` is a character string containing a **miscellany of additional information** about the property, with multiple attributes concatenated and separated by the `|` symbol. To understand which features are present in the dataset and how frequently they occur, we first perform a **parsing step**. Each string is split into individual tokens, leading and trailing whitespace is removed, and all extracted terms are collected across the full training set.

This exploratory step allows us to:
- identify all unique attributes encoded in `other_features`;
- compute their empirical frequencies;
- distinguish between attributes that can naturally be treated as **binary indicators** and those requiring **multi–categorical or ordinal encoding**.

The resulting frequency table provides the basis for the feature engineering choices described in the following sections. 

``` r
out <- c()

for (i in 1:nrow(data)) {
  s <- data$other_features[i]
  split_righe <- strsplit(s, "\\|")[[1]]
  parole <- trimws(split_righe)
  out <- c(out, parole)
}

unique_words <- unique(out)

tab <- table(out, useNA = "always")
```

## Binary Factor Variables

A first group of attributes extracted from `other_features` can naturally be encoded as **binary indicators**, describing the presence or absence of specific amenities or structural characteristics of the property. For each selected keyword, the helper function `binaryExtr()` scans the `other_features` string and creates a new factor variable with levels `yes` and `no`. Each of these variables assumes value `yes` if the corresponding feature is mentioned for the property, and `no` otherwise.

``` r
binaryExtr("fireplace") 
binaryExtr("hydromassage")
binaryExtr("video entryphone")
binaryExtr("terrace")
binaryExtr("security door")
binaryExtr("tavern")
binaryExtr("optic fiber")
binaryExtr("electric gate")
binaryExtr("pool")
binaryExtr("tennis court")
binaryExtr("alarm system")
binaryExtr("attic")
binaryExtr("balcony")
binaryExtr("cellar")
binaryExtr("closet")
binaryExtr("reception")
```
## Multi-Categorical Variables

Some attributes extracted from `other_features` cannot be meaningfully encoded as binary variables and are therefore treated as **multi–categorical** (or ordinal) predictors. 

These variables describe graded or mutually exclusive characteristics of the property, such as the level of service provided by the building, the degree of furnishing, or the type of exposure. The general strategy consists of:
- identifying mutually exclusive textual patterns;
- defining a small number of interpretable categories;
- collapsing rare or redundant levels when appropriate.

### Example: concierge service

The presence of a concierge is encoded as an **ordinal variable**, reflecting increasing levels of service:
- no concierge,
- half-day concierge,
- full-day concierge.

Each property is assigned to exactly one category, based on the information contained in `other_features`. The resulting variable is then mapped to a numeric scale (0, 0.5, 1) to preserve its ordinal nature. 

This approach allows the model to capture differences in service intensity without introducing unnecessary dummy variables.

``` r
indici1 <- grep("half-day concierge", data$other_features, ignore.case = TRUE)
indici2 <- grep("full day concierge", data$other_features, ignore.case = T)

any(indici1 %in% indici2)

data$concierge[1:8000] <- NA 
data$concierge[indici1] <- "half-day concierge"
data$concierge[indici2] <- "full day concierge"
data$concierge[-c(indici1, indici2)] <- "no concierge"

concierge_map <- c(
  "no concierge" = 0,
  "half-day concierge"  = 0.5,
  "full day concierge" = 1)

data$concierge <- concierge_map[as.character(data$concierge)]
```
Missing Data Overview
====================

The dataset contains **5678 missing values**, which represents a non–negligible issue for the analysis. Before deciding how to handle them, it is necessary to understand the extent and structure of the missingness.

A naive approach would be to discard all observations containing at least one missing value. However, this strategy is problematic for two main reasons:
1. if incomplete observations differ systematically from complete ones, the resulting analysis may be biased;
2. when many variables are included, the number of fully observed rows can become very small, leading to a substantial loss of information.

In this dataset, **3790 observations contain at least one missing value**, which corresponds to nearly half of the training set. Removing all these rows would severely compromise the analysis. For this reason, rather than dropping observations, we adopt an **imputation-based approach**. 

## Zone

The variable `zone` identifies the neighborhood in which each property is located and is encoded as a categorical predictor with a large number of levels. In the raw data, this variable exhibits a high degree of sparsity: many neighborhoods appear only a handful of times in the training set. 

Such rare categories can be problematic for statistical modeling, as they introduce noise and reduce the stability of parameter estimates. For this reason, low–frequency zones are aggregated with geographically nearby neighborhoods.

The aggregation was performed **manually**, without relying on formal criteria such as geographic coordinates or clustering algorithms. The guiding principle was to preserve a **plausible geographic coherence** based on the map of Milan. 

An initial idea was to merge all zones with frequency below the first quartile; however, this would have required recoding 442 observations, which was deemed excessive. Instead, only neighborhoods with **fewer than 10 observations** were aggregated. This threshold represents a pragmatic compromise between reducing sparsity and preserving spatial detail. 

After aggregation, the number of distinct neighborhoods is reduced while maintaining a meaningful geographic structure. The single missing value in `zone` is imputed using the mode of the recoded variable.

``` r
tab_zone <- table(data$zone, useNA = "always")

data$zona_new <- data$zone

# (manual aggregation of rare zones, see script for full list)

tab <- table(data$zona_new)
tab 

# The only NA is imputed with the mode

data$zona_new[which(is.na(data$zona_new))] <- names(tab[which.max(tab)])
data$zona_new <- as.factor(data$zona_new)
```

## Other Variables

Before proceeding with the imputation of missing values of other variables, a set of **preliminary cleaning steps** is applied to address inconsistencies and enforce basic logical and regulatory constraints. 

The variable `square_meters` does not initially contain missing values, but some observations report implausibly small surface areas. According to Italian housing regulations (Ministerial Decree of July 5, 1975), the minimum size of a residential unit is 28 sqm, reduced to 20 sqm in 2024. All values below this threshold are therefore treated as invalid and set to missing. 

The variable `year_of_construction` is originally continuous and is discretized into ordered categories. Although discretization of continuous variables is often discouraged, doing so for building age allows the identification of broad generational patterns while simplifying the modeling of nonlinear effects. 

The variables `floor` and `total_floors` are strongly related and must satisfy basic consistency constraints. Observations where the apartment floor exceeds the total number of floors in the building are interpreted as data entry errors. Since the apartment floor is considered more reliable, the corresponding values of `total_floors` are set to missing. 

For the variable `lift`, missing values are handled using regulatory information. Italian law mandates the presence of an elevator in residential buildings with more than three floors. Accordingly, all missing values associated with `total_floors > 3` are deterministically imputed as `"yes"`. Remaining missing values are later imputed using a conditional mode based on `total_floors`. 

The categorical variable `conditions` presents missing values as well. Since it is nominal and unordered, missingness is handled by introducing an additional category representing unspecified conditions.

Energy efficiency information is summarized through the derived ordered variable `house_efficiency`, which groups the original energy classes into three levels: **Low**, **Medium**, and **High**. This transformation reduces sparsity and improves interpretability. 

Finally, the variable `condominium_fees` represents monthly condominium expenses and can legitimately take value zero. A small number of observations report extremely high values (above 2500), which are considered implausible and are therefore set to missing.

A summary of variables affected by missingness and their frequency is reported in Table 1. 

<div align="center"

  **Table 1: Missing values by variable**

| Variable              | Number of NAs | % of NAs |
|----------------------|---------------|----------|
| square.meters        | 16            | 0.2%     |
| bathrooms.number     | 25            | 0.3%     |
| heating.centralized  | 63            | 0.8%     |
| total.floors         | 102           | 1.3%     |
| year                 | 789           | 9.9%     |
| house.efficiency     | 842           | 10.5%    |
| condominium.fees     | 907           | 11.3%    |



