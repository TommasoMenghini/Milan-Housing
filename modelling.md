# Modeling and Results

This document describes the modeling phase of the Milan housing price project.
The analysis closely follows the *Modeling and Results* section of the original report
and mirrors the implementation in the R modeling script.

> **Prerequisite**  
> All analyses presented here assume that the data have already been cleaned,
> feature-engineered, and imputed as described in  
> [`preprocessing.md`](./preprocessing.md).
> The input dataset for this phase is `cleaned_training.csv`.

---

## 1. Setup and Data Loading

We begin by clearing the workspace, loading the required libraries, and reading the
cleaned dataset produced during the pre-processing phase.

```r
rm(list = ls())

library(ggplot2)
library(dplyr)
library(MASS)

data <- read.csv("cleaned_training.csv", header = TRUE)
str(data)

