# `stdv`: Robust Standard Deviation Calculator for R

**Developed at Cochin University of Science and Technology (CUSAT)**

`stdv` is a custom R function designed to calculate the sample standard deviation of vectors, matrices, and data frames. It provides robust error handling, strict type safety, and flexible missing value strategies that mirror base R's `cov()` function logic.

## Features

- **Multi-Type Support:** Computes standard deviations for numeric vectors, and performs column-wise computations for matrices and data frames.
- **Advanced NA Handling:** Features a `use` argument (`all.obs`, `complete.obs`, `everything`, `na.or.complete`) to give you granular control over missing values.
- **Strict Type Safety:** Automatically blocks incompatible types like `Date`, `POSIXt`, `factor`, and raw `list` objects, preventing silent errors.
- **Graceful Degradation:** When passed a data frame, it calculates SD for numeric columns while safely returning `NA` for non-numeric columns (like characters or factors) without breaking the execution.
- **Mathematical Accuracy:** Enforces a minimum of 2 complete observations to compute the sample standard deviation mathematically.

## Installation

Save the `stdv.R` file into your project directory and source it:
