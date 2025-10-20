# R Filesize Time-Series Analysis

This repository contains a comprehensive R script for analyzing the growth and patterns of file sizes over time, based on file system metadata (e.g., file extension, size, and modification time).

The analysis utilizes the power of `tidyverse`, `lubridate`, and `ggplot2` to transform raw file data into insightful time-series visualizations.

## Key Features

- **Data Wrangling:** Uses `data.table::fread` for fast data loading and `dplyr` for cleaning.
- **Time Feature Engineering:** Creation of new time dimensions (Year, Quarter, Year-Month, Week-of-Month, Weekday) using the `lubridate` package.
- **Multi-Granularity Analysis:** Aggregation and visualization of total and cumulative file size across five different time scales:
    1.  **Yearly**
    2.  **Quarterly**
    3.  **Monthly**
    4.  **Weekly**
    5.  **Weekday** (to find daily operational patterns)
- **Trend Smoothing:** Application of **Rolling Averages** (`zoo::rollapply`) to smooth out daily and monthly noise and reveal underlying long-term trends.

## Prerequisites

To run the analysis script (`timeseries_analysis.R`), you need the following R packages installed:

```R
install.packages(c("tidyverse", "lubridate", "ggplot2", "ggrepel", "zoo", "data.table"))
