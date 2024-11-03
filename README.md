# Research on Association between Crime Rates and Seasons
## Overview
This project analyzes the association between crime rates and seasons in New York City. Using time series models (ARIMA, ETS) and ANOVA tests, we evaluated crime patterns over time to determine how seasonal changes affect overall crime rates and specific crime types, such as assault.

## Key Features

### 1. Data Preprocessing
- Merged and cleaned two raw datasets from the NYPD, covering 5+ million crime records from 2017–2023.
- Retained relevant fields (e.g., arrest date, crime type) and transformed data to include monthly crime counts.

### 2. Time Series Modeling
- **Models Used**: ARIMA, ETS
- **Purpose**: Detect long-term trends and seasonality in crime rates.
- **Findings**: ARIMA was the optimal model, effectively capturing time-dependent patterns with stationary data. ETS models provided additive patterns but showed some autocorrelation.

### 3. ANOVA Testing
- Divided the data into four seasonal groups and conducted ANOVA to test if seasonal factors influence crime rates.
- **Results**: Significant p-values (<0.05) suggest that crime rates vary across seasons, with peaks in spring and summer.

## Figures
- **Top 10 Crime Types in NYC**: This bar chart shows the most common crime types in NYC, with incident counts for each category.

  ![Top 10 Crime Types in NYC](https://github.com/LinZhou606/Crime-Analysis-in-New-York-City/blob/main/Results/types.png)

- **Seasonal Crime Trends**: This box plot displays the distribution of crime counts across different seasons, highlighting higher crime rates in spring and summer.

  ![Seasonal Crime Trends](https://github.com/LinZhou606/Crime-Analysis-in-New-York-City/blob/main/Results/Seasonal%20Crime%20Trends.png)

- **Residual Diagnostics for ARIMA and ETS Models**: These residual diagnostic plots evaluate the model fit and check for white noise in ARIMA and ETS models, helping to assess model accuracy.

  ![Residual Diagnostics](https://github.com/LinZhou606/Crime-Analysis-in-New-York-City/blob/main/Results/Residual%20Diagnostics.png)

## Key Technologies Used

- **Languages**: R
- **Libraries**: dplyr, lubridate, forecast, tseries, ggplot2
- **Tools**: RStudio, GitHub for version control

## Detailed Report
For an in-depth analysis, including methodology, results, and conclusions, refer to the [Project Report](https://github.com/LinZhou606/Crime-Analysis-in-New-York-City/blob/main/Results/Report.pdf).

