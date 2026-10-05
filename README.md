# Unsheltered Unaccompanied Youth Across States (2025 PIT)

## Purpose
Using 2025 state-level Point-in-Time (PIT) data from HUD, this analysis
asks: in states where more of the homeless population is unsheltered,
are a larger share of counted unaccompanied youth unsheltered too, and
how does the youth share compare with that of the rest of the homeless
population? A secondary aim identifies states whose youth counts depart
most from what their overall pattern predicts.

## Data source
U.S. Department of Housing and Urban Development (HUD), 2007–2025
Point-in-Time Estimates by State, available on the HUD User AHAR page:
https://www.huduser.gov/portal/datasets/ahar/2025-ahar-part-1-pit-estimates-of-homelessness-in-the-us.html

Only the 2025 sheet is used. It was saved from the original .xlsb file
to .csv in Excel (macros disabled).

## Files
- `Project 3.R`: data cleaning, exploration, Poisson and negative
  binomial models
- `Data/2025-PIT-Counts-by-State.csv`: analysis data (public)
- `Project 3.Rproj`: RStudio project file

## How to run
1. Open `Project 3.Rproj` in RStudio.
2. Install packages if needed: readr, dplyr, tidyr, ggplot2, ggrepel, AER, MASS
3. Run `Project 3.R`.

## Analysis
- Descriptive statistics, correlations, and a figure comparing youth
  and rest-of-population unsheltered shares by state
- Poisson regression of unsheltered youth counts, with total counted
  youth as an offset
- Overdispersion checks (Pearson ratio, formal dispersion test)
- Negative binomial regression, compared with Poisson by AIC