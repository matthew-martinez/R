# Mapping Census Data in R

## Goal
Efficiently create self-contained web-ready interactive maps of U.S. Census data using R. In the tutorial, I look at median household income across census tracts in Washington, D.C.

## Data Source
Data is from the U.S. Census Bureau's American Community Survey (ACS, 5-year, 2019). It is extracted from the U.S. Census Bureau API using tidycensus. The table requested is B19013, which provides a measure of median household income in the past 12 months.

## Files
CensusDataMappingWithR.r
- Annotated code for the analysis presented in the tutorial

ACSMedIncomeData2019.csv
- The data acquired from the U.S. Census Bureau API used for the analysis

## Libraries Used
The code requires the following libraries be installed:

- tidyverse
- tidycensus
- mapview
- leaflet
- htmltools
- htmlwidgets

## Links
[PRB Webpage](https://www.prb.org)
