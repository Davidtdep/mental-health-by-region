# Mental-health-by-region

This repository contains the R scripts used for the bibliometric and indicator-based analysis of **mental health research**, categorized by **WHO regions**, and their associations with **health, economic, governance, and inequality indicators** from global databases including WHO, World Bank, Our World in Data, and IHME.

## Purpose
This repository provides a reproducible workflow for analyzing global mental health research trends and their association with multiple national and regional indicators. The analyses aim to:

- Explore the relationship between **bibliometric metrics** and **sociodemographic, economic, and health indicators**.
- Evaluate **regional differences** in research output and its association with structural indicators.
- Perform **linear regression models** for each **WHO region**.
- Conduct **meta-analyses** to estimate pooled associations across regions.
- Visualize results using **heatmaps** and **indicator-specific plots**.

## Required R packages
The following R packages are necessary to execute the analyses:

- **base packages**
  - `readxl`
  - `readr`
  - `dplyr`
  - `stringr`
  - `tidyr`
  - `purrr`
  - `tibble`
  - `ggplot2`

- **data manipulation & writing**
  - `tidyverse`
  - `writexl`
  - `openxlsx`

- **data visualization**
  - `ggsci`
  - `pheatmap`
  - `ggbreak`
  - `igraph`

- **API and data extraction**
  - `httr`
  - `jsonlite`
  - `rvest`
  - `wbstats`

- **meta-analysis**
  - `metafor`

Make sure to install these packages using `install.packages("package_name")` or load them via `library()` before running the scripts.

## Repository contents
This repository includes the following two scripts:

- `pre_analysis.R`: Organizes and defines the indicator groups, roles (dependent or independent), and prepares variable classification by theme.
- `main_analysis.R`: Executes the regression models by region, generates heatmaps and plots, and performs meta-analysis on regression results.

## Data sources and preprocessing
A total of **386,671 mental health research articles** were included, obtained using a predefined search strategy. Metadata included **publication year**, **country of the first author**, **journal h-index**, **citations**, **open access status**, and **journal quartile**. Each article was assigned to one of the **WHO regions** based on the first author’s country.

A total of **60 national indicators** were compiled from:
- WHO Global Health Observatory
- The World Bank
- Our World in Data
- Institute for Health Metrics and Evaluation (IHME)

These indicators were grouped into five thematic categories:
1. Economy, Development, and Education  
2. General Health  
3. Mental Health and Well-Being  
4. Inequality and Poverty  
5. Governance and Rights

Indicators were summarized by region and year to enable cross-regional comparison. Complete lists are available in Supplementary Tables 1–4.

## Analyses included

1. **Data preparation and indicator grouping**  
   - Assignment of each indicator as either **independent (i)** or **dependent (d)**.
   - Classification into thematic categories for structured modeling.

2. **Linear regression models**  
   - Models were fitted to examine the relationship between **each indicator** and **total number of publications**
   - For each indicator, its role (dependent or independent) was used to build the appropriate model.  
   - Separate models were computed for each **WHO region**, and standardized regression coefficients were also calculated.

3. **Heatmap visualizations**  
   - A matrix of standardized coefficients was generated to illustrate the associations.  
   - Heatmaps were annotated with significance levels (*** p < 0.001, ** p < 0.01, * p < 0.05).  
   - Indicators were grouped and sorted for interpretability.

4. **Focused plots for key indicators**  
   - Separate bar plots with coefficient labels were created for specific indicators of interest, including:
     - Prevalence and DALYs of mental health disorders  
     - Child mortality  
     - Physicians per 1,000 people  
     - R&D expenditure  
     - Extreme poverty rates  

5. **Meta-analysis**  
   - For each indicator, a **random-effects meta-analysis** was conducted using regression coefficients and standard errors across WHO regions.  
   - The **Restricted Maximum Likelihood (REML)** method was used to estimate between-region variance.  
   - Pooled estimates and heterogeneity statistics (τ², I², Q) were reported.  
   - Only significant pooled results were included in the main text; full outputs are provided as supplementary material.

6. **Output and significance reporting**  
   - Results were filtered by **p-value** and marked with stars for quick interpretation.  
   - Outputs are available for use in **tables**, **figures**, and **supplementary datasets**.

## Data availability
This project uses openly available data from public databases. The mental health bibliometric dataset derived from Web of Science is **available upon reasonable request**. All indicator data are from:

- **WHO Global Health Observatory**

## License
This repository is licensed under the **MIT License**, allowing free use, modification, and distribution with attribution. See the `LICENSE` file for more details.
