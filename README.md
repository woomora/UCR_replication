# Replication Package: *Unveiling the Cosmic Race*

## Overview

This repository contains the replication materials for the paper:

[**L. Guillermo Woo-Mora (2025)** _Unveiling the Cosmic Race: Skin Tone and Intergenerational Economic Disparities in Latin America and the Caribbean_](https://doi.org/10.1016/j.jdeveco.2025.103594)

This study presents the most comprehensive cross-country analysis to date of how **skin tone**, distinct from self-reported race, correlates with **income**, **human capital**, and **educational intergenerational mobility** (IM) in 25 Latin American and Caribbean countries. The analysis leverages enumerator-assessed and machine-rated skin tone data from LAPOP and PRODER.

This replication package includes all scripts necessary to:

- Harmonize raw LAPOP data and generate derived datasets
- Replicate all tables and figures in the published article

> ⚠ **Note:** This repository does **not include raw data files** due to licensing restrictions. You must manually obtain the required LAPOP datasets and (optionally) the PRODER dataset as described below.

---

## Repository Contents

### `UCR_master.R`

Main script that executes the full replication pipeline: loads data, runs regression models, creates plots, and exports results. Start here after preparing the data.

### `UCR_functions.R`

Helper functions for data cleaning, transformation, modeling, and visualization. Called internally by the other scripts.

### `UCR_lapop_merge.R`

This script processes LAPOP survey files to create a harmonized, cross-country analytical dataset. It performs the following tasks:

- Filters and merges survey waves (primarily 2012, 2014, and 2016/2017)
- Cleans and recodes the PERLA skin tone scale (enumerator-assessed)
- Constructs key outcome and control variables: income, schooling, and educational intergenerational mobility
- Generates unique cluster IDs at the strata-by-year-by-enumerator level for robust inference

**Required Input:**

- LAPOP AmericasBarometer raw data, either by country or by wave, depending on your access.

> ⚠ **Note:** This project uses the *merged country-level datasets* available through institutional access, covering survey waves from approximately 2004 to 2023 (e.g., `ARG_merge_2008–2023_LAPOP_AmericasBarometer_v1.0_w.sav`). These files should be saved in the `lapop/merge_by_country/` directory. If you have institutional access, these are the recommended input files for running the pipeline as-is.

> Alternatively, if you do not have access to the merged files, you will need to manually download and merge the raw survey files available at [https://www.vanderbilt.edu/lapop/raw-data.php](https://www.vanderbilt.edu/lapop/raw-data.php). You can either:
> 
> - Merge by **country** (across survey years) — recommended for replicating this analysis
> - Or merge by **wave** (across countries) — also supported
> 
> Please feel free to reach out if you have questions about how to replicate the merging or preprocessing steps.

### `UCR_proder.R`

Incorporates data from the PRODER (Proyecto sobre Discriminación Étnico-Racial en México) dataset to extend the analysis for the Mexico case study using machine-rated skin tone data. Includes additional controls for childhood context and parental occupation.

**Required Input:**
- [PRODER dataset](https://discriminacion.colmex.mx/documentacion-y-base-de-datos-de-la-encuesta-proder/)

> Please cite the following articles if using PRODER data:
>
> Roth, W. D., Solís, P., & Sue, C. A. (2022). Beyond Money Whitening: Racialized Hierarchies and Socioeconomic Escalators in Mexico. American Sociological Review, 87(5), 827–859. https://doi.org/10.1177/00031224221119803
>
> Solís, P., Güémez, B., & Campos-Vázquez, R. M. (2025). Skin Tone and Inequality of Socioeconomic Outcomes in Mexico: A Comparative Analysis Using Optical Colorimeters and Color Palettes. Sociology of Race and Ethnicity, 11(1), 50–68. https://doi.org/10.1177/23326492231217232

---

## Replication Instructions

### 1. System Requirements

- **R version**: 4.5.0 (2025-04-11) or higher
- **RStudio**: 2025.05.0+496 (Mariposa Orchid)
- **Pandoc**: 3.4 (via Quarto)
- **Quarto**: 1.6.42
- **Operating System**: macOS Sequoia 15.3.1 (Apple Silicon / aarch64)


#### Additional Software and Tools

- **macOS**: 
  - Install Xcode command line tools:  
    ```bash
    xcode-select --install
    ```
  - Optionally, install system dependencies using Homebrew:  
    ```bash
    brew install openssl libgit2
    ```

- **Windows**:  
  - Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

- **Linux**:  
  - Install development libraries (Ubuntu/Debian):
    ```bash
    sudo apt-get install libssl-dev libcurl4-openssl-dev libxml2-dev
    ```


### 2. Setup

#### a. Clone the Repository
Download or clone the repository:
```bash
git clone https://github.com/billywoom/UCR_replication.git
cd UCR_replication
```

#### b. Install R Packages
This project uses the `pacman` package to simplify package management.

1. Install `pacman` if it's not already installed:
```r
# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")
```
    
2. Load and install all required packages:
```r
library(pacman)
# Load and manage packages
pacman::p_load(
  # Data manipulation and wrangling
  tidyverse,     # Core packages for data manipulation and visualization (e.g., dplyr, ggplot2)
  haven,         # Import foreign data formats (e.g., Stata files from LAPOP)
  hutils,        # Utility functions for weighted stats, survey analysis, etc.
  
  # Visualization enhancements
  ggridges,      # Ridgeline plots for distributions
  viridis,       # Colorblind-friendly color palettes for plots
  wesanderson,   # Aesthetic color palettes inspired by Wes Anderson films
  ggExtra,       # Adds marginal plots and enhancements to ggplot2
  ggrepel,       # Prevents overlapping text labels in ggplot2
  ggthemes,      # Additional themes and styling for ggplot2
  cowplot,       # Align and combine multiple ggplot2 plots
  ggpubr,        # Publication-ready ggplot2-based charts
  
  # Table formatting
  kableExtra,    # Create and style LaTeX/HTML tables (e.g., regression tables)
  
  # Regression and statistical modeling
  fixest,        # High-performance regression models with fixed effects
  fastDummies,   # Create dummy variables quickly from factor variables
  broom,         # Tidy up model outputs into data frames
  marginaleffects, # Compute and plot marginal effects from regression models
  ggeffects,     # Extract and visualize marginal effects for ggplot2
  nnet,          # Neural networks and multinomial logit models
  
  # Statistical inference and diagnostics
  binsreg,       # Binned scatterplots, useful for visualizing nonlinear trends
  progress       # Adds progress bars to long-running R operations
)

# Load GitHub packages
pacman::p_load_gh(
  "chadhazlett/sensemakr",  # Sensitivity analysis for causal inference (GH version)
  "dmbwebb/qval"            # Multiple hypothesis testing q-values (GH version)
)  
```

### Session Information

### Session Information

To replicate the analysis, ensure your R session matches the following configuration:

```r
R version 4.5.0 (2025-04-11)
Platform: aarch64-apple-darwin20 (macOS Sequoia 15.3.1)
RStudio: 2025.05.0+496 (Mariposa Orchid)
Pandoc: 3.4
Quarto: 1.6.42
Time zone: Europe/Paris

Key attached packages:
- tidyverse, dplyr, ggplot2, tidyr, tibble, readr, purrr, stringr, forcats, lubridate
- fixest (0.12.1), fastDummies, janitor, haven, hutils, binsreg, cowplot
- broom, marginaleffects (0.26.0), ggeffects, ggpubr, ggrepel, ggridges
- ggthemes, kableExtra, pacman, progress, countrycode, sf, WeightIt, nnet, viridis, wesanderson
```

> ⚠️ **Note**: For full compatibility with the plotting and marginal effects workflow in this project, it is critical to use:
> - `fixest` version **0.12.1** or newer  
> - `marginaleffects` version **0.26.0** or newer  
> 
> Older versions may lead to errors when converting model estimates into tidy data frames or generating visualizations.

---

## Citation

If you use this replication package or build upon the findings in your research, please cite the article as follows:

**L. Guillermo Woo-Mora (Forthcoming)**  
*"Unveiling the Cosmic Race: Skin Tone and Intergenerational Economic Disparities in Latin America and the Caribbean."*  
*Journal of Development Economics* (Forthcoming).

### BibTeX

```bibtex
@article{Woo-Mora2025,
   author = {L. Guillermo Woo-Mora},
   doi = {10.1016/J.JDEVECO.2025.103594},
   issn = {0304-3878},
   journal = {Journal of Development Economics},
   month = {8},
   pages = {103594},
   publisher = {North-Holland},
   title = {Unveiling the Cosmic Race: Skin tone and intergenerational economic disparities in Latin America and the Caribbean},
   url = {https://linkinghub.elsevier.com/retrieve/pii/S0304387825001452},
   year = {2025}
}
```
