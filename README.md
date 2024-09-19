# Replication Package for 'Careers in Multinational Enterprises'

**Last update: Sep 12, 2024**

## Overview

This replication package contains the scripts required to replicate the results of the paper [*"Careers in Multinational Enterprises"*](https://mrcsrsch.github.io/research/2_2023_careers/) by Marcus Roesch, Michiel Gerritse, and Bas Karreman. Due to the confidential nature of the data, the analyses must be executed on the servers of Statistics Netherlands (CBS). For more details on accessing the data, see "source data access."

## Folder structure

- `scripts/`: Contains R and Stata code to run the analysis.
- `data/`: Stores intermediate datasets created during the analysis.
- `data/source_data/`: Place the (confidential) source data files here (see "source data access").
- `outputs/`: Result figures and tables for the paper will be saved here.

## Replication instructions

### Data preparation
1. Place the worker-level source data in the `data/source_data/` folder. Make sure to also place the file `data/source_data/LOCATION/pc2020_NL_NUTS-2021_v1.0.csv`.
2. Specify the SQL server connection for the firm-level source data in the `01_company_data.R` and `07_rob_location_experience.R` scripts (see comments in the scripts for guidance).
3. Adjust the file paths in `00_main.R` to match your directory structure.

### Run analysis 
4. Run `00_main.R` from the `scripts/` folder in R. This will:
   - Build the analysis datasets from the source data.
   - Reproduce the result figures and tables from the paper.
5. To replicate Appendix B, run the Stata code located in `scripts/Stata/` after the R scripts have finished running.
6. Result tables and figures will be saved in the `outputs/` folder.
7. **Important**: Running the full analysis, especially the bootstraps, may take several days. We recommend running the scripts in stages while monitoring closely. 

### Software and hardware requirements

- **R version >= 4.0.0**
- **Stata version >= 17.0** (for Appendix B)
- The analysis was performed on a server with an 8-core processor running at 3.19 GHz and 48 GB of memory. Ensure you get access to a server at CBS that has sufficient computational resources for large datasets and long runtimes.
- Important R-packages: data.table, fixest, ggplot2 
- Important Stata packages: estquant

## Source data access

This project uses confidential datasets provided by Statistics Netherlands (CBS). The specific datasets are:

### Worker-level datasets
- **(S)POLISBUS** (employer-employee data): 2006 - 2021
- **GBAPERSOON** (worker demographics): 2021
- **NIET_GBAPERSOON** (worker demographics): 2021
- **DIPLOMAHOTAB** (HO/WO graduations): 2004 - 2020
- **DIPLOMAMBOTAB** (MBO graduations): 2004 - 2020

### Firm-level datasets
- **Uci** (ultimate controlling institutional unit): 2006 - 2021
- **Sfgo** (foreign subsidiaries): 2006 - 2009
- **Multinationals** (foreign and domestic multinationals): 2010 - 2021
- **Abr** (company group IDs): 2006 - 2021
- **Abr_regio** (locations of firms): 2006 - 2021
- **Abr_pab** (NACE industry codes): 2006 - 2021
- **ihg** (export and import statistics of firms): 2007 - 2021

Due to confidentiality, we cannot share the data directly. To replicate the analysis, researchers can obtain access to these datasets through the [CBS Microdata Service](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research). Please note that access is subject to CBS's terms and fees. All data processing occurs on CBS servers, and exported outputs are subject to privacy and confidentiality checks.

To request access, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl).

### Important note

We had employee access to CBS data, which did not require requesting data through the Microdata Service. However, to the best of our knowledge, with the code provided here the analyses can be replicated with Microdata Service access. If any of the individual datasets are incomplete or unavailable through the Microdata Service, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl) with your specific request.