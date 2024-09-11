# Replication Package for 'Careers in Multinational Enterprises'

**Last update: Sep 11, 2024**

## Overview

This replication package contains the scripts required to replicate the results of the paper *"Careers in Multinational Enterprises"* by Marcus Roesch, Michiel Gerritse, and Bas Karreman. Due to the confidential nature of the data, the analyses must be executed on the servers of Statistics Netherlands (CBS). For more details on accessing the data, see "Source Data Access."

## Folder Structure

- `scripts/`: Contains R and Stata code to run the analysis.
- `source_data/`: Place the confidential source data files here (see "Source Data Access").
- `data/`: Stores intermediate datasets created during analysis.
- `outputs/`: Final figures and tables for the paper will be saved here.

## Replication Instructions

1. Place worker-level source data in the `source_data/` folder.
2. Specify the SQL server connection for firm-level data in the `01_company_data.R` script (see comments in the script for guidance).
3. Adjust file paths in `00_main.R` to match your directory structure.
4. Run `00_main.R` from the `scripts/` folder in R. This will:
   - Build the analysis datasets from raw data.
   - Reproduce the figures and tables from the paper.
5. To replicate Appendix B, run the Stata code located in `scripts/Stata/` after the R scripts have finished running.
6. Results (tables and figures) will be saved in the `outputs/` folder.
7. **Important**: Running the full analysis (especially the bootstraps) may take several days. We recommend running the scripts in stages based on available resources.

### Software and Hardware Requirements

- **R version >= 4.0.0**
- **Stata version >= 17.0** (for Appendix B)
- The analysis was performed on a server with an 8-core processor running at 3.19 GHz and 48 GB of memory. Ensure you get access to a system at CBS that has sufficient computational resources for large datasets and long runtimes.
- Important R-packages: data.table, fixest, ggplot2 
- Important Stata packages: estquant

## Source Data Access

This project uses confidential datasets provided by Statistics Netherlands (CBS). The specific datasets are:

### Worker-level Datasets
- **(S)POLISBUS** (employer-employee data): 2006 - 2021
- **GBAPERSOON** (worker demographics): 2021
- **NIET_GBAPERSOON** (worker demographics): 2021
- **DIPLOMAHOTAB** (HO/WO graduations): 2004 - 2020
- **DIPLOMAMBOTAB** (MBO graduations): 2004 - 2020

### Firm-level Datasets
- **Uci** (Ultimate controlling institutional unit): 2006 - 2021
- **Sfgo** (Foreign subsidiaries): 2006 - 2009
- **Multinationals** (Domestic Multinationals): 2010 - 2021
- **Abr** (company group IDs): 2006 - 2021
- **Abr_regio** (locations of firms): 2006 - 2021
- **Abr_pab** (NACE industry codes): 2006 - 2021
- **ihg** (export and import statistics of firms): 2007 - 2021

### Important Notes

We had employee access to CBS data, which did not require requesting data through the Microdata service. However, to the best of our knowledge, the analyses can be replicated using the CBS Microdata service. If any of the individual datasets are incomplete or unavailable through the Microdata service, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl) with your specific request.