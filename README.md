# brfss_scd README

## Prevalence of Subjective Cognitive Decline (SCD), 2019-2023

This repository contains the analysis code for estimating the prevalence of Subjective Cognitive Decline (SCD) using BRFSS data from 2019-2023. The analysis incorporates survey weighting, imputation, and stratification by race/ethnicity.

### **Data Availability**

Zipped folders containing SAS-format `.XPT` files for this analysis are available from the **CDC BRFSS website**. To keep this repository lightweight, we do **not** store large datasets here.

To download the necessary BRFSS datasets, please refer to [`data_links.txt`](data_links.txt), which contains a full list of download links from the CDC.

### **Setup Instructions**

1. **Download this repository**
```sh
git clone https://github.com/lamhine/brfss_scd.git
cd brfss_scd
```
Or download the ZIP manually from GitHub and extract it to a local folder.

2. **Download BRFSS data** from the CDC using the links in [`data_links.txt`](data_links.txt).
   - Extract the `.XPT` files and place them inside the `data/` folder inside this repository.

3. **Open the RStudio Project (`brfss_scd.Rproj`)**.

4. **Run the analysis pipeline in R:**
```r
source("01_load_data.R")    # Loads raw BRFSS data
source("02_clean_data.R")   # Cleans and preprocesses data
source("03_impute_data.R")  # Handles missing data
source("04_analyze_data.R") # Runs analysis and estimates prevalence
source("05_visualize_results.R") # Generates plots
```

### **Outputs**
- `BRFSS_Raw.rds` → Raw combined dataset (01_load_data.R)
- `BRFSS_Cleaned.rds` → Cleaned dataset (02_clean_data.R)
- Tables & figures → From 04_analyze_data.R & 05_visualize_results.R

### **Troubleshooting**
- **Missing BRFSS files?** Ensure `.XPT` files are inside `data/`.
- **`config.R` errors?** Ensure `data_dir` is correctly set to your BRFSS data folder.
- **Working directory errors?** Always open the `.Rproj` file before running scripts.

For questions, open an issue on GitHub.
