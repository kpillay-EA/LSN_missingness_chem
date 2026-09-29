# Evaluation of the Lake Surveillance Network data - Missing data

Based of code from Ben's repository [here:](https://github.com/Environment-Agency-Gov/ncea-das-network_eval-missing_data/)

Code and analysis for evaluation of the networks, looking at patterns of missing data.

Published reports for each analysis are [here:](https://kpillay-ea.github.io/LSN_missingness_chem/)

## Running the code

To generate the reports run the `LSN_WIMS_missing_data_report.Rmd` file

The following R packages are required to run the code:

```{R}
data.table # Used to store and process the data
databricks # databricks connections
sf # Used to load and process GIS data
```

Currently only WIMS data is available on Databricks (`ard_lsn_tbl_sample_wims`)

Set the parameters to run the code at the start of the .Rmd file then click the Run all button to run the analysis and generate the report. 

`LSN_missingness_WIMS.R` will produce a missing data.csv file saved in the `Outputs` folder.

You will need to set the following parameters:

```{R}
st    = "LS"       # Sets the suite type to RSN (change to SSN if required)
dc    = "Ammoniacal Nitrogen, Filtered as N"      # Sets the det code, (should be det code but based on the LSN data available it is det_desc)
fn    = "lsn_wims_" # prefix for output file names
fnn   = "Nitrate"   # Sets the name of the variable
years = "2024:2025"   # Set which years you want to analyse
```

Little's MCAR test is run using a custom function found in the `missing_data_functions.r` script file. This is run in the .Rmd file. The test fails for some networks/variables which have high levels of missing data (e.g. missing entire periods).



