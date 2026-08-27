getwd()
#from Ben B's repository https://github.com/Environment-Agency-Gov/ncea-das-network_eval-missing_data/blob/main/scripts/rss_chem_missing_data.r 
#############################
### Analysis of missing data in LSN network
### Chemistry (Water quality - WIMS)

### This script generates the missing data, refer to report file for analysis and plots.
### This script has been modified from Ben's original to run on RSTudio workbench 

# Missing data is not recorded, its just missing.
# Find which months have missing values by reshaping data by month - this will add NaN to sites with missing data for each month
# Some network id's have multiple values per month - for the purposes of finding missing data, just calculate a mean.
# Not all sites are monitored each year.
# Chemistry = 100 sites a year 

#############################
# PART 1 - Loads data and initial cleaning/processing.
#############################

# Load libraries
library(data.table) # data management
library(databricks) # connect to databricks
library(DBI)
library(sf)
library(dplyr)
library(lubridate)

# Load functions 
source("LSN_missingness_chem/missing_data_functions.r")

# for reference delete later
#RSS_data <- dbGetQuery(con,
"SELECT * FROM prd_dash_lab.seda_restricted.rss_tbl_samp_wims"


# load data

warehouses <- db_sql_warehouse_list()

con <- dbConnect(
  drv = DatabricksSQL(),
  warehouse_id = "0a86ae40313bb7db",
  catalog = "prd_dash_lab")

LSN_data <- dbGetQuery(con,
                       "SELECT * FROM prd_dash_lab.seda_unrestricted.ard_lsn_tbl_sample_wims")


LSN_data_filtered <- LSN_data %>%
  dplyr::select(
    suite_type,
    det_desc,
    network_id) %>%
  dplyr::filter(
    suite_type == param$st,
    det_desc == param$dc)


### RSS/RSN/SSN data parameters
#st= suite type
#dc= det_code but not included atm
#fn = 
#fnn = 
#years = sampling years

param <- list(st="LSN", dc="Nitrogen, Total Oxidised, Filtered as N", fn="lsn_wims_", fnn="Nitrogen", years=2026)


## Site data
# Filter on network, and on design years
# Convert years to "design years"
if(param$st=="LSN") {
  param$design_years <- ifelse(param$years %in% 2024:2027, 2024, ifelse(param$years >= 2028, 2028, param$years))
} else {
  param$design_years <- ifelse(param$years %in% 2021:2025, 2021, ifelse(param$years >= 2026, 2026, param$years))
}


LSN_sites_1 <- sdf_sql(sc, paste0("SELECT network_id, sample_datetime, meas_sign, meas_result 
FROM prd_dash_lab.seda_unrestricted.ard_lsn_point_sites
AND suite_type = '", param$st,
                             "' AND det_desc =", param$dc)) |> as.data.table()

# Load sites, match on design year
LSN_sites <- dbGetQuery(con,
                       "SELECT * FROM prd_dash_lab.seda_unrestricted.ard_lsn_point_sites")

#Select columns

LSN_sites_filtered <- LSN_sites %>%
  mutate(version_start = dmy(version_start)) %>%
  select(
    network_id,
    original_or_oversample,
    panel,
    site_commenced,
    status,
    replacement_site_flag,
    latitude_wgs_84,
    longitude_wgs_84,
    version_start
  ) %>%
  filter(
    startsWith(network_id, param$st),
    year(version_start) %in% param$design_years)

#Panel

LSN_panel <- dbGetQuery(con,
                        "SELECT * FROM prd_dash_lab.seda_unrestricted.ard_lsn_tbl_panel_design")



