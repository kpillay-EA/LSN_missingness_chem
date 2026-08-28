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
library(tidyr)

# Load functions 
source("LSN_missingness_chem/missing_data_functions.r")

### RSS/RSN/SSN data parameters
#st= suite type
#dc= det_code but not included atm
#fn = 
#fnn = 
#years = sampling years

param <- list(st="LS", dc="Ammoniacal Nitrogen, Filtered as N", fn="lsn_wims_", fnn="Nitrogen", years=2026)

## Site data
# Filter on network, and on design years
# Convert years to "design years"

if(param$st=="LSN") {
  param$design_years <- ifelse(param$years %in% 2024:2027, 2024, ifelse(param$years >= 2028, 2028, param$years))
} else {
  param$design_years <- ifelse(param$years %in% 2021:2025, 2021, ifelse(param$years >= 2026, 2026, param$years))
}


# for reference delete later
#RSS_data <- dbGetQuery(con,
"SELECT * FROM prd_dash_lab.seda_restricted.rss_tbl_samp_wims"


# load data

warehouses <- db_sql_warehouse_list()

con <- dbConnect(
  drv = DatabricksSQL(),
  warehouse_id = "0a86ae40313bb7db",
  catalog = "prd_dash_lab")

#in Ben's code this is rss_dt
LSN_data <- dbGetQuery(con,
                       "SELECT * FROM prd_dash_lab.seda_unrestricted.ard_lsn_tbl_sample_wims")


LSN_data_filtered <- LSN_data %>%
  dplyr::select(
    suite_type,
    det_desc,
    network_id)

LSN_data_filtered <- LSN_data %>%
  filter(
    startsWith(suite_type, param$st),
    det_desc == param$dc
  ) %>%
  dplyr::select(
    suite_type,
    det_desc,
    network_id,
    sample_datetime,
    meas_sign,
    meas_result)


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
#Ben's code, but the input folder is very different to LSN panel design table
#but the information can be extracted from LSN_dta and LSN_sites
#rss_panel_dt <- sdf_sql(sc, paste0("SELECT network, panel_name, programme_year, year_of_sampling
#FROM prd_dash_lab.seda_restricted.rss_tbl_paneldesign
#WHERE network LIKE '", ifelse(param$st == "RSN", "River", "Small"), "%'")) |> as.data.table()

#columns required
#network --> not required as in LSN they are the same
#panel_name --> panel column from LSN_sites (should i rename?)
#programme_year --> all should be 2024 when the sampling on LSN started - version start extract the year only 
#year_of_sampling --> year column from LSN_sites (used only 2024 and 2025 so that is year 1 and 2 - need to format this again)

LSN_panel_filered <- LSN_sites %>%
  dplyr::select(network_id,
    panel,
    version_start,
    year)

LSN_panel_filered_1 <- LSN_panel_filered %>%
  mutate(panel = gsub(" ", "_", panel))

LSN_panel_filered_long <- LSN_panel_filered_1 %>%
  separate_rows(year, sep = ",\\s*")

LSN_panel_filtered_long_filtered <- LSN_panel_filered_long %>%
  filter(year %in% c("Y1", "Y2"))

LSN_panel_filtered_long_filtered <- LSN_panel_filtered_long_filtered %>%
  mutate(
    programme_year = recode(year,
                         "Y1" = "2024",
                         "Y2" = "2025"))


### Convert data
# Convert date/time to .IDate
#convert to data.table first
LSN_data_filtered <- as.data.table(LSN_data_filtered)
LSN_data_filtered[, sample_date := as.IDate(sample_datetime)]

# Create year and month columns
LSN_data_filtered[, sample_year := year(sample_date)]
LSN_data_filtered[, sample_month := month(sample_date)]
# change id to factors
LSN_data_filtered[, network_id := as.factor(network_id)]
# Set key (order)
setkeyv(LSN_data_filtered, c("network_id", "sample_date"))


# Add geometry to sites
LSN_sites_filtered <- as.data.table(LSN_sites_filtered)
LSN_sites_filtered[, geometry := sf::st_as_sf(LSN_sites_filtered[, .(longitude_wgs_84, latitude_wgs_84)], coords=c("longitude_wgs_84", "latitude_wgs_84"), crs=4326) |> st_transform(27700)]


# Add years to panel
LSN_panel_filtered_long_filtered <- as.data.table(LSN_panel_filtered_long_filtered)
LSN_panel_filtered_long_filtered[, panel_year := year(programme_year)]


###################################
### Add rbd and mncat to sites list









