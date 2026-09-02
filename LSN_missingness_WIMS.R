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
library(brickster)
library(DBI)
library(sf)
library(dplyr)
library(lubridate)
library(tidyr)

# Load functions 
source("missing_data_functions.r")

### RSS/RSN/SSN data parameters
#st= suite type
#dc= det_code but not included atm
#fn = 
#fnn = 
#years = sampling years

param <- list(st="LS", dc="Ammoniacal Nitrogen, Filtered as N", fn="lsn_wims_", fnn="Nitrogen", years=2024:2025)

## Site data
# Filter on network, and on design years
# Convert years to "design years"

if(param$st=="LS") {
  param$design_years <- ifelse(param$years %in% 2024:2027, 2024, ifelse(param$years >= 2028, 2028, param$years))
} else {
  param$design_years <- ifelse(param$years %in% 2021:2025, 2021, ifelse(param$years >= 2026, 2026, param$years))
}


# for reference delete later
#RSS_data <- dbGetQuery(con,
"SELECT * FROM prd_dash_lab.seda_restricted.rss_tbl_samp_wims"


# load data
dapi5c0bef5807d14cd0cd87a8058f30e5d2
Sys.setenv(DATABRICKS_TOKEN = "dapi5c0bef5807d14cd0cd87a8058f30e5d2")
Sys.getenv("DATABRICKS_TOKEN")
Sys.getenv("DATABRICKS_HOST")


warehouses <- db_sql_warehouse_list()

con <- dbConnect(
  drv = DatabricksSQL(),
  warehouse_id = "0a86ae40313bb7db",
  catalog = "prd_dash_lab")


#in Ben's code this is rss_dt
LSN_data <- dbGetQuery(con,
                       "SELECT * FROM prd_dash_lab.seda_unrestricted.ard_lsn_tbl_sample_wims")

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
  dplyr::select(
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
    year(version_start) %in% c(2024, 2025))
# here version start are all 2024
#here edit to only inlcude 2024 and 2025
#only 2 records have been removed

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
LSN_panel_filtered_long_filtered[, panel_year := as.integer(programme_year)]
#here is it under panel column: fixed_year or just a column with year?

###################################
### Add rbd and mncat to sites list
# Load GIS boundary data
path <- '/Volumes/prd_dash_lab/seda_restricted/ncea/analysis_ready_gis/EA_RBD.Rds'
file <- db_volume_read(path = path,
                       destination = tempfile())
ea_rbd <- readRDS(file)

path_mncat <- '/Volumes/prd_dash_lab/seda_restricted/ncea/analysis_ready_gis/EA_MNCAT.Rds'
file_mncat <- db_volume_read(path = path_mncat,
                       destination = tempfile())
ea_mncat <- readRDS(file_mncat)

path_eng_border <- '/Volumes/prd_dash_lab/seda_restricted/ncea/analysis_ready_gis/ENGLAND_BORDER.Rds'
file_eng_border <- db_volume_read(path = path_eng_border,
                             destination = tempfile())
sh_eng <- readRDS(file_eng_border)


# Crop ea_rbd/mcat to england shape (this tidies it up)
ea_rbd_crop <- st_intersection(ea_rbd, sh_eng)

# Merge Solway Tweed into Northumbria, and Dee into Northwest
st_geometry(ea_rbd[9,]) <- st_union(x=ea_rbd[1,], y=ea_rbd[9,], by_feature=TRUE) |> st_combine()
st_geometry(ea_rbd[4,]) <- st_union(x=ea_rbd[5,], y=ea_rbd[4,], by_feature=TRUE) |> st_combine()

# Drop ST & Dee
ea_rbd <- ea_rbd[-c(1, 5),]

# Which basins/catchments do sample points fall in
rss_rbd <- st_contains(ea_rbd, LSN_sites_filtered[, geometry])
rss_mncat <- st_contains(ea_mncat, LSN_sites_filtered[, geometry])

# Add to sample points data table
LSN_sites_filtered[, ea_rbd := contains_point_name(x=rss_rbd, n=ea_rbd$RIVER_BASIN_DISTRICT)]
LSN_sites_filtered[, ea_mncat := contains_point_name(x=rss_mncat, n=ea_mncat$mncat_id)]

###################################
### Join site data to main table
LSN_data_filtered[LSN_sites_filtered, on = "network_id",
       `:=`(ea_rbd = i.ea_rbd,
            ea_mncat = i.ea_mncat,
            site_status = i.status,
            replacement_site_flag = i.replacement_site_flag)]

###################################
### Sites planned to visit, but no data will not show in database.
# Need to cross check these against panel and sites.

panel_table <- LSN_panel_filtered_long_filtered[, .(panel, panel_year)] |> table() |> as.data.table()
names(panel_table)[names(panel_table) == "panel"] <- "panel_name"

# Get unique panel names
panel <- panel_table[, panel_name] |> unique()


#because i dont have current panel column, it is just panel and none of the sites have been rejected
# Get sites for each panel
LSN_panel_filtered_long_filtered <- lapply(panel,\(p) LSN_sites_filtered[panel == p, network_id])

LSN_panel_filtered_long_filtered <- split(
  LSN_sites_filtered$network_id,
  LSN_sites_filtered$panel)

#this returns a list of 8 with th NA lists, this is because LSN_sites_filtered has data from 2026, but LSN_panel_filtered_long_filtered has 2024 and 2026 only
#so i have just filtered to remove the 2026 data

panels_keep <- c(
  "Coupled 1",
  "Coupled 2",
  "Coupled 5",
  "Fixed",
  "Rotating 1",
  "Rotating 2")

LSN_panel_filtered_long_filtered <-
  LSN_panel_filtered_long_filtered[names(LSN_panel_filtered_long_filtered) %in% panels_keep]

#replace the spaces with '_'
names(LSN_panel_filtered_long_filtered) <-
  gsub(" ", "_", names(LSN_panel_filtered_long_filtered))

str(LSN_panel_filtered_long_filtered)

# Planned sites per year
#check if 'N > 0' is correct if not the object returned is empty
LSN_sites_panel_year <- lapply(param$years, \(x) panel_table[panel_year == x & N > 0, panel_name])
LSN_sites_planned_year <- lapply(1:length(param$years), \(x) do.call(c, LSN_panel_filtered_long_filtered[LSN_sites_panel_year[[x]]] |> unname()) |> as.data.table())

# Rename + setkey
lapply(LSN_sites_planned_year, \(x) setnames(x, "V1", "network_id"))
lapply(LSN_sites_planned_year, \(x) setkey(x, "network_id"))
# Add ea_rbd
lapply(LSN_sites_planned_year, \(x) x[LSN_data_filtered, ea_rbd := i.ea_rbd, on="network_id"])

#############################
# PART 2 - processes data
#############################

### Reshape data to wide by year, to identify missing values
# !! To exclude retired + replacements, add site_status!="Retired" & replacement_site_flag!="Yes"
LSN_wide <- lapply(c(2024, 2025), function(x) dcast(LSN_data_filtered[sample_year==x], network_id + ea_rbd ~ sample_month, value.var="meas_result", fun.aggregate=mean)) 
names(LSN_wide[[1]])
#this produces 2 data tables, a table each for 2024 and 2025

# columns
cols <- as.character(1:12)

# Find missing columns (if any)
missing_cols <- lapply(LSN_wide, \(x) setdiff(cols, names(x[, -c(1:2)])))

years_24_25 <- c(2024, 2025)

# Add any columns that are missing
for(i in seq_along(years)) {
  if(length(missing_cols[[i]]) > 0) {LSN_wide[[i]][, (missing_cols[[i]]) := as.numeric(NaN)]}
}
# Set col order
lapply(LSN_wide, \(x) setcolorder(x, c("network_id", "ea_rbd", cols)))

# Rename columns as dates
yr <- range(years_24_25)
d <- seq(as.Date(paste0(yr[1],"-01-01")), as.Date(paste0(yr[2],"-12-01")), by="month")

for(i in seq_along(years)) {
  x <- c(1:12) + 12 * (i-1)
  setnames(LSN_wide[[i]], cols, as.character(d[x]))
}

# col names (dates)
LSN_wide_names <- lapply(LSN_wide, \(x) names(x)[-c(1:2)])

# Convert NAN to NA
lapply(LSN_wide, \(x) for(col in names(x)) set(x, which(is.nan(x[[col]])), col, NA))

### Which sites are missing per year
sites_missing <- lapply(1:length(param$years), \(x) which(!(LSN_sites_planned_year[[x]][, network_id] %in% LSN_wide[[x]][, network_id])))

# Which sites have data, but were not planned to be visited
sites_visited_not_planned <- lapply(1:length(param$years), \(x) which(!(LSN_wide[[x]][, network_id] %in% LSN_sites_planned_year[[x]][, network_id])))

# Add flag for "extra sites" - those in the database, but were not supposed to be sampled that year
lapply(1:length(param$years), \(x) LSN_wide[[x]][, extra_site := FALSE])
lapply(1:length(param$years), \(x) LSN_wide[[x]][sites_visited_not_planned[[x]], extra_site := TRUE])

# Count number of sites with at least 1 visit (sample collection/data point)
sites_visited <- lapply(LSN_wide, \(x) x[extra_site == FALSE] |> nrow()) 

# Add sites with full missing data
LSN_wide <- lapply(1:length(param$years), \(x) rbindlist(list(LSN_wide[[x]][extra_site == FALSE], LSN_sites_planned_year[[x]][sites_missing[[x]],]), fill=TRUE))

# Table of sites
LSN_sites_table <- cbind(year=param$years, original_design=unlist(lapply(LSN_sites_planned_year, nrow)), planned=unlist(lapply(LSN_wide, nrow)), sampled_at_least_once=sites_visited, planned_not_sampled=unlist(lapply(LSN_wide, nrow)) - unlist(sites_visited))

#dont think i need this?
# Change missing columns for LSN to 0 value
#if(param$st=="LS" & param$years[1]==2023) {
#  LSN_wide[[1]][, LSN_wide_names[[1]][1:3] := as.numeric(0)]}

#############################
### Missing data at National and Regional (RBD) levels (by year)

# Count number of missing values per site per year
# missing value = NA
lapply(1:length(param$years), \(x) LSN_wide[[x]][, missing_count := apply(.SD, 1, \(x) length(which(is.na(x)))), .SDcols=LSN_wide_names[[x]]])

# Count number of missing data per month, national level
LSN_missing_nat <- lapply(1:length(param$years), \(x) LSN_wide[[x]][, lapply(.SD, \(x) length(which(is.na(x)))), .SDcols=LSN_wide_names[[x]]])
# as %
LSN_missing_nat_percent <- lapply(1:length(param$years), \(x) LSN_missing_nat[[x]] / nrow(LSN_wide[[x]]) * 100)
#RSn wide here names is wrong

