#from Ben B's repository https://github.com/Environment-Agency-Gov/ncea-das-network_eval-missing_data/blob/main/scripts/rss_wims_missing_data.r 
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
library(here)

# Load functions 
source("/mnt/workbench/home/GKirthana.Pillay/LSN_missingness_wims/Scripts/missing_data_functions.r")

### RSS/RSN/SSN data parameters
#st= suite type/network here it is LS
#dc= det_code but not included atm
#fn = prefix for output file names
#fnn = the name of the variable, used in file names and figures.
#years = sampling years/which years to get missing data for

param <- list(st="LS", dc="Ammoniacal Nitrogen, Filtered as N", fn="lsn_wims_", fnn="Nitrogen", years=2024:2025)

#establish connectioin to databricks

warehouses <- db_sql_warehouse_list()

con <- dbConnect(
  drv = DatabricksSQL(),
  warehouse_id = "0a86ae40313bb7db",
  catalog = "prd_dash_lab")

#SQL code here to import the relevant dataset from databricks
LSN_data <- dbGetQuery(con,
                       "SELECT * FROM prd_dash_lab.seda_unrestricted.ard_lsn_tbl_sample_wims")

#Filter the data based on network id (but since this network is Lakes only it should be okay)
LSN_data_filtered <- LSN_data %>%
  filter(
    startsWith(suite_type, param$st),
    det_desc == param$dc) %>%
  dplyr::select(
    suite_type,
    det_desc,
    network_id,
    sample_datetime,
    meas_sign,
    meas_result)


## Site data
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
    version_start) %>%
  filter(
    startsWith(network_id, param$st),
    year(version_start) %in% c(2024, 2025))
# here version start are all 2024
#here edit to only inlcude 2024 and 2025
#only 2 records have been removed

#For panel information extract required data from LSN_sites 
#columns required

LSN_panel<- LSN_sites %>%
  dplyr::select(network_id,
                panel,
                version_start,
                year)

LSN_panel_filtered <- LSN_panel %>%
  mutate(
    panel = gsub(" ", "_", panel)) %>%
  separate_rows(year, sep = ",\\s*") %>%
  mutate(programme_year = c(Y1 = "2024", Y2 = "2025", Y3 = "2026", Y4 = "2027", Y5 = "2028")[year])

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
LSN_panel_filtered <- as.data.table(LSN_panel_filtered)
LSN_panel_filtered[, panel_year := as.integer(programme_year)]


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
LSN_rbd <- st_contains(ea_rbd, LSN_sites_filtered[, geometry])
LSN_mncat <- st_contains(ea_mncat, LSN_sites_filtered[, geometry])

# Add to sample points data table
LSN_sites_filtered[, ea_rbd := contains_point_name(x=LSN_rbd, n=ea_rbd$RIVER_BASIN_DISTRICT)]
LSN_sites_filtered[, ea_mncat := contains_point_name(x=LSN_mncat, n=ea_mncat$mncat_id)]

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

panel_table <- LSN_panel_filtered[, .(panel, panel_year)] |> table() |> as.data.table()
names(panel_table)[names(panel_table) == "panel"] <- "panel_name"

# Get unique panel names
panel <- panel_table[, panel_name] |> unique()

#because i dont have current panel column, it is just panel and none of the sites have been rejected
# Get sites for each panel
LSN_panel_filtered <- lapply(panel,\(p) LSN_sites_filtered[panel == p, network_id])

LSN_panel_filtered <- split(
  LSN_sites_filtered$network_id,
  LSN_sites_filtered$panel)

#replace the spaces with '_'
names(LSN_panel_filtered) <-
  gsub(" ", "_", names(LSN_panel_filtered))

#str(LSN_panel_filtered)

# Planned sites per year
#check if 'N > 0' is correct if not the object returned is empty
LSN_sites_panel_year <- lapply(param$years, \(x) panel_table[panel_year == x & N >0, panel_name])
LSN_sites_planned_year <- lapply(1:length(param$years), \(x) do.call(c, LSN_panel_filtered[LSN_sites_panel_year[[x]]] |> unname()) |> as.data.table())

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
LSN_wide <- lapply(param$years, function(x) dcast(LSN_data_filtered[sample_year==x], network_id + ea_rbd ~ sample_month, value.var="meas_result", fun.aggregate=mean)) 
names(LSN_wide[[1]])
names(LSN_wide[[2]]) 

# columns
cols <- as.character(1:12)

# Find missing columns (if any)
missing_cols <- lapply(LSN_wide, \(x) setdiff(cols, names(x[, -c(1:2)])))

# Add any columns that are missing
for(i in seq_along(years)) {
  if(length(missing_cols[[i]]) > 0) {LSN_wide[[i]][, (missing_cols[[i]]) := as.numeric(NaN)]}
}
# Set months in correct order
lapply(LSN_wide, \(x) setcolorder(x, c("network_id", "ea_rbd", cols)))

# Rename columns as dates
yr <- range(param$years)
d <- seq(as.Date(paste0(yr[1],"-01-01")), as.Date(paste0(yr[2],"-12-01")), by="month")

for(i in 1:length(param$years)) {
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

# Change missing columns for SSN to 0 value
if(param$st=="LS" & param$years[1]==2024) {
  LSN_wide[[1]][, LSN_wide_names[[1]][1:3] := as.numeric(0)]
}

#############################
### Missing data at National and Regional (RBD) levels (by year)
# Count number of missing values per site per year
# missing value = NA
lapply(1:length(param$years), \(x) LSN_wide[[x]][, missing_count := apply(.SD, 1, \(x) length(which(is.na(x)))), .SDcols=LSN_wide_names[[x]]])

# Count number of missing data per month, national level
LSN_missing_nat <- lapply(1:length(param$years), \(x) LSN_wide[[x]][, lapply(.SD, \(x) length(which(is.na(x)))), .SDcols=LSN_wide_names[[x]]])
# as percentage %
LSN_missing_nat_percent <- lapply(1:length(param$years), \(x) LSN_missing_nat[[x]] / nrow(LSN_wide[[x]]) * 100)

# Total missing data per month
LSN_missing_nat_totals <- rbindlist(LSN_missing_nat, use.names = FALSE) |> colSums() 
names(LSN_missing_nat_totals) <- format(ISOdate(2010,1:12, 1),"%b")

# as percentage %
planned_sites <- LSN_sites_table[,2] |> unlist() |> sum()

# FOR LSN, planned sites per month (since jan - mar was not done in 2024)
if(param$st=="LS" & param$years[1]==2024) {
  planned_sites <- c(rep(planned_sites - unlist(LSN_sites_table[,2][1]), times=3), rep(planned_sites, times=9))}

# Overall missing data per month
LSN_missing_nat_totals_pc <- LSN_missing_nat_totals / planned_sites * 100

## Count number of missing data per month per RBD
LSN_missing_rbd <- lapply(1:length(param$years), \(x) LSN_wide[[x]][, lapply(.SD, \(x) length(which(is.na(x)))), by=ea_rbd, .SDcols=LSN_wide_names[[x]]] |> setkey("ea_rbd"))

# DROP rows with NA values (some sites do not pick up an RBD for some reason)
LSN_missing_rbd <- lapply(LSN_missing_rbd, \(x) x[!is.na(ea_rbd), ])

# add total number of sites per RBD
lapply(1:length(param$years), \(x) LSN_missing_rbd[[x]][LSN_wide[[x]][, .N, by=ea_rbd], sites_total := i.N, on="ea_rbd"])

# as percentage
LSN_missing_rbd_percent <- lapply(1:length(param$years), \(x) LSN_missing_rbd[[x]][, .SD, .SDcols=LSN_wide_names[[x]]] / LSN_missing_rbd[[x]][, sites_total] * 100) 

# Add yearly mean
lapply(1:length(param$years), \(x) LSN_missing_rbd_percent[[x]][, yearly_mean := apply(.SD, 1, mean)])

# add rbd back
lapply(1:length(param$years), \(x) LSN_missing_rbd_percent[[x]][, ea_rbd := LSN_missing_rbd[[x]][, ea_rbd]])

#############################
### Individual missing table ranked

# Create missing totals for individual sites
LSN_missing_all <- lapply(LSN_wide, \(x) x[, .(network_id, ea_rbd, missing_count)]) |> rbindlist() |> melt(id.vars=c("network_id", "ea_rbd")) |> dcast(network_id + ea_rbd ~ variable, fun.aggregate = sum, na.rm=TRUE)
# Add missing as percentage
# counts 
site_counts <- lapply(LSN_wide, \(x) x[, network_id]) |> unlist() |> as.character() |> table() |> as.data.table()
setnames(site_counts, c("V1", "N"), c("network_id", "n_years"))
# Join tables
LSN_missing_all[site_counts, n_years := i.n_years, on="network_id"]
# % (multiply years by 12 to work out)
LSN_missing_all[, missing_count_pc := round(missing_count / (n_years * 12) * 100, 1)]

# Add site status
LSN_missing_all[LSN_sites_filtered, site_status := i.status, on="network_id"]

# Format as table and write to file
LSN_site_miss_table <- LSN_missing_all[, .(network_id, site_status, ea_rbd, n_years, missing_count, missing_count_pc)][order(missing_count_pc, decreasing = TRUE)]

#write to a .csv file in outputs folder
fwrite(LSN_site_miss_table, file=here("Outputs", paste0(param$fn, tolower(param$fnn), "_missing_data_", yr[1], "-", yr[2], ".csv")))


