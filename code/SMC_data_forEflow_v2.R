# Paradigm Environmental: Cannabis E Flows
# Script to dynamically pull relevant stream monitoring datasets from SMC database
# Ideally pull all California-wide data available, but be able to subset regionally

# Current data types of interest:
# station information (latlong/county/region/reference status)
# bug taxonomy/capture prob data
# algal taxonomy
# phab data (flow/sediment related metrics)
# channel engineering (natural or engineerged channel type)

#### LOAD LIBRARIES, CONNECT TO SMC DATABASE ####

# libraries for data wrangling
library(tidyverse)
library(lubridate)
# libraries to connect with SMC database and query
library(DBI) 
library(dbplyr)
library(RPostgreSQL)
library(rstudioapi)

# smc login information/connection
# note that this should only be shared in-house
con <- dbConnect(
  PostgreSQL(),
  host = "192.168.1.17",
  dbname = 'smc',
  user = 'smcread',
  password = '1969$Harbor' 
)

#### QUERY DATA TABLES ####


# ----------------------------------------- SITE STATUS DATA ---------------------------------------

# for now just a placeholder
# can get Reference/nonref/Stressed from tblgismetrics ('sitestatus' column)

# ----------------------------------------- BUG TAXONOMY DATA ---------------------------------------

# this dataset provides stream site bug count/taxonomic ids
# because these datasets are so big, limiting data pull to North Coast for now (Region 1)

# # run this to see one example station dataset, with all available columns
# # if more columns should be added to large bug tax dataset, can add them to the 'bug_tax_sql' query
# bug_tax_sql_ex <- paste0("SELECT * FROM unified_taxonomy WHERE stationcode = 'SMC00027'")
# # pull data, writing to tibble
# bug_tax_example <- tbl(con, sql(bug_tax_sql_ex)) %>%
#   as_tibble()

# create SQL query for North Coast bug data pull
# this pulls subset of columns and aligns the data with station information
# then filters HUCs in the 100s to just get stations in Region 1
bug_tax_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
lu_stations.county, lu_stations.huc, lu_stations.smcshed, lu_stations.smc_lu, lu_stations.comid,
unified_taxonomy.sampledate, unified_taxonomy.agencycode, unified_taxonomy.replicate, unified_taxonomy.collectionmethodcode,
unified_taxonomy.targetorganismcount, unified_taxonomy.actualorganismcount, unified_taxonomy.finalid, unified_taxonomy.lifestagecode, unified_taxonomy.baresult
FROM lu_stations
INNER JOIN unified_taxonomy ON lu_stations.stationid = unified_taxonomy.stationcode
WHERE lu_stations.huc >99 AND lu_stations.huc < 200")

# run data query, pulling data into R environment/writing to tibble
bug_tax_1 <- tbl(con, sql(bug_tax_sql)) %>% 
  as_tibble() 

save(bug_tax_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_bmi_north_coast.csv")



# ----------------------------------------- BUG capture prob DATA ---------------------------------------

# calculated capture probabilities for OTUs are located in CSCI data tables (suppl1 oe)

# create SQL query, also just for North Coast stations for now
oe_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude, lu_stations.county, lu_stations.huc,
analysis_csci_suppl1_oe.sampleid, analysis_csci_suppl1_oe.otu, analysis_csci_suppl1_oe.captureprob, analysis_csci_suppl1_oe.meanobserved
FROM lu_stations
INNER JOIN analysis_csci_suppl1_oe ON lu_stations.stationid = analysis_csci_suppl1_oe.stationcode
WHERE lu_stations.huc >99 AND lu_stations.huc < 200")

# run query
oe_1 <- tbl(con, sql(oe_sql)) %>% 
  as_tibble() 

# # run example dataset, to see all columns available
# oe_sql_ex <- paste0("SELECT * FROM analysis_csci_suppl1_oe WHERE stationcode = 'SMC00027'")
# # pull data, writing to tibble
# oe_example <- tbl(con, sql(oe_sql_ex)) %>%
#   as_tibble()
# # names(oe_example)

save(oe_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_cap_prob_north_coast.csv")




# ----------------------------------------- ALGAE TAXONOMY DATA ---------------------------------------

# this dataset provides stream site algae count/taxonomic ids

# create SQL statement
# again for just North Coast stations
alg_tax_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
lu_stations.county, lu_stations.huc, 
unified_algae.sampledate, unified_algae.agencycode, unified_algae.replicate, unified_algae.collectionmethodcode, unified_algae.sampletypecode,
unified_algae.targetorganismcount, unified_algae.actualorganismcount, unified_algae.finalid,
unified_algae.lifestagecode, unified_algae.baresult, unified_algae.result
FROM lu_stations
INNER JOIN unified_algae ON lu_stations.stationid = unified_algae.stationcode
WHERE lu_stations.huc >99 AND lu_stations.huc < 200")

# run query
alg_tax_1 <- tbl(con, sql(alg_tax_sql)) %>% 
  as_tibble()

# # run example dataset, to see all columns available
# alg_tax_sql_ex <- paste0("SELECT * FROM unified_algae WHERE stationcode = 'SMC00027'")
# # pull data, writing to tibble
# alg_tax_example <- tbl(con, sql(alg_tax_sql_ex)) %>%
#   as_tibble()
# # names(alg_tax_example)

save(alg_tax_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_algae_north_coast.csv")



# ----------------------------------------- PHYSICAL HABITAT DATA ---------------------------------------


# this dataset contains physical habitat data
# list of analytenames requested, from Katie:
## Bankfull Height, Bankfull Width, Discharge, Bank Angle, Channelization, DischargeMeasurementMethod, DischargeMeasurementRating, 
# DominantSubstrate, Embeddedness,Glide/Pool Pool Variability, Glide/Pool Channel Sinuosity, Glide/Pool Pool Substrate,
# Hydraulic Height, Length Pool, Length Riffle,Length, Reach, Length, Segment, Riffle/Run Frequency, Riffle/Run Velocity/Depth Regime,
# Slope, Substrate Size Class, Velocity, Wetted Width,Width

# first pull all analytename names to see what would be useful
phab_analytes <- tbl(con, sql("SELECT DISTINCT analytename FROM sde.unified_phab")) %>% 
  as_tibble()

# for now, pulling all phab data related to 'Sediment' or 'Flow' or 'Velocity' analytenames
# also just for North Coast Region

# phab_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
# lu_stations.county, lu_stations.huc, 
# unified_phab.sampledate, unified_phab.sampleagencycode, unified_phab.targetlatitude, unified_phab.targetlongitude,
# unified_phab.collectionmethodcode, unified_phab.replicate, unified_phab.methodname, unified_phab.analytename,
# unified_phab.unitname, unified_phab.variableresult, unified_phab.result, unified_phab.resqualcode, unified_phab.qacode
# FROM lu_stations
# INNER JOIN unified_phab ON lu_stations.stationid = unified_phab.stationcode 
# AND ((unified_phab.analytename LIKE '%Sediment%' OR unified_phab.analytename LIKE '%Velocity%' OR unified_phab.analytename LIKE '%Flow%'))
# WHERE lu_stations.huc >99 AND lu_stations.huc < 200")


# updating data pull to include specific analytenames requested
# still just North Coast Region

phab_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
lu_stations.county, lu_stations.huc, 
unified_phab.sampledate, unified_phab.sampleagencycode, unified_phab.targetlatitude, unified_phab.targetlongitude,
unified_phab.collectionmethodcode, unified_phab.replicate, unified_phab.methodname, unified_phab.analytename,
unified_phab.unitname, unified_phab.variableresult, unified_phab.result, unified_phab.resqualcode, unified_phab.qacode
FROM lu_stations
INNER JOIN unified_phab ON lu_stations.stationid = unified_phab.stationcode 
AND ((unified_phab.analytename IN ('Bankfull Width', 'Bankfull Height', 'Bank Angle', 'Channelization', 'Discharge',
'DischargeMeasurementMethod',
'DischargeMeasurementRating', 'DominantSubstrate', 'Embeddedness', 'Glide/Pool Pool Variability', 'Glide/Pool Channel Sinuosity',
'Glide/Pool Pool Substrate', 'Hydraulic Height', 'Length Pool', 'Length Riffle', 'Length, Reach', 'Length, Segment',
'Riffle/Run Frequency', 'Riffle/Run Velocity/Depth Regime', 'Slope', 'Substrate Size Class', 'Velocity','Wetted Width', 'Width')))
WHERE lu_stations.huc >99 AND lu_stations.huc < 200")


phab_1 <- tbl(con, sql(phab_sql)) %>% 
  as_tibble()

unique(phab_1$analytename)

save(phab_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_phab_north_coast.csv")
# note that not all analytenames in sql query above appear in this imported North Coast dataset
# are some datatypes/analytenames not always collected? like Velocity and Discharge? 

# again, if want to see full example dataset without the filters
phab_sql_ex <- paste0("SELECT * FROM unified_phab WHERE stationcode = 'SMC00027'")
phab_example <- tbl(con, sql(phab_sql_ex)) %>%
  as_tibble()
# names(phab_example)




# ----------------------------------------- CHANNEL ENGINEERING DATA ---------------------------------------

# data on channel type (engineered, natural, etc)
# categorical data basically

# pulling all data, as not large dataset (I think there is just data for sites in the SMC region?)

chaneng_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
lu_stations.county, lu_stations.huc, 
unified_channelengineering.sampledate, unified_channelengineering.channeltype, unified_channelengineering.rightsideofstructure,
unified_channelengineering.leftsideofstructure, unified_channelengineering.bottom, unified_channelengineering.bottomcomments
FROM lu_stations
INNER JOIN unified_channelengineering ON lu_stations.stationid = unified_channelengineering.stationcode")

chaneng_1 <- tbl(con, sql(chaneng_sql)) %>% 
  as_tibble()

# again, if want to see full example dataset without the filters
chaneng_sql_ex <- paste0("SELECT * FROM unified_channelengineering WHERE stationcode = 'SMC00027'")
chaneng_example <- tbl(con, sql(chaneng_sql_ex)) %>%
  as_tibble()
# names(chaneng_example)

