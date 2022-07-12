# Paradigm Environmental: Cannabis E Flows
# Script to dynamically pull relevant stream monitoring datasets from SMC database
# Ideally pull all California-wide data available, but be able to subset regionally

# Current data types of interest:
# station information (latlong/county/region/reference status)
# bug taxonomy/capture prob data
# algal taxonomy
# phab data (flow/sediment related metrics)
# channel engineering (natural or engineerged channel type)

# Note: we discovered some physical habitat data gaps in the SMC database
# so for now, decided to do a manual data download from CEDEN website as database is still being updated
# website:
# https://ceden.waterboards.ca.gov/AdvancedQueryTool
# will then import that phab data from local location

getwd()
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
# because these datasets are so big, limiting first data pull to North Coast for now (Region 1)

# # run this to see one example station dataset, with all available columns
# # if more columns should be added to large bug tax dataset, can add them to the 'bug_tax_sql' query
# bug_tax_sql_ex <- paste0("SELECT * FROM unified_taxonomy WHERE stationcode = 'SMC00027'")
# # pull data, writing to tibble
# bug_tax_example <- tbl(con, sql(bug_tax_sql_ex)) %>%
#   as_tibble()

# create SQL query for North Coast bug data pull
# this pulls subset of columns and aligns the data with station information

# # then filters HUCs in the 100s to just get stations in Region 1
# bug_tax_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
# lu_stations.county, lu_stations.huc, lu_stations.smcshed, lu_stations.smc_lu, lu_stations.comid,
# unified_taxonomy.sampledate, unified_taxonomy.agencycode, unified_taxonomy.replicate, unified_taxonomy.collectionmethodcode,
# unified_taxonomy.targetorganismcount, unified_taxonomy.actualorganismcount, unified_taxonomy.finalid, unified_taxonomy.lifestagecode, unified_taxonomy.baresult
# FROM lu_stations
# INNER JOIN unified_taxonomy ON lu_stations.stationid = unified_taxonomy.stationcode
# WHERE lu_stations.huc >99 AND lu_stations.huc < 200")
# 
# # run data query, pulling data into R environment/writing to tibble
# bug_tax_1 <- tbl(con, sql(bug_tax_sql)) %>%
#   as_tibble()
# 
# save(bug_tax_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_bmi_north_coast.csv")



# for all of California 
bug_tax_sql_ca <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
lu_stations.county, lu_stations.huc, lu_stations.smcshed, lu_stations.smc_lu, lu_stations.comid,
unified_taxonomy.sampledate, unified_taxonomy.agencycode, unified_taxonomy.replicate, unified_taxonomy.collectionmethodcode,
unified_taxonomy.targetorganismcount, unified_taxonomy.actualorganismcount, unified_taxonomy.finalid, unified_taxonomy.lifestagecode, unified_taxonomy.baresult
FROM lu_stations
INNER JOIN unified_taxonomy ON lu_stations.stationid = unified_taxonomy.stationcode")

# run data query, pulling data into R environment/writing to tibble
bug_tax_ca <- tbl(con, sql(bug_tax_sql_ca)) %>% 
  as_tibble() 

save(bug_tax_ca, file="input_data/SMC_bmi_cali.csv")
load(file = "input_data/SMC_bmi_cali.csv")
head(bug_tax_ca)

## do we have expected vs observed species?


# CSCI data pull ----------------------------------------------------------

## stations pull
lu_stations_ca <- tbl(con, sql("SELECT * FROM lu_stations")) %>% 
  as_tibble() 
head(lu_stations_ca)

# run data query, pulling data into R environment/writing to tibble
bug_csci_ca <- tbl(con, sql("SELECT * FROM analysis_csci_core")) %>% 
  as_tibble() 
head(bug_csci_ca)

object.size(bug_csci_ca)
write.csv(bug_csci_ca, "ignore/csci_ca_scores.csv")

## example cross walk
lu_bug_test <- bug_csci_ca %>%
  inner_join(lu_stations_ca, by=c("stationcode"="stationid")) ## join with masterid to datasets

### component metrics

bug_oe_ca <- tbl(con, sql("SELECT * FROM analysis_csci_suppl1_oe")) %>%  ##analysis_csci_suppl1_mmi, analysis_csci_suppl1_grps, 
  # analysis_csci_suppl2_mmi, analysis_csci_suppl2_oe, analysis_phab_ipi, analysis_phab_metrics
  as_tibble() 
head(bug_oe_ca)


# ----------------------------------------- BUG capture prob DATA ---------------------------------------

# calculated capture probabilities for OTUs are located in CSCI data tables (suppl1 oe)

# # create SQL query, also just for North Coast stations for now
# oe_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude, lu_stations.county, lu_stations.huc,
# analysis_csci_suppl1_oe.sampleid, analysis_csci_suppl1_oe.otu, analysis_csci_suppl1_oe.captureprob, analysis_csci_suppl1_oe.meanobserved
# FROM lu_stations
# INNER JOIN analysis_csci_suppl1_oe ON lu_stations.stationid = analysis_csci_suppl1_oe.stationcode
# WHERE lu_stations.huc >99 AND lu_stations.huc < 200")
# 
# # run query
# oe_1 <- tbl(con, sql(oe_sql)) %>% 
#   as_tibble() 
# 
# # # run example dataset, to see all columns available
# # oe_sql_ex <- paste0("SELECT * FROM analysis_csci_suppl1_oe WHERE stationcode = 'SMC00027'")
# # # pull data, writing to tibble
# # oe_example <- tbl(con, sql(oe_sql_ex)) %>%
# #   as_tibble()
# # # names(oe_example)
# 
# save(oe_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_cap_prob_north_coast.csv")


# for all of California
oe_sql_ca <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude, lu_stations.county, lu_stations.huc,
analysis_csci_suppl1_oe.sampleid, analysis_csci_suppl1_oe.otu, analysis_csci_suppl1_oe.captureprob, analysis_csci_suppl1_oe.meanobserved
FROM lu_stations
INNER JOIN analysis_csci_suppl1_oe ON lu_stations.stationid = analysis_csci_suppl1_oe.stationcode")

# run query
oe_ca <- tbl(con, sql(oe_sql_ca)) %>% 
  as_tibble() 


save(oe_ca, file="input_data/SMC_cap_prob_cali.csv")


# ----------------------------------------- ALGAE TAXONOMY DATA ---------------------------------------

# this dataset provides stream site algae count/taxonomic ids

# # create SQL statement
# # again for just North Coast stations
# alg_tax_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
# lu_stations.county, lu_stations.huc, 
# unified_algae.sampledate, unified_algae.agencycode, unified_algae.replicate, unified_algae.collectionmethodcode, unified_algae.sampletypecode,
# unified_algae.targetorganismcount, unified_algae.actualorganismcount, unified_algae.finalid,
# unified_algae.lifestagecode, unified_algae.baresult, unified_algae.result
# FROM lu_stations
# INNER JOIN unified_algae ON lu_stations.stationid = unified_algae.stationcode
# WHERE lu_stations.huc >99 AND lu_stations.huc < 200")
# 
# # run query
# alg_tax_1 <- tbl(con, sql(alg_tax_sql)) %>% 
#   as_tibble()
# 
# # # run example dataset, to see all columns available
# # alg_tax_sql_ex <- paste0("SELECT * FROM unified_algae WHERE stationcode = 'SMC00027'")
# # # pull data, writing to tibble
# # alg_tax_example <- tbl(con, sql(alg_tax_sql_ex)) %>%
# #   as_tibble()
# # # names(alg_tax_example)
# 
# save(alg_tax_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_algae_north_coast.csv")
# 

# for all of California
alg_tax_sql_ca <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
lu_stations.county, lu_stations.huc, 
unified_algae.sampledate, unified_algae.agencycode, unified_algae.replicate, unified_algae.collectionmethodcode, unified_algae.sampletypecode,
unified_algae.targetorganismcount, unified_algae.actualorganismcount, unified_algae.finalid,
unified_algae.lifestagecode, unified_algae.baresult, unified_algae.result
FROM lu_stations
INNER JOIN unified_algae ON lu_stations.stationid = unified_algae.stationcode")

# run query
alg_tax_ca <- tbl(con, sql(alg_tax_sql_ca)) %>% 
  as_tibble()

save(alg_tax_ca, file="input_data/SMC_algae_cali.RData")

# ASCI data pull ----------------------------------------------------------

## stations pull
lu_stations_ca <- tbl(con, sql("SELECT * FROM lu_stations")) %>% 
  as_tibble() 
head(lu_stations_ca)


# run data query, pulling data into R environment/writing to tibble
alg_asci_ca <- tbl(con, sql("SELECT * FROM analysis_asci")) %>% 
  as_tibble() 
head(alg_asci_ca)

object.size(alg_asci_ca)
write.csv(alg_asci_ca, "ignore/asci_ca_scores.csv")

## example cross walk
lu_alg_test <- alg_asci_ca %>%
  inner_join(lu_stations_ca, by=c("stationcode"="stationid")) ## join with masterid to datasets

head(lu_alg_test)

# ----------------------------------------- PHYSICAL HABITAT DATA ---------------------------------------


# this dataset contains physical habitat data
# list of analytenames requested, from Katie:
## Bankfull Height, Bankfull Width, Discharge, Bank Angle, Channelization, DischargeMeasurementMethod, DischargeMeasurementRating, 
# DominantSubstrate, Embeddedness,Glide/Pool Pool Variability, Glide/Pool Channel Sinuosity, Glide/Pool Pool Substrate,
# Hydraulic Height, Length Pool, Length Riffle,Length, Reach, Length, Segment, Riffle/Run Frequency, Riffle/Run Velocity/Depth Regime,
# Slope, Substrate Size Class, Velocity, Wetted Width,Width

# # first pull all analytename names to see what would be useful
# phab_analytes <- tbl(con, sql("SELECT DISTINCT analytename FROM sde.unified_phab")) %>% 
#   as_tibble()
# 
# # for now, pulling all phab data related to 'Sediment' or 'Flow' or 'Velocity' analytenames
# # also just for North Coast Region
# 
# # phab_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
# # lu_stations.county, lu_stations.huc, 
# # unified_phab.sampledate, unified_phab.sampleagencycode, unified_phab.targetlatitude, unified_phab.targetlongitude,
# # unified_phab.collectionmethodcode, unified_phab.replicate, unified_phab.methodname, unified_phab.analytename,
# # unified_phab.unitname, unified_phab.variableresult, unified_phab.result, unified_phab.resqualcode, unified_phab.qacode
# # FROM lu_stations
# # INNER JOIN unified_phab ON lu_stations.stationid = unified_phab.stationcode 
# # AND ((unified_phab.analytename LIKE '%Sediment%' OR unified_phab.analytename LIKE '%Velocity%' OR unified_phab.analytename LIKE '%Flow%'))
# # WHERE lu_stations.huc >99 AND lu_stations.huc < 200")
# 
# 
# # updating data pull to include specific analytenames requested
# # still just North Coast Region
# 
# phab_sql <- paste0("SELECT lu_stations.masterid, lu_stations.latitude, lu_stations.longitude,
# lu_stations.county, lu_stations.huc, 
# unified_phab.sampledate, unified_phab.sampleagencycode, unified_phab.targetlatitude, unified_phab.targetlongitude,
# unified_phab.collectionmethodcode, unified_phab.replicate, unified_phab.methodname, unified_phab.analytename,
# unified_phab.unitname, unified_phab.variableresult, unified_phab.result, unified_phab.resqualcode, unified_phab.qacode
# FROM lu_stations
# INNER JOIN unified_phab ON lu_stations.stationid = unified_phab.stationcode 
# AND ((unified_phab.analytename IN ('Bankfull Width', 'Bankfull Height', 'Bank Angle', 'Channelization', 'Discharge',
# 'DischargeMeasurementMethod',
# 'DischargeMeasurementRating', 'DominantSubstrate', 'Embeddedness', 'Glide/Pool Pool Variability', 'Glide/Pool Channel Sinuosity',
# 'Glide/Pool Pool Substrate', 'Hydraulic Height', 'Length Pool', 'Length Riffle', 'Length, Reach', 'Length, Segment',
# 'Riffle/Run Frequency', 'Riffle/Run Velocity/Depth Regime', 'Slope', 'Substrate Size Class', 'Velocity','Wetted Width', 'Width')))
# WHERE lu_stations.huc >99 AND lu_stations.huc < 200")
# 
# 
# phab_1 <- tbl(con, sql(phab_sql)) %>% 
#   as_tibble()
# 
# unique(phab_1$analytename)
# 
# save(phab_1, file="/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Projects/Cannabis/Methods/data/SMC_phab_north_coast.csv")
# # note that not all analytenames in sql query above appear in this imported North Coast dataset
# # are some datatypes/analytenames not always collected? like Velocity and Discharge? 
# 
# # again, if want to see full example dataset without the filters
# phab_sql_ex <- paste0("SELECT * FROM unified_phab WHERE stationcode = 'SMC00027'")
# phab_example <- tbl(con, sql(phab_sql_ex)) %>%
#   as_tibble()
# # names(phab_example)


# ----------------------------------------- PHYSICAL HABITAT DATA, STATIC ---------------------------------------

# importing California-wide phab dataset from local location, originally queried from CEDEN website
# using same list of analyte names as before
# can eventually update above phab database query, but while there are still datagaps use this file instead

# import from text file, tab separated
phab_raw_ca <- read_tsv("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/input_data/ceden_data_retrieval_202233116121.txt")

# also import lu stations, so can add latitude/longitude/masterid/huc/county info
lu_stations <- tbl(con, sql("SELECT masterid, stationid, latitude, longitude, county, huc from lu_stations")) %>%
  as_tibble()


phab_ca <- phab_raw_ca %>% 
  rename_all(tolower) %>% 
  select(stationcode, sampledate, sampleagency, targetlatitude, targetlongitude, collectionmethodname, collectionreplicate,
         methodname, analyte, unit, result, variableresult, resultqualcode, qacode) %>% 
  # conforming names so match names of columns from database
  rename(sampleagencycode = sampleagency, collectionmethodcode = collectionmethodname, replicate = collectionreplicate,
         analytename = analyte, unitname = unit, resqualcode = resultqualcode) %>% 
  # getting masterids/lat/long/county/huc used in smc database, inner join so only retain sites with masterid
  inner_join(lu_stations, by = c("stationcode" = "stationid")) %>% 
  select(masterid, latitude, longitude, county, huc,  sampledate, sampleagencycode, targetlatitude, targetlongitude, collectionmethodcode,
         replicate,
         methodname, analytename, unitname, result, variableresult, resqualcode, qacode)


save(phab_ca, file="input_data/SMC_phab_cali.csv")

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


save(chaneng_1, file="input_data/SMC_channel_eng_cali.csv")
# again, if want to see full example dataset without the filters
chaneng_sql_ex <- paste0("SELECT * FROM unified_channelengineering WHERE stationcode = 'SMC00027'")
chaneng_example <- tbl(con, sql(chaneng_sql_ex)) %>%
  as_tibble()
# names(chaneng_example)
chaneng_example
