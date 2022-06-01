#' Script to assemble/calculate functional flow metrics and DeltaH for North Coast Paired Gages
#' Annie Holt, 6/1/2022
#' 

# Direction from Kris:
# Shapefile of USGS gages matched to bioassessment sites/geomorphic sites
    # C:\Users\KristineT.SCCWRP2K\SCCWRP\SD Hydro Vulnerability Assessment - General\Data\SpatialData\PairedGages_BioGeomorph_Katie
# Identify which gages we already calculated DeltaH values for RB9 project
    # Check reference and impaired gage database
        # Reference DeltaFFM: data frame that you developed
        # Impaired deltaFFM:
            #C:/Users/KristineT.SCCWRP2K/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/RawData/Data_for_SDSU/R/output_02/2021-07-27_RF_input.csv
                  # Delta columns: 271:286
                  # Has different delta names --> lookup table for FFM (in github repo data folder)
                  # C:\Users\KristineT.SCCWRP2K\Documents\Git\RB9_FFM_RF\data\04_data\all_metric_def_list_FFMs_v2.csv
# Subset DeltaH values for those already calculated into a dataframe for Cannabis study
# For gages where we haven't calculated DeltaH, go ahead and calculate FFM and deltaH based on your previous code
# Compile entire dataframe for Cannabis study --> save as csv file


#' NOTE: to calculate ffms/deltah for list of comids/gages, can reference Donny's Impaired_FFM_deltah.R script (RB9 FFM RF Repository)
#' Annie did not complete this step yet, so far just retrieved data we already created


### Import libraries
{
  # required packages
  library(dplyr) # for data wrangling
  library(purrr) # for iterating over lists and applying functions
  library(glue) # good for pasting things together
  library(fs) # for working with directories and files
  library(tictoc) # timing stuff
  library(here) # helps with setting home directory/path *DhK: I personally don't like using this package.
  library(stringr) # for working with strings
  library(tidyr)
  library(tidyverse)
  library(sf)
}

### Customized version of package ==============================================
# Original ffc package would be: library(ffcAPIClient)
# Due to some constraints,we are going to use customized version of package locally installed.

# Annie had used this package in the past based on Donny's recommendation, as it helps iterate the ffc across sites
# but this script does not yet use it, will have to go back to Annie/Donny's scripts in RB9 FFM RF Repository to finish this task

library(devtools)
# change paths depending on user
build(paste(SDHydro_path, "/R/ffcAPIClient_custom", sep = "")) # build into compress file.
install_local(paste(SDHydro_path, "/R/ffcAPIClient.custom_1.0.tar.gz", sep = ""), repos = NULL, type = "source") # local install
library(ffcAPIClient.custom) # load cutomized package
# detach(package:ffcAPIClient.custom, unload = TRUE) # Just in case if you want to get rid of locally installed package

# set ffctoken, I included my token in the code, but PLEASE change it when you're actually running this code!
# Annie's token
ffctoken <- ffcAPIClient::set_token("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBbm5lIiwibGFzdE5hbWUiOiJIb2x0IiwiZW1haWwiOiJhZ2hvbHRAdWNkYXZpcy5lZHUiLCJyb2xlIjoiVVNFUiIsImlhdCI6MTYwNTI5NTQzMn0.wzbBa5rLs6DAvEpUQVVLmQx23g2EBgEQhM2wcqyweUc")


#### Data Prepping ####

# import dataset of USGS gages match to bioassessment sites/geomorphic sites
# this is a shapefile, but can convert to dataframe
paired_gages <- st_read("C:/Users/anneh/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/PairedGages_BioGeomorph_Katie/02_selected_gages_may132022.shp") %>% 
  as.data.frame() %>% 
  select(-geometry)

# location of previously calculated deltaH values for RB9 project (also location of custom packages etc)
SDHydro_path <- "C:/Users/anneh/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/RawData/Data_for_SDSU"


# checking previous deltaH datasets before calculating new FFMs
# importing reference dataset
reference_deltah <- read_csv(paste(SDHydro_path, "/R/output_XX/ffc_results_run_2022-04-27_delta_value.csv", sep = ""))
# imparied dataset
impaired_deltah <- read_csv(paste(SDHydro_path, "/R/output_02/2021-07-27_RF_input.csv", sep = ""))
# imparied dataset naming conventions
impaired_deltah_def <- read_csv("C:/Users/anneh/Documents/Repositories/RB9_FFM_RF/data/all_metric_def_list_FFMs_v2.csv")
# final name list
impaired_deltah_ffm_names <- impaired_deltah_def$flow_metric2 %>% na.omit()


# creating dataframe of gages where deltah values already calculated
paired_gages_1 <- paired_gages %>% 
  mutate(ref_check = case_when(COMID %in% reference_deltah$comid ~ "yes",
                               TRUE ~ "no")) %>% 
  mutate(impaired_check = case_when(COMID %in% impaired_deltah$comid ~ "yes",
                               TRUE ~ "no")) %>% 
  filter(ref_check == "yes" | impaired_check == "yes")


# creating dataframe of gages where need to run ffc
paired_gages_2 <- paired_gages %>% 
  mutate(ref_check = case_when(COMID %in% reference_deltah$comid ~ "yes",
                               TRUE ~ "no")) %>% 
  mutate(impaired_check = case_when(COMID %in% impaired_deltah$comid ~ "yes",
                                    TRUE ~ "no")) %>% 
  filter(ref_check == "no" & impaired_check == "no") %>% 
  mutate(COMID = as.character(COMID))

# Making a list. It will be used for running multiple FFCs in parallel.  
# COMID_list <- as.character(paired_gages_2$COMID)


#### DATA ASSEMBLY (gages we already have data for) ####

# assembling deltaH datasets for those gages
# first for gages in reference dataset
paired_gages_deltah_1a <- paired_gages_1 %>% 
  filter(ref_check == "yes") %>% 
  select(-ref_check, -impaired_check, -COMID1) %>% 
  left_join(reference_deltah, by = c("COMID" = "comid")) %>% 
  rename(wayr = year)

# then for gages in imparied dataset, have to conform names/info
paired_gages_deltah_1b <- paired_gages_1 %>% 
  filter(impaired_check == "yes") %>% 
  select(-ref_check, -impaired_check, -COMID1) %>% 
  # joining deltah ffm data to list of comids, only selected ffm columns not the excess
  left_join(impaired_deltah, by = c("COMID" = "comid")) %>% 
  # drop weird format column
  select(-"...1") %>% 
  # select only flow metric columns, have a d in front, in addition to other identifying columns
  select(Name, Filename, ID, gagelat, gagelong, COMID, HUC_12, gageid, comid_wy, wayr, wyt, paste("d_", impaired_deltah_ffm_names, sep = ""))


# combining into one deltah file
paired_gages_deltah_1 <- paired_gages_deltah_1a %>% 
  mutate(database = "reference") %>% 
  bind_rows(paired_gages_deltah_1b %>% mutate(database = "impaired"))


# assembling q99 datasets for those gages
reference_imparied_q99 <- read_csv(paste(SDHydro_path, "/R/output_XX/reference_impaired_q99.csv", sep = ""))

paired_gages_q99_1 <- reference_imparied_q99 %>% 
  filter(comid %in% paired_gages_1$COMID) %>% 
  rename(q99 = Q99) %>% 
  mutate(wyt = tolower(wyt))



# ffms
# reference dataset
reference_gage_ffms<- read_csv(paste(SDHydro_path, "/R/output_XX/ffc_results_run_2022-04-27_delta_ready.csv", sep = ""))
  
# importing reference dataset
ref_FFM <- read.csv(paste(SDHydro_path, "/reference_database/ref_percentiles_codefornature/Functional_Flows_Data.csv", sep = ""))
# selecting only columns of interest
ref_FFM <- ref_FFM %>% select(comid, ffm, wyt, p50) 



# using reference dataset to back calculate to get original FFMs for the reference gages
paired_gages_ffm_1a <- paired_gages_deltah_1a %>% 
  pivot_longer(-c(Name, Filename, ID, gagelat, gagelong, COMID, HUC_12, gageid, comid_wy, wayr, wyt),
               names_to = "d_ffm", values_to = "delta") %>%
  mutate(ffm = substring(d_ffm, 3)) %>% 
  left_join(ref_FFM, by = c("COMID" = "comid", "wyt", "ffm")) %>% 
  rename(reference_result = p50) %>% 
  # delta = result - reference_result
  mutate(result = delta + reference_result) %>% 
  # drop extra columns
  select(-reference_result, -delta, -d_ffm) %>% 
  pivot_wider(names_from = ffm, values_from = result)


# using reference dataset to back calculate to get original FFMs for the imparied gages
paired_gages_ffm_1b <- paired_gages_deltah_1b %>% 
  pivot_longer(-c(Name, Filename, ID, gagelat, gagelong, COMID, HUC_12, gageid, comid_wy, wayr, wyt),
               names_to = "d_ffm", values_to = "delta") %>%
  mutate(ffm = substring(d_ffm, 3)) %>% 
  left_join(ref_FFM, by = c("COMID" = "comid", "wyt", "ffm")) %>% 
  rename(reference_result = p50) %>% 
  # delta = result - reference_result
  mutate(result = delta + reference_result) %>% 
  # drop extra columns
  select(-reference_result, -delta, -d_ffm) %>% 
  pivot_wider(names_from = ffm, values_from = result)

# combining ffms datasets 
paired_gages_ffm_1 <-  paired_gages_ffm_1a %>% 
  mutate(database = "reference") %>% 
  bind_rows(paired_gages_ffm_1b %>% mutate(database = "impaired"))


#### EXPORT (gages we already have data for) ####

Cannabis_Eflows_path <- "C:/Users/anneh/SCCWRP/Cannabis E-Flows - Data/RawData/Paired_Gages/FFMs"


# write_csv(paired_gages_ffm_1, paste(Cannabis_Eflows_path, "/PairedGages_FFMs_pt1.csv", sep = ""))
# write_csv(paired_gages_q99_1, paste(Cannabis_Eflows_path, "/PairedGages_Q99_pt1.csv", sep = ""))
# write_csv(paired_gages_deltah_1, paste(Cannabis_Eflows_path, "/PairedGages_DeltaH_pt1.csv", sep = ""))


