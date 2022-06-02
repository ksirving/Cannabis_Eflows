# Script to calculate functional flow metrics and deltah for PRMS model subbasins
# by Annie Holt, 6/1/2022

# Directions from Kris:
# Flow data:
#   C:\Users\KristineT.SCCWRP2K\SCCWRP\Cannabis E-Flows - General\Data\RawData\SF_Eel_data\South_Fork_Eel_PRMS\SFER_subbasins.sub_cfs.csv
# Spatial data pourpoint locations:
#   C:\Users\KristineT.SCCWRP2K\SCCWRP\Cannabis E-Flows - General\Data\RawData\SF_Eel_data\South_Fork_Eel_PRMS\SFER_GIS\modeled_pour_points.shp
#     Use this to find the COMIDs associated with each subbasin number
#     Make sure that COMIDs from NHD layer fall on correct line (visual inspection)
# Calculate FFMs and deltaH for all flow timeseries
# Include COMID, subbasin number in output dataframe


# have yet to do this part of the task: matching comids with paired dataset:

# Identify matching or about matching gages (02_selected_gages_may132022.shp):
#   matching COMIDs with task above, ultimate goal is to compare gage FFMs with PRMS model FFM



#### Libraries ####

library(tidyverse) #for data wranging
library(lubridate) #for easy data manipulation
library(ffcAPIClient) #calculators
library(sf) #for spatial data
library(nhdplusTools) #for retrieving comid

# library(devtools)
# #install {ffcAPIClient} package, only have to do this once
# devtools::install_github("ceff-tech/ffc_api_client/ffcAPIClient")

#### Data Locations ####

# update this path depending on user/file location
# I personally synced a sharepoint folder locally, which contains all .csv flow files in one folder
setwd("C:/Users/anneh/SCCWRP/Cannabis E-Flows - Data/RawData/SF_Eel_data/South_Fork_Eel_PRMS")

#### FFC TOKEN ####

# Annie's FFC toke, update depending on user
ffctoken <- ffcAPIClient::set_token("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBbm5lIiwibGFzdE5hbWUiOiJIb2x0IiwiZW1haWwiOiJhZ2hvbHRAdWNkYXZpcy5lZHUiLCJyb2xlIjoiVVNFUiIsImlhdCI6MTYwNTI5NTQzMn0.wzbBa5rLs6DAvEpUQVVLmQx23g2EBgEQhM2wcqyweUc")


#### DATA PREPPING ####

# generally, want to assign COMID to each pour point, which is required to run FFC

# importing point shapefile data for those pour pounts
# make WGS84 projection
# will use these locations to associate the subbassin number/pour point to COMIDS
pour_pt <- st_read("./SFER_GIS/modeled_pour_points.shp") %>% 
  st_transform(crs = 4326) %>% 
  arrange(SUBNUM) 

# extracting lat/longs from geometry 
pour_pt_coords <- st_coordinates(pour_pt) %>% 
  as.data.frame() %>% 
  select(X, Y) %>% 
  rename(latitude = Y, longitude = X)

# getting comids using input points, putting into dataframe with ID as SUBNUM
# this uses the 'discover_nhdplus_id' function from NHD tools package

pour_pt_comid <- pour_pt %>%
  group_split(SUBNUM) %>%
  set_names(., pour_pt$SUBNUM) %>%
  # iteratining funciton over datasets
  map(~discover_nhdplus_id(.x$geometry)) %>% 
  # flattening into dataframe
  flatten_dfc() %>% t() %>%
  as.data.frame() %>% 
  # final column prep
  rename("COMID"=V1) %>% rownames_to_column(var = "SUBNUM")

# manually replacing some comids, based on reviewing shapefiles (NHD flowlines + modeled subbasins + pour points) in ArcPro
# we want the stream reach/pour point location capturing the above flow in the subbasins 
# in the below cases, the pour points fell on a confluence, so matched with a different COMID than we want
pour_pt_comid_final <- pour_pt_comid %>% 
  mutate(COMID = case_when(SUBNUM == 20 ~ "8287556",
                           SUBNUM == 9 ~ "8285342",
                           SUBNUM == 3 ~ "8285008",
                           TRUE ~ as.character(COMID)))


# combining all datasets into one final ID dataset
pour_pt_id <- pour_pt_comid_final %>% 
  left_join(pour_pt %>% mutate(SUBNUM = as.character(SUBNUM)), by = "SUBNUM") %>% 
  bind_cols(pour_pt_coords) %>% 
  select(-geometry)



###########################################

#### FFC RUNS ####

# creating list of subbasins to interate through
subbasins_list <- 1:21

# importing flow timeseries data
timeseries_all <- read.csv("./SFER_subbasins.sub_cfs.csv")
# viewing the id dataset, which we will use for the COMID and add to the final FFM datasets
pour_pt_id


# Creating function for analysis/interation (function creates all requested datasets, one site/subbasin at a time)
# will import data, prep it, run through calculator, extract/assemble requested results

# want ffc_results for each site
# also want to retain id columns

ffc_fun <- function(id){
  
  # # for testing
  # id <- subbasins_list[[1]]
  
  # Subbasin number
  col_id <- paste("X", id, sep = "")
  
  # COMID
  id_df <- pour_pt_id %>% 
    filter(SUBNUM == id)
  
  COMID <- id_df$COMID
  
  # import timeseries, selecting the single subbasin ID at a time
  # prepping to create 'date' and 'flow' column for input to calculator
  timeseries <- timeseries_all %>% 
    select(Date, col_id) %>% 
    rename(flow = col_id, date = Date) %>% 
    # make sure date is in date formatted column 
    mutate(date = ymd(date)) %>% 
    # transform to MM/DD/YYYY
    mutate(date = format(date, "%m/%d/%Y")) %>%
    # make columns character format, though not sure if this is actually necessary
    mutate(date = as.character(date)) %>% 
    select(date, flow)
  
  
  ffc <- tryCatch(ffcAPIClient::evaluate_alteration(timeseries_df  = timeseries, 
                                                    token = get_token(), 
                                                    # plot_output_folder = "./ffc_outputs/", 
                                                    comid= COMID),
                  error = function(e){data.frame()})
  
  
  # also assign water year type to each COMID
  # based on average flow per water year
  wateryear_type <- timeseries %>% 
    mutate(date2 = mdy(date)) %>% 
    # assing water year
    mutate(year = case_when(month(date2) < 10 ~ year(date2),
                            month(date2) >= 10 ~ year(date2) + 1)) %>% 
    group_by(year) %>% 
    # average flow for each wateryear
    summarize(avg_flow = mean(flow)) %>% 
    # 2022 isn't full water year so remove
    filter(year < 2022) %>%
    # arrange(avg_flow) %>% 
    ungroup() %>% 
    # calcualte top/middle/bottom third of average flow, assign to water year type categories
    mutate(q_1_3 = quantile(avg_flow, 1/3), q_2_3 = quantile(avg_flow, 2/3)) %>% 
    mutate(wyt = case_when(avg_flow < q_1_3 ~ "dry", 
                           avg_flow >= q_1_3 & avg_flow < q_2_3 ~ "moderate",
                           avg_flow >= q_2_3 ~ "wet")) %>% 
    select(year, wyt) %>% 
    mutate(year = as.character(year))
  
  
  
  ffm_results <- ffc$ffc_results %>% 
    mutate(SUBNUM = as.character(id), COMID = COMID) %>% 
    # adding extra id info
    left_join(id_df, by = c("SUBNUM", "COMID")) %>% 
    # adding water year type
    left_join(wateryear_type, by = c("Year" = "year")) %>% 
    # moving to front of column order just for viewing
    relocate(SUBNUM, COMID, TYPE, latitude, longitude, Year, wyt)
  
  # making everything lowercase
  colnames(ffm_results) <- tolower(colnames(ffm_results))  
  
  return(ffm_results)
  
}

# iterate function over the different flow data/COMIDs, returning dataframes
# used map_dfc to both iterate and row bind results into one dataframe
# this takes a few minutes to run on the 20 or so subbasins 
ffc_fun_run <- subbasins_list %>%  purrr::map_dfr(~ffc_fun(.x))



#### DELTA H ####

# directory for other datasets
# also update depending on user
setwd("C:/Users/anneh/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/RawData/Data_for_SDSU/")

# # Import wyt classification done by Grantham et al. (2021). This takes a while.
# # looks like this only goes  up to 2014?? decided to calculate water year based on the flow timeseries instead
# wyt_df <- read.csv("./reference_database/ref_percentiles_codefornature/WYT_ALL_COMIDS.csv") %>% 
#   select(COMID, WYT, year) %>% 
#   rename(comid = COMID, wyt = WYT)

# importing reference dataset
ref_FFM <- read.csv(file = "./reference_database/ref_percentiles_codefornature/Functional_Flows_Data.csv")
# selecting only columns of interest
ref_FFM <- ref_FFM %>% select(comid, ffm, wyt, p50) 


# adding reference and ffc results together to calculate deltah (by comid and water year type)
ffm_deltah <- ffc_fun_run %>% 
  pivot_longer(-c(subnum, comid, type, latitude, longitude, year, wyt), names_to = "ffm", values_to = "result") %>% 
  # # adding water year type, by comid
  # left_join(wyt_df %>% select(comid, wyt, year) %>% mutate(year = as.character(year)) %>% mutate(comid = as.character(comid)), by = c("year", "comid"))
  
  # joining reference values
  left_join(ref_FFM %>% mutate(comid = as.character(comid)) %>% rename(reference_result = p50), by = c("comid", "wyt", "ffm")) %>% 
  
  # calculating delta
  mutate(delta = result - reference_result) %>% 
  # creating new metric names matching Donny's version
  mutate(d_ffm = paste("d_", ffm, sep = "")) %>% 
  # dropping other columns before pivoting wide again
  select(-ffm, -reference_result, -result) %>% 
  pivot_wider(names_from = "d_ffm", values_from = "delta")


#### Q99 ####

# function to calculate q99 for each subbasin/flow timeseries per year

q99_fun <- function(id){
  
  # # # for testing
  # id <- subbasins_list[[1]]
  
  # Subbasin number
  col_id <- paste("X", id, sep = "")
  
  # COMID
  id_df <- pour_pt_id %>% 
    filter(SUBNUM == id)
  
  COMID <- id_df$COMID
  
  # import timeseries, selecting the single subbasin ID at a time
  # prepping to create 'date' and 'flow' column for input to calculator
  timeseries <- timeseries_all %>% 
    select(Date, col_id) %>% 
    rename(flow = col_id, date = Date) %>% 
    # make sure date is in date formatted column 
    mutate(date = ymd(date)) %>% 
    # transform to MM/DD/YYYY
    mutate(date = format(date, "%m/%d/%Y")) %>%
    # make columns character format, though not sure if this is actually necessary
    mutate(date = as.character(date)) %>% 
    select(date, flow)
  
  q99 <- timeseries %>% 
    mutate(date2 = mdy(date)) %>% 
    mutate(year = case_when(month(date2) < 10 ~ year(date2),
                            month(date2) >= 10 ~ year(date2) + 1)) %>%  
    group_by(year) %>% 
    summarize(q99 = quantile(flow, 0.99)) %>% 
    # removing 2022 because we don't have a full water year of flow data
    filter(year < 2022) %>% 
    ungroup() %>% 
    # adding id information
    mutate(subnum = id_df$SUBNUM, comid = id_df$COMID, type = id_df$TYPE, longitude = id_df$longitude, latitude = id_df$latitude) %>% 
    select(subnum, comid, type, longitude, latitude, year, q99)
  
  return(q99)
  
}


# iterate function over the different flow data/COMIDs, returning dataframes
# used map_dfc to both iterate and row bind results into one dataframe
q99_fun_run <- subbasins_list %>%  purrr::map_dfr(~q99_fun(.x))


#### EXPORT ####

# location from Kris: PRMS FFMS here: C:\Users\KristineT.SCCWRP2K\SCCWRP\Cannabis E-Flows - General\Data\RawData\SF_Eel_data\South_Fork_Eel_PRMS\SFER_PRMS_FFMs

# updating directory again
# update depending on user
setwd("C:/Users/anneh/SCCWRP/Cannabis E-Flows - Data/RawData/SF_Eel_data/South_Fork_Eel_PRMS")

# write_csv(ffc_fun_run, "./SFER_PRMS_FFMs/SFER_PRMS_FFMs.csv")
# write_csv(q99_fun_run, "./SFER_PRMS_FFMs/SFER_PRMS_Q99.csv")
# write_csv(ffm_deltah, "./SFER_PRMS_FFMs/SFER_PRMS_DeltaH.csv")
