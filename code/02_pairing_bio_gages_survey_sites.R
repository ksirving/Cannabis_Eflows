## pairing gages in south fork eel and north coast

## packages
library(readr)
library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)
library(nhdplusTools)

# workflow
#1 - upload all bio sites, gages, and transect sites
#2 - combine layers in each category
#3 - match layers directly - bio-gage, bio-transect, and all 3
#4 - pair layers with criteria
# a - same HUC
# b - 3km downstream


# Upload shapefiles -------------------------------------------------------------------------

sf::sf_use_s2(FALSE) ## switch off spherical geom

## bio
bugs <- st_read("ignore/Web_GIS_Cannabis/bioassessment_sites/00_bug_sites_all.shp")
algae <- st_read("ignore/Web_GIS_Cannabis/bioassessment_sites/00_algae_sites_all.shp")
phab <- st_read("ignore/Web_GIS_Cannabis/bioassessment_sites/SMC_phab_north_coast.shp")


## gages
TU <- st_read("ignore/Web_GIS_Cannabis/TU_Gages_SWRCB.shp")
trin <- st_read("ignore/Web_GIS_Cannabis/Trinity sites_SWRCB.shp")
alart <- st_read("ignore/Web_GIS_Cannabis/Alart2Sites_SWRCB.shp")
cal <- st_read("ignore/Web_GIS_Cannabis/CSWRCB_StreamGages/CaliforniaStreamGages.shp")

TU
cal
GU <- st_read("ignore/Web_GIS_Cannabis/Gauges_updated_SWRCB.shp")
Otrin <- st_read("ignore/Web_GIS_Cannabis/Ongoing Trinity monitoring sites_SWRCB.shp")

som <- st_read("ignore/Web_GIS_Cannabis/SonomaOneRain2020_SWRCB.shp")
rb2 <- st_read("ignore/Web_GIS_Cannabis/Region 2 Stream Gauge Layer_2019_SWRCB.shp")

mas <- st_read("ignore/Web_GIS_Cannabis/All_Sites_Master_File_1_Exce1 Events_SWRCB.shp")
will <- st_read("ignore/Web_GIS_Cannabis/WIL_PAL_StageLoggers_SWRCB.shp")

## survey sites

sfe <- st_read("ignore/Web_GIS_Cannabis/North Coast/regional datasets/geodatabase/geo_high_resolution/SFE/SFE_sites_highresolution.shp")
nc <- st_read("ignore/Web_GIS_Cannabis/North Coast/regional datasets/geodatabase/geo_high_resolution/NC/NC_sites_highresolution.shp")
nc_fin <- st_read("ignore/Web_GIS_Cannabis/North Coast/regional datasets/geodatabase/geo_transect_survey/3_Coastal_regions/NC_sites_FINAL.shp")

## HUCS

load("input_data/huc12_sf.rda") ## h12

# Format data -------------------------------------------------------------
TU
## add file name as identifier
TU <- TU %>%
  mutate(Filename = "TU_Gages_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("T", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

trin <- trin %>%
  mutate(Filename = "Trinity sites_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("Tr", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

alart <- alart %>%
  mutate(Filename = "Alart2Sites_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("Al", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

GU <- GU %>%
  mutate(Filename = "Gauges_updated_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("G", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

Otrin <- Otrin %>%
  mutate(Filename = "Ongoing Trinity monitoring sites_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("Ot", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

som <- som %>%
  mutate(Filename = "SonomaOneRain2020_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("S", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

rb2 <- rb2 %>%
  mutate(Filename = "Region 2 Stream Gauge Layer_2019_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("R", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

mas <- mas %>%
  mutate(Filename = "All_Sites_Master_File_1_Exce1 Events_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("M", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

will <- will %>%
  mutate(Filename = "WIL_PAL_StageLoggers_SWRCB.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("W", ID)) %>%
  dplyr::select(Name, geometry, Filename, ID)

## combine
all_gages <- bind_rows(TU,trin,alart,GU,Otrin, som,rb2,mas,will)
head(all_gages)

all_gages <- all_gages %>%
  mutate(gagelat = unlist(map(geometry,2)),
         gagelong = unlist(map(geometry,1)))


## cal file is different - format
## different geometry - join separtely to bio etc
cal <- cal %>%
  mutate(Filename = "CaliforniaStreamGages.shp", ID = 1:length(geometry)) %>%
  mutate(ID = paste0("C", ID)) %>%
  select(sitename,geometry, Filename, comid_medr, ID) %>%
  rename(Name = sitename, COMID1 = comid_medr) %>%
  mutate(gagelat = unlist(map(geometry,2)),
         gagelong = unlist(map(geometry,1)))



## save as shape
write_rds(all_gages, "output_data/02_all_gages_combined.rds")

# Get comids --------------------------------------------------------------
# comids_all_ca <- st_read("/Users/katieirving/SCCWRP/Staff - Data/KatherineIrving/FromAnnie/NHD_Plus_CA/NHDPlus_V2_Flowline_CA.shp")

library(nhdplusTools)
#  
class(bugs)

# Create dataframe for looking up COMIDS (here use all stations)
bug_segs <- bugs %>%
  dplyr::select(masterid, longitude, latitude) %>%
  distinct(masterid, longitude, latitude) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=3310, remove=F)

# use nhdtools to get comids
bug_all_coms <- bug_segs %>%
  group_split(masterid) %>%
  set_names(., bug_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

# flatten into single dataframe instead of list
bug_segs_df <-bug_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

bugs <- full_join(bugs, bug_segs_df, by = "masterid")

# Create dataframe for looking up COMIDS (here use all stations)
algae_segs <- algae %>%
  dplyr::select(masterid, longitude, latitude) %>%
  distinct(masterid, longitude, latitude) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=3310, remove=F)

# use nhdtools to get comids
algae_all_coms <- algae_segs %>%
  group_split(masterid) %>%
  set_names(., algae_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

# flatten into single dataframe instead of list
algae_segs_df <-algae_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

algae <- full_join(algae, algae_segs_df, by =  "masterid")

## gages
st_crs(all_gages)

# Create dataframe for looking up COMIDS (here use all stations)
gage_segs <- all_gages %>%
  dplyr::select(ID, gagelong, gagelat) %>%
  distinct(ID, gagelong, gagelat) %>% as.data.frame() %>% 
  rename(latitude = gagelat, longitude = gagelong) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=3310, remove=F)

gage_segs

# use nhdtools to get comids
gage_all_coms <- gage_segs %>%
  group_split(ID) %>%
  set_names(., gage_segs$ID) %>%
  map(~discover_nhdplus_id(.x$geometry))

# flatten into single dataframe instead of list
gage_segs_df <-gage_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "ID")

all_gages <- full_join(all_gages, gage_segs_df, by =  "ID")

## cal gages
cal
# Create dataframe for looking up COMIDS (here use all stations)
cal_segs <- cal %>%
  dplyr::select(ID, gagelong, gagelat) %>%
  distinct(ID, gagelong, gagelat) %>% 
  rename(latitude = gagelat, longitude = gagelong) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=3310, remove=F)

# use nhdtools to get comids
cal_all_coms <- cal_segs %>%
  group_split(ID) %>%
  set_names(., cal_segs$ID) %>%
  map(~discover_nhdplus_id(.x$geometry))

# flatten into single dataframe instead of list
cal_segs_df <-cal_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "ID")

cal <- full_join(cal, cal_segs_df, by =  "ID")

## transect suveys
nc_fin
# Create dataframe for looking up COMIDS (here use all stations)
nc_segs <- nc_fin %>%
  dplyr::select(siteID, LONGITUDE, LATITUDE) %>%
  distinct(siteID, LONGITUDE, LATITUDE) %>% 
  rename(latitude = LATITUDE, longitude = LONGITUDE) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=3310, remove=F)

# use nhdtools to get comids
nc_all_coms <- nc_segs %>%
  group_split(siteID) %>%
  set_names(., nc_segs$siteID) %>%
  map(~discover_nhdplus_id(.x$geometry))

# flatten into single dataframe instead of list
nc_segs_df <-nc_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "siteID")

nc_fin <- full_join(nc_fin, nc_segs_df, by =  "siteID")

nc_fin

# Add HUC -----------------------------------------------------------------


## add HUC
head(h12)
# Add H12 to  Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using algae DISTINCT STATIONS
all_gages_h12 <- st_join(all_gages,left = TRUE, h12[c("HUC_12")])
all_gages_h12

## same with CAL gages

cal <- st_transform(cal, crs = 4326) ## change CRS
all_cal_gages_h12 <- st_join(cal,left = TRUE, h12[c("HUC_12")])
all_cal_gages_h12

# check projections are same
st_crs(all_gages) == st_crs(h12)
st_crs(bugs) == st_crs(h12)
st_crs(cal) == st_crs(h12)
st_crs(nc_fin) == st_crs(h12)

#### bio
all_bugs_h12
all_bugs_h12 <- st_join(bugs,left = TRUE, h12[c("HUC_12")])
all_algae_h12 <- st_join(algae,left = TRUE, h12[c("HUC_12")])

## phab - reduce to only sites

phab <- phab %>%
  select("masterid", "latitude",  "longitude", "county",  "geometry" ) %>%
  distinct()
  
all_phab_h12 <- st_join(phab,left = TRUE, h12[c("HUC_12")])
all_phab_h12

dim(all_bugs_h12) ## 520
dim(all_algae_h12) ## 257
dim(all_phab_h12) ## 185
dim(all_cal_gages_h12) ## 2611
dim(all_gages_h12) ## 567


# map sites in same HUC 12 -------------------------------------------------------

## first join by huc 12, then find directly overlapping sites
## gages and bio

head(bugs)

## make bio a df
all_bugs_h12_df <- as.data.frame(all_bugs_h12) %>% select(-geometry)
all_algae_h12_df <- as.data.frame(all_algae_h12) %>% select(-geometry)

## lat, long are bio sites
## gemoetry is gage site

# now join based on H12: what  stations share same H12 as gage?

########### bugs 
bugs_gages_h12 <- left_join(all_gages_h12, all_bugs_h12_df, by = "HUC_12") %>% 
  distinct() %>% filter(!is.na(masterid))

class(bugs_gages_h12_bug_cds)
head(bugs_gages_h12_bug_cds)
  
# number of unique?
length(unique(factor(bugs_gages_h12$HUC_12))) # h12=47
length(unique(bugs_gages_h12$masterid)) # bug sites = 158
length(unique(bugs_gages_h12$Name)) # gages = 149

## sf object with only coords for bugs - bug layer1
bugs_gages_h12_bug_cds <- bugs_gages_h12 %>%
  select(HUC_12:county) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

bugs_cal_gages_h12
bugs_cal_gages_h12 <- left_join(all_cal_gages_h12, all_bugs_h12_df, by = "HUC_12") %>% 
  distinct() %>%  filter(!is.na(masterid))

# number of unique?
length(unique(factor(bugs_cal_gages_h12$HUC_12))) # h12= 90
length(unique(bugs_cal_gages_h12$masterid)) # bug sites = 252
length(unique(bugs_cal_gages_h12$Name)) # gages = 157

## sf object with only coords for bugs - bug layer2
bugs_cal_gages_h12_bug_cds <- bugs_cal_gages_h12 %>%
  select(HUC_12:county) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

all_bugs_gages_huc <- bind_rows(bugs_gages_h12_bug_cds, bugs_cal_gages_h12_bug_cds)
all_bugs_gages_huc <- distinct(all_bugs_gages_huc)

####### Algae
algae_gages_h12 <- left_join(all_gages_h12, all_algae_h12_df, by = "HUC_12") %>% 
  distinct() %>% filter(!is.na(masterid))

# number of unique?
length(unique(factor(algae_gages_h12$HUC_12))) # h12=39
length(unique(algae_gages_h12$masterid)) # bug sites = 39
length(unique(algae_gages_h12$Name)) # gages = 110

## sf object with only coords for algae - algae layer1
algae_gages_h12_algae_cds <- algae_gages_h12 %>%
  select(HUC_12:county) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)


algae_cal_gages_h12 <- left_join(all_cal_gages_h12, all_algae_h12_df, by = "HUC_12") %>% 
  distinct() %>%  filter(!is.na(masterid))

# number of unique?
length(unique(factor(algae_cal_gages_h12$HUC_12))) # h12= 62
length(unique(algae_cal_gages_h12$masterid)) # bug sites = 114
length(unique(algae_cal_gages_h12$Name)) # gages = 108

## sf object with only coords for algae - algae layer1
algae_cal_gages_h12_algae_cds <- algae_cal_gages_h12 %>%
  select(HUC_12:county) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

all_algae_gages_h12 <- bind_rows(algae_cal_gages_h12_algae_cds, algae_gages_h12_algae_cds)
all_algae_gages_h12 <- distinct(all_algae_gages_h12)


## gages with Bio

Gages_sel_cal <- c(unique(algae_cal_gages_h12$Name), 
               unique(bugs_cal_gages_h12$Name))

Gages_sel_loc <- c(unique(algae_gages_h12$Name),
                   unique(bugs_gages_h12$Name))

length(Gages_sel_loc)

all_gages_h12

### filter gages to bio sites
all_gages_h12_sel <- all_gages_h12 %>%
  filter(Name %in% Gages_sel_loc) %>%
  as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("gagelong", "gagelat"), crs=4326, remove=F) %>%
  na.omit()

all_cal_gages_h12_sel <- all_cal_gages_h12 %>%
  filter(Name %in% Gages_sel_cal)

head(all_cal_gages_h12_sel)

write.csv(all_gages_h12_sel, "output_data/02_bio_gages_same_huc.csv")
write.csv(all_cal_gages_h12_sel, "output_data/02_bio_cal_gages_same_huc.csv")

### HUCs with bio

HUCs_sel <- c(unique(algae_cal_gages_h12$HUC_12),
              unique(algae_gages_h12$HUC_12),
              unique(bugs_cal_gages_h12$HUC_12),
              unique(bugs_gages_h12$HUC_12))

length(unique(HUCs_sel)) ## 228

h12 <- h12 %>%
  filter(HUC_12 %in% HUCs_sel)

st_write(h12, "output_data/02_h12_selected.shp",append=FALSE)

### high res sites
head(nc)

dim(nc)

nc_h12 <- st_join(nc,left = TRUE, h12[c("HUC_12")])
nc_h12

head(nc_fin)
dim(nc_fin)

nc_fin_h12 <- st_join(nc_fin,left = TRUE, h12[c("HUC_12")])
sum(is.na(nc_fin_h12$HUC_12)) ## 82 Nas

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

# this map of all sites in same HUC 12
m1 <- mapview(all_bugs_gages_huc, cex=6, col.regions="orange",
              layer.name="Bugs Stations") +
  mapview(all_cal_gages_h12_sel, col.regions="skyblue", cex=2, color="blue2",
          layer.name="USGS Gages") + 
  mapview(all_algae_gages_h12, col.regions="green", cex=6,
          layer.name="Algae Stations") +
  # mapview(algae_gages_h12_algae_cds, col.regions="green", cex=6,
  #         layer.name="Algae Stations Local") +
  # mapview(bugs_gages_h12_bug_cds, col.regions="orange", cex=6,
  #         layer.name="Bugs Stations Local") +
  mapview(h12, col.regions="dodgerblue", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="HUC12") +
mapview(all_gages_h12_sel, col.regions="red", cex=2, color="red",
        layer.name="Gages Local") +
  mapview(nc_fin_h12, col.regions="purple", cex=2, color="purple",
          layer.name="Transect Survey Sites") 


m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# Overlapping sites -------------------------------------------------------

## which transect sites over lap directly with bio

## join bugs and algae with transects

bugs_trans <- st_join(all_bugs_gages_huc, nc_fin_h12 )
algae_trans <- st_join(all_algae_gages_h12, nc_fin_h12 )
## nothing matches directly


# GT UPSTREAM FLOWLINES FROM TRANSECTS ----------------------------------------

## TRANSFORM TO UTM datum for flowlines
all_algae_gages_h12 <- st_transform(all_algae_gages_h12, crs=3310) # use CA Teale albs metric
all_bug_gages_h12 <- st_transform(all_bugs_gages_huc, crs=3310)
nc_fin_h12 <- st_transform(nc_fin_h12, crs=3310)

# use a list of comids to make a list to pass to the nhdplusTools function


summary(nc_fin_h12$COMID) # survey sites

# Use the surveycom_list
coms_list <- map(nc_fin_h12$COMID, ~list(featureSource = "COMID", featureID=.x))
coms_list
# check
coms_list[[200]] # should list feature source and featureID

library(beepr)
library(data.table)


# Get upstream mainstem streamlines (10 km limit) from gages
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x, 
                                             mode="UM", # upstream main 
                                             distance_km = 1))
?as.lts
?navigate_nldi

# transform the sf layer to match mainstems crs (4326)
nc_fin_h12 <- nc_fin_h12 %>% st_transform(4326)

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., nc_fin_h12$siteID) %>%
  map2(nc_fin_h12$siteID, ~mutate(.x, siteID =.y))

nc_fin_h12$siteID
mainstems_flat_us

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., nc_fin_h12$siteID) %>%
  map2(mainstemsUS, nc_fin_h12$siteID, ~mutate(.x, siteID =.y))

head(mainstems_flat_us)
length(nc_fin_h12$siteID)
length(mainstemsUS)

# bind together
mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "UM")

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

# preview
mapview(mainstems_us) + 
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# save as both for now
#save(mainstems_us, file = "data_output/03_selected_nhd_mainstems_gages_us.rda")

# GET DOWNSTREAM FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 20 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 20))
beep(2)

# check length (for NAs?)
mainstemsDS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_algae$site_id) %>%
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DM")

rm(mainstems_flat_ds, mainstemsDS)

mapview(mainstems_us, color="yellow") + mapview(mainstems_ds, color="blue") +
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")


# get diversions
mainstemsDD <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamDiversions",
                                             distance_km = 20))
beep(2)

# check length (for NAs?)
mainstemsDD %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_dd <- mainstemsDD %>%
  set_names(., sel_gages_algae$site_id) %>%
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_dd <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_dd, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_dd <- mainstems_dd %>% 
  mutate(from_gage = "DD")

rm(mainstems_flat_ds, mainstemsDS, mainstemsDD, mainstems_flat_dd)



