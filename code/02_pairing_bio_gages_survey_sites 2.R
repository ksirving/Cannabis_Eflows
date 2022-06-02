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
names(cal)
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

cal

## save as shape
write_rds(all_gages, "output_data/02_all_gages_combined.rds")

# Get comids --------------------------------------------------------------
# comids_all_ca <- st_read("/Users/katieirving/SCCWRP/Staff - Data/KatherineIrving/FromAnnie/NHD_Plus_CA/NHDPlus_V2_Flowline_CA.shp")
#  
class(bugs)
st_crs(bugs)

# Create dataframe for looking up COMIDS (here use all stations)
bug_segs <- bugs %>%
  dplyr::select(masterid, longitude, latitude) %>%
  distinct(masterid, longitude, latitude) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  arrange(masterid)

# use nhdtools to get comids
bug_all_coms <- bug_segs %>%
  group_split(masterid) %>%
  set_names(., bug_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

bug_all_coms

# flatten into single dataframe instead of list
bug_segs_df <-bug_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

bugs <- full_join(bugs, bug_segs_df, by = "masterid")

# Create dataframe for looking up COMIDS (here use all stations)
algae_segs <- algae %>%
  dplyr::select(masterid, longitude, latitude) %>%
  distinct(masterid, longitude, latitude) %>% 
  st_as_sf(coords=c("longitude", "latitude"),  crs=4326, remove=F) %>%
  arrange(masterid)

str(algae_segs)
# use nhdtools to get comids
algae_all_coms <- algae_segs %>%
  group_split(masterid) %>%
  set_names(., algae_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

algae_all_coms

# ?discover_nhdplus_id

# flatten into single dataframe instead of list
algae_segs_df <-algae_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

algae <- full_join(algae, algae_segs_df, by =  "masterid")

## gages
st_crs(all_gages)
head(all_gages)

# Create dataframe for looking up COMIDS (here use all stations)
gage_segs <- all_gages %>%
  dplyr::select(ID, gagelong, gagelat) %>%
  distinct(ID, gagelong, gagelat) %>% as.data.frame() %>% 
  rename(latitude = gagelat, longitude = gagelong) %>%
  st_as_sf(coords=c("longitude", "latitude"),  crs=4326, remove=F) %>%
  arrange(ID)

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

gage_segs_df

## join with coordinates
all_gages2 <- full_join(all_gages, gage_segs_df, by =  "ID")
## join with gage segs??
head(all_gages2)

## cal gages - doesn't work for all - but has COMIDs 

# # Create dataframe for looking up COMIDS (here use all stations)
cal_segs <- cal %>%
  dplyr::select(ID, gagelong, gagelat) %>%
  distinct(ID, gagelong, gagelat) %>%
  rename(latitude = gagelat, longitude = gagelong) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=3310, remove=F) %>%
  arrange(ID)

# use nhdtools to get comids - doesn't work here!!!!
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
  st_as_sf(coords=c("longitude", "latitude"),  crs=4326, remove=F) %>%
  arrange(siteID)

# use nhdtools to get comids
nc_all_coms <- nc_segs %>%
  group_split(siteID) %>%
  set_names(., nc_segs$siteID) %>%
  map(~discover_nhdplus_id(.x$geometry))

nc_all_coms

# flatten into single dataframe instead of list
nc_segs_df <-nc_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "siteID")

nc_fin <- full_join(nc_fin, nc_segs_df, by =  "siteID")

nc_fin

# Add HUC -----------------------------------------------------------------

sf::sf_use_s2(FALSE) ## switch off spherical geom

## add HUC
head(h12)
# Add H12 to  Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using algae DISTINCT STATIONS
all_gages_h12 <- st_join(all_gages2,left = TRUE, h12[c("HUC_12")])
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

all_bugs_h12 <- st_join(bugs,left = TRUE, h12[c("HUC_12")])
all_algae_h12 <- st_join(algae,left = TRUE, h12[c("HUC_12")])

## phab - reduce to only sites

phab <- phab %>%
  select("masterid", "latitude",  "longitude", "county",  "geometry" ) %>%
  distinct()

all_phab_h12 <- st_join(phab,left = TRUE, h12[c("HUC_12")])


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

########### bugs and local gages with same H12 - both coords for bugs and gages included
bugs_gages_h12 <- left_join(all_gages_h12, all_bugs_h12_df, by = "HUC_12") %>% 
  distinct() %>% filter(!is.na(masterid))
head(bugs_gages_h12)

# number of unique?
length(unique(factor(bugs_gages_h12$HUC_12))) # h12=47
length(unique(bugs_gages_h12$masterid)) # bug sites = 158
length(unique(bugs_gages_h12$ID)) # gages = 199

## sf object with only coords for bugs - bug layer1 (i.e. change geometry to bug site)
bugs_gages_h12_bug_cds <- bugs_gages_h12 %>%
  select(ID,HUC_12:COMID.y) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID = COMID.y)

class(bugs_gages_h12_bug_cds)
head(bugs_gages_h12_bug_cds)

## same for california gages
bugs_cal_gages_h12 <- left_join(all_cal_gages_h12, all_bugs_h12_df, by = "HUC_12") %>% 
  distinct() %>%  filter(!is.na(masterid))
bugs_cal_gages_h12

# number of unique?
length(unique(factor(bugs_cal_gages_h12$HUC_12))) # h12= 90
length(unique(bugs_cal_gages_h12$masterid)) # bug sites = 252
length(unique(bugs_cal_gages_h12$ID)) # gages = 157

## sf object with only coords for bugs - bug layer2 (i.e. change geometry to bug site)
bugs_cal_gages_h12_bug_cds <- bugs_cal_gages_h12 %>%
  select(ID,HUC_12:COMID.y) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID = COMID.y)

head(bugs_cal_gages_h12_bug_cds)
## join both bug layers together for all bug sites in same HUC as one gage
all_bugs_gages_huc <- bind_rows(bugs_gages_h12_bug_cds, bugs_cal_gages_h12_bug_cds)
all_bugs_gages_huc <- distinct(all_bugs_gages_huc)

head(all_bugs_gages_huc)
# number of unique?
length(unique(factor(all_bugs_gages_huc$HUC_12))) # h12=108
length(unique(all_bugs_gages_huc$masterid)) # bug sites = 306
length(unique(all_bugs_gages_huc$ID)) # gages = 356

####### Algae
algae_gages_h12 <- left_join(all_gages_h12, all_algae_h12_df, by = "HUC_12") %>% 
  distinct() %>% filter(!is.na(masterid))

# number of unique?
length(unique(factor(algae_gages_h12$HUC_12))) # h12=29
length(unique(algae_gages_h12$masterid)) # bug sites = 39
length(unique(algae_gages_h12$ID)) # gages = 145

## sf object with only coords for algae - algae layer1
algae_gages_h12_algae_cds <- algae_gages_h12 %>%
  select(ID, HUC_12:COMID.y) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID = COMID.y)


algae_cal_gages_h12 <- left_join(all_cal_gages_h12, all_algae_h12_df, by = "HUC_12") %>% 
  distinct() %>%  filter(!is.na(masterid))

# number of unique?
length(unique(factor(algae_cal_gages_h12$HUC_12))) # h12= 62
length(unique(algae_cal_gages_h12$masterid)) # bug sites = 114
length(unique(algae_cal_gages_h12$ID)) # gages = 108

## sf object with only coords for algae - algae layer1
algae_cal_gages_h12_algae_cds <- algae_cal_gages_h12 %>%
  select(ID, HUC_12:COMID.y) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID = COMID.y)

all_algae_gages_h12 <- bind_rows(algae_cal_gages_h12_algae_cds, algae_gages_h12_algae_cds)
all_algae_gages_h12 <- distinct(all_algae_gages_h12)

# number of unique?
length(unique(factor(all_algae_gages_h12$HUC_12))) # h12=74
length(unique(all_algae_gages_h12$masterid)) # bug sites = 129
length(unique(all_algae_gages_h12$ID)) # gages = 253


## combine all bio sites 
algae_sites_paired <- all_algae_gages_h12 %>%
  mutate(Bio ="Algae")

bugs_sites_paired <- all_bugs_gages_huc %>%
  mutate(Bio ="Bugs")

all_bio_paird <- bind_rows(algae_sites_paired, bugs_sites_paired)

# all_bio_paird %>% 
#   group_by(Bio, HUC_12) 


## gages with Bio - identifier is ID
head(all_algae_gages_h12)

Gages_sel_cal <- c(unique(algae_cal_gages_h12$ID), 
                   unique(bugs_cal_gages_h12$ID))

Gages_sel_loc <- c(unique(algae_gages_h12$ID),
                   unique(bugs_gages_h12$ID))

length(Gages_sel_loc) ## 344

all_gages_h12

### filter gages to bio sites
all_gages_h12_sel <- all_gages_h12 %>%
  filter(ID %in% Gages_sel_loc) %>%
  as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("gagelong", "gagelat"), crs=4326, remove=F) %>%
  na.omit()

all_cal_gages_h12_sel <- all_cal_gages_h12 %>%
  filter(ID %in% Gages_sel_cal)

head(all_cal_gages_h12_sel)
dim(all_cal_gages_h12_sel) ## 157
dim(all_gages_h12_sel) ## 180

class(all_cal_gages_h12_sel)

write.csv(all_gages_h12_sel, "output_data/02_bio_gages_same_huc.csv")
write.csv(all_cal_gages_h12_sel, "output_data/02_bio_cal_gages_same_huc.csv")

### HUCs with bio

HUCs_sel <- c(unique(algae_cal_gages_h12$HUC_12),
              unique(algae_gages_h12$HUC_12),
              unique(bugs_cal_gages_h12$HUC_12),
              unique(bugs_gages_h12$HUC_12))

length(unique(HUCs_sel)) ## 108

## filter HUCs with bio
h12 <- h12 %>%
  filter(HUC_12 %in% HUCs_sel)

### high res sites

nc_h12 <- st_join(nc,left = TRUE, h12[c("HUC_12")])

nc_h12

head(nc_fin)
dim(nc_fin)

nc_fin_h12 <- st_join(nc_fin, left = TRUE, h12[c("HUC_12")])
sum(is.na(nc_fin_h12$HUC_12)) ## 82 Nas

nc_fin_h12 <- na.omit(nc_fin_h12)

length(unique(nc_fin_h12$siteID))

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

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


m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

## save all layers

st_write(h12, "output_data/02_h12_selected.shp",append=FALSE) ## h12s with gages and bio
st_write(all_gages_h12_sel, "output_data/02_local_gages_sites_same_huc_as_bio.shp",append=FALSE) ## local gages 
st_write(all_cal_gages_h12_sel, "output_data/02_cali_gages_sites_same_huc_as_bio.shp",append=FALSE) ## cali gages
st_write(all_algae_gages_h12,  "output_data/02_algae_sites_same_huc_as_gages.shp",append=FALSE) ## algae sites
st_write(all_bugs_gages_huc,  "output_data/02_bug_sites_same_huc_as_gages.shp",append=FALSE) ## bug sites
st_write(nc_fin_h12,  "output_data/02_transect_surveys_same_huc_as_gages_and_bio.shp",append=FALSE) ## bug sites
# class(all_algae_gages_h12)



# Overlapping sites -------------------------------------------------------

## which transect sites over lap directly with bio

## join bugs and algae with transects

bugs_trans <- st_join(all_bugs_gages_huc, nc_fin_h12 )
algae_trans <- st_join(all_algae_gages_h12, nc_fin_h12 )
## nothing matches directly


# GT UPSTREAM FLOWLINES FROM TRANSECTS ----------------------------------------

h12 <- st_read( "output_data/02_h12_selected.shp") ## h12s with gages and bio
all_gages_h12_sel <- st_read("output_data/02_local_gages_sites_same_huc_as_bio.shp") ## local gages 
all_cal_gages_h12_sel <- st_read("output_data/02_cali_gages_sites_same_huc_as_bio.shp") ## cali gages
all_algae_gages_h12 <- st_read("output_data/02_algae_sites_same_huc_as_gages.shp") ## algae sites
all_bugs_gages_huc <- st_read("output_data/02_bug_sites_same_huc_as_gages.shp") ## bug sites
nc_fin_h12 <- st_read("output_data/02_transect_surveys_same_huc_as_gages_and_bio.shp") ## bug sites
#
h12

## TRANSFORM TO UTM datum for flowlines
all_algae_gages_h12 <- st_transform(all_algae_gages_h12, crs=3310) # use CA Teale albs metric
all_bug_gages_h12 <- st_transform(all_bugs_gages_huc, crs=3310)
nc_fin_h12 <- st_transform(nc_fin_h12, crs=3310)
all_gages_h12_sel <- st_transform(all_gages_h12_sel, crs=3310)
all_cal_gages_h12_sel <- st_transform(all_cal_gages_h12_sel, crs=3310)
h12 <-  st_transform(h12, crs=3310)
all_cal_gages_h12_sel
# use a list of comids to make a list to pass to the nhdplusTools function
# first do gages, then see how many bio sites are on lines, then same with transects 
names(all_gages_h12_sel)
names(all_cal_gages_h12_sel)

all_cal_gages_h12_sel <- all_cal_gages_h12_sel %>%
  select(-COMID, Name:Filename, ID, COMID, gagelat, gagelong, HUC_12)   %>%
  # rename(COMID = COMID1) %>% ## remove retrieved comid, stick with original for now
  mutate(COMID = as.integer(COMID))

all_gages_h12_sel2 <- bind_rows(all_gages_h12_sel, all_cal_gages_h12_sel)
head(all_gages_h12_sel2)

st_write(all_gages_h12_sel2, "output_data/02_selected_gages_may132022.shp")
# Use the gage com_list
coms_list <- map(all_gages_h12_sel2$COMID, ~list(featureSource = "COMID", featureID=.x))
coms_list
# check
coms_list[1] # should list feature source and featureID




# Get upstream mainstem streamlines (10 km limit) from gages
# coms_list can be a dataframe (then may need to change `coms_list` to `coms_list$comid` or just a list of comids. 
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="UM", # upstream main
                                             distance_km = 10))

# transform the sf layer to match mainstems crs (4326)
all_gages_h12_sel <- all_gages_h12_sel %>% st_transform(4326)

# check length (for NAs?)
mainstemsUS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# drop NA/empty elements
mainstemsUS_c <- mainstemsUS %>% purrr::compact()
# mainstemsUS_c 
# testList <- mainstemsUS_c[172]

# testList
class(mainstemsUS_c)
# make a single flat layer
# this accesses each item in our list of items...nhdplus returns a list of 2 dataframes that include $UM_flowlines, $origin. 
# the name UM_flowlines can change depending on the "mode" above (may be DS or DD_flowlines).

mainstems_flat_us <- map_df(mainstemsUS_c, ~mutate(.x$UM_flowlines, comid_origin=.x$origin$comid, .after=nhdplus_comid))

head(mainstems_flat_us)

length(unique(mainstems_flat_us$nhdplus_comid)) ## 974
length(unique(mainstems_flat_us$comid_origin)) ## 215

# bind together
mainstems_us <- sf::st_as_sf(mainstems_flat_us, use.names = TRUE, fill = TRUE)

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "UM")

length(unique(mainstems_us$nhdplus_comid)) ## 974
length(unique(mainstems_us$comid_origin)) ## 215

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

st_crs(mainstems_us)

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

# this map of all sites in same HUC 12
m1 <- mapview(mainstems_us, color = "navyblue") + mapview(all_bugs_gages_huc, cex=6, col.regions="orange",
                                                          layer.name="Bugs Stations") +
  # mapview(all_cal_gages_h12_sel, col.regions="skyblue", cex=2, color="blue2",
  #         layer.name="USGS Gages") +
  mapview(all_algae_gages_h12, col.regions="green", cex=6,
          layer.name="Algae Stations") +
  # mapview(algae_gages_h12_algae_cds, col.regions="green", cex=6,
  #         layer.name="Algae Stations Local") +
  # mapview(bugs_gages_h12_bug_cds, col.regions="orange", cex=6,
  #         layer.name="Bugs Stations Local") +
  mapview(h12, col.regions="dodgerblue", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="HUC12") +
  mapview(all_gages_h12_sel, col.regions="red", cex=2, color="red",
          layer.name="Gages (All)") +
  mapview(nc_fin_h12, col.regions="purple", cex=2, color="purple",
          layer.name="Transect Survey Sites")


m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# GET DOWNSTREAM FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 20 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 10))
# beep(2)

# check length (for NAs?)
mainstemsDS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# drop NA/empty elements
mainstemsDS_c <- mainstemsDS %>% purrr::compact()
# mainstemsDS_c

# make a single flat layer
# this accesses each item in our list of items...nhdplus returns a list of 2 dataframes that include $UM_flowlines, $origin. 
# the name UM_flowlines can change depending on the "mode" above (may be DS or DD_flowlines).

mainstems_flat_ds <- map_df(mainstemsDS_c, ~mutate(.x$DM_flowlines, comid_origin=.x$origin$comid, .after=nhdplus_comid))

# bind together
mainstems_ds <- sf::st_as_sf(mainstems_flat_ds, use.names = TRUE, fill = TRUE)

head(mainstems_flat_us)

length(unique(mainstems_flat_ds$nhdplus_comid)) ## 968
length(unique(mainstems_flat_ds$comid_origin)) ## 215

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DM")

rm(mainstems_flat_ds, mainstemsDS)

# get diversions
mainstemsDD <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamDiversions",
                                             distance_km = 20))
beep(2)

# check length (for NAs?)
mainstemsDD %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# drop NA/empty elements
mainstemsDD_c <- mainstemsDD %>% purrr::compact()

# make a single flat layer
# this accesses each item in our list of items...nhdplus returns a list of 2 dataframes that include $UM_flowlines, $origin. 
# the name UM_flowlines can change depending on the "mode" above (may be DS or DD_flowlines).

mainstems_flat_dd <- map_df(mainstemsDD_c, ~mutate(.x$DD_flowlines, comid_origin=.x$origin$comid, .after=nhdplus_comid))

# bind together
mainstems_dd <- sf::st_as_sf(mainstems_flat_dd, use.names = TRUE, fill = TRUE)

head(mainstems_flat_dd)

length(unique(mainstems_flat_dd$nhdplus_comid)) ## 1511
length(unique(mainstems_flat_dd$comid_origin)) ## 215

# add direction to gage col
mainstems_dd <- mainstems_dd %>% 
  mutate(from_gage = "DD")

rm(mainstems_flat_ds, mainstemsDS, mainstemsDD, mainstems_flat_dd)

## add south fork eel

## south fork eel polygon
sfer <- st_read("ignore/Web_GIS_Cannabis/Belize_Morphology/SouthForkEel.shp")

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

# this map of all sites in same HUC 12
m1 <- mapview(mainstems_us, color = "navyblue") +
  mapview(mainstems_ds, color = "pink") +
  # mapview(mainstems_dd, color = "black") +
  mapview(all_bugs_gages_huc, cex=6, col.regions="orange",layer.name="Bugs Stations") +
  mapview(all_algae_gages_h12, col.regions="green", cex=6,
          layer.name="Algae Stations") +
  mapview(h12, col.regions="dodgerblue", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="HUC12") +
  mapview(sfer, col.regions="red", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="South Fork Eel") +
  mapview(all_gages_h12_sel, col.regions="red", cex=4, color="red",
          layer.name="Gages (All)") +
  mapview(nc_fin_h12, col.regions="purple", cex=2, color="purple",
          layer.name="Transect Survey Sites")


m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

### save out
st_write(mainstems_us, "output_data/02_upstream_flowlines_from_gage.shp", append=FALSE)
st_write(mainstems_ds, "output_data/02_downstream_flowlines_from_gage.shp", append=FALSE)
st_write(mainstems_dd, "output_data/02_diversions_flowlines_from_gage.shp", append=FALSE)


# Subset to SFER ----------------------------------------------------------

dim(h12)
## join all dfs and subset

all_bio_paird ## bugs and algae

length(unique(na.omit(all_bio_paird$HUC_12))) ## 108

al <- all_bio_paird %>% filter(Bio == "Algae") #%>% 

length(unique(al$masterid)) ## 129
length(unique(al$HUC_12)) ## 129

bg <- all_bio_paird %>% filter(Bio == "Bugs") #%>% 

length(unique(bg$masterid)) ## 306
length(unique(bg$HUC_12)) ## 306


nc_fin_h12 ## tansect surveys

length(unique(na.omit(nc_fin_h12$HUC_12))) ## 19
length(unique(na.omit(nc_fin_h12$siteID))) ## 119

nc_h12 ## high res

length(unique(na.omit(nc_h12$HUC_12))) ## 3
length(unique(na.omit(nc_h12$siteID))) ## 7

all_gages_h12_sel ## small gages

length(unique(na.omit(all_gages_h12_sel$HUC_12))) ## 47
length(unique(na.omit(all_gages_h12_sel$ID))) ## 180

all_cal_gages_h12_sel ## Cali gages

length(unique(na.omit(all_cal_gages_h12_sel$HUC_12))) ## 90
length(unique(na.omit(all_cal_gages_h12_sel$ID))) ## 157

## all bio HUC have a gage

length(unique(HUCs_sel)) ## 108 all gages

## south fork eel polygon
sfer <- st_read("ignore/Web_GIS_Cannabis/Belize_Morphology/SouthForkEel.shp")

## change crs to match 
all_bio_paird <- st_transform(all_bio_paird, crs = 3310)

## join with Bio and count HUCs
bio_sub <- st_join(sfer, left = TRUE, all_bio_paird)
bio_sub

length(unique(na.omit(bio_sub$HUC_12))) ## 9

al <- bio_sub %>% filter(Bio == "Algae") #%>% 

length(unique(al$masterid)) ## 11
length(unique(al$HUC_12)) ## 7

bg <- bio_sub %>% filter(Bio == "Bugs") #%>% 

length(unique(bg$masterid)) ## 20
length(unique(bg$HUC_12)) ## 9

## join to ggaes

## change crs to match 
all_gages_h12_sel <- st_transform(all_gages_h12_sel, crs = 3310)

gage_sub <- st_join(sfer, left = TRUE, all_gages_h12_sel)
gage_sub

## small gages

length(unique(na.omit(gage_sub$HUC_12))) ## 8
length(unique(na.omit(gage_sub$ID))) ## 8

all_cal_gages_h12_sel ## Cali gages

## change crs to match 
all_cal_gages_h12_sel <- st_transform(all_cal_gages_h12_sel, crs = 3310)

gage_sub2 <- st_join(sfer, left = TRUE, all_cal_gages_h12_sel)
gage_sub2

length(unique(na.omit(gage_sub2$HUC_12))) ## 3
length(unique(na.omit(gage_sub2$ID))) ## 5

## change crs to match 
nc_fin_h12 <- st_transform(nc_fin_h12, crs = 3310)

serv_sub <- st_join(sfer, left = TRUE, nc_fin_h12)
serv_sub

## small gages

length(unique(na.omit(serv_sub$HUC_12))) ## 8
length(unique(na.omit(serv_sub$siteID))) ## 8

#### same with north coast

norCoa <- st_read("ignore/Web_GIS_Cannabis/NorthCoast_Border.shp")

## change crs to match 
all_bio_paird <- st_transform(all_bio_paird, crs = 3310)

## join with Bio and count HUCs
bio_sub <- st_join(norCoa, left = TRUE, all_bio_paird)
bio_sub

length(unique(na.omit(bio_sub$HUC_12))) ## 30

al <- bio_sub %>% filter(Bio == "Algae") #%>% 

length(unique(al$masterid)) ## 50
length(unique(al$HUC_12)) ## 20

bg <- bio_sub %>% filter(Bio == "Bugs") #%>% 

length(unique(bg$masterid)) ## 72
length(unique(bg$HUC_12)) ## 30

## join to ggaes

## change crs to match 
all_gages_h12_sel <- st_transform(all_gages_h12_sel, crs = 3310)

gage_sub <- st_join(norCoa, left = TRUE, all_gages_h12_sel)
gage_sub

## small gages

length(unique(na.omit(gage_sub$HUC_12))) ## 11
length(unique(na.omit(gage_sub$ID))) ## 11

all_cal_gages_h12_sel ## Cali gages

## change crs to match 
all_cal_gages_h12_sel <- st_transform(all_cal_gages_h12_sel, crs = 3310)

gage_sub2 <- st_join(norCoa, left = TRUE, all_cal_gages_h12_sel)
gage_sub2

length(unique(na.omit(gage_sub2$HUC_12))) ## 23
length(unique(na.omit(gage_sub2$ID))) ## 47

## all gages HUC 

length(c(unique(na.omit(gage_sub$HUC_12)), unique(na.omit(gage_sub2$HUC_12))))

## change crs to match 
nc_fin_h12 <- st_transform(nc_fin_h12, crs = 3310)

serv_sub <- st_join(norCoa, left = TRUE, nc_fin_h12)
serv_sub

## small gages

length(unique(na.omit(serv_sub$HUC_12))) ## 8
length(unique(na.omit(serv_sub$siteID))) ## 8



