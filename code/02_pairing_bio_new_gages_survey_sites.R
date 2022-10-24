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
getwd()

# Upload shapefiles -------------------------------------------------------------------------

## bio
bugs <- st_read("ignore/Web_GIS_Cannabis/bioassessment_sites/00_bug_sites_all.shp")
algae <- st_read("ignore/Web_GIS_Cannabis/bioassessment_sites/00_algae_sites_all.shp")
phab <- st_read("ignore/Web_GIS_Cannabis/bioassessment_sites/SMC_phab_north_coast.shp")
# crs(bugs)

## gages from Kris
g1 <- read.csv("input_data/actual-flows-training-sites-in-ca.csv")
g2 <- read.csv("input_data/ca-actual-flows-eval-100.csv")
g3 <- read.csv("input_data/ca-unimpaired-eval-55.csv")
g4 <- read.csv("input_data/unimpaired-training-sites-in-ca.csv")
head(g1)
## make spatial

g1 <- g1 %>% dplyr:: select(-X) %>% st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)
g2 <- g2 %>% dplyr:: select(-X) %>% st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)
g3 <- g3 %>% dplyr:: select(-X) %>% st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)
g4 <- g4 %>% dplyr:: select(-X) %>% st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

## join together

gAll <- bind_rows(g1,g2,g3,g4) %>% distinct()

dim(gAll)
length(unique(gAll$remote_id)) ## 339
length(unique(gAll$COMID)) ## 338

class(gAll)
head(gAll)
## save as shape
st_write(gAll, "output_data/02_all_new_gages_combined.shp")

## HUCS

load("input_data/huc12_sf.rda") ## h12

# Get comids --------------------------------------------------------------

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
head(bugs)

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

head(algae)
class(algae)

# algae$masterid %in% bugs$masterid

bioData <- bind_rows(bugs, algae)
# head(bioData)
# class(bioData)
bioData <- bioData %>% select(-Field1)

st_write(bioData, "output_data/02_bio_sites_ids_comids_v2.shp", append = F)
# write.csv(bioData, "02_bio_sites_ids_comids_v2.csv")

bioData <- st_read( "output_data/02_bio_sites_ids_comids_v2.shp")
head(bioData)


# Add HUC -----------------------------------------------------------------

sf::sf_use_s2(FALSE) ## switch off spherical geom

## add HUC
head(h12)
# Add H12 to  Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using algae DISTINCT STATIONS
all_gages_h12 <- st_join(gAll,left = TRUE, h12[c("HUC_12")])
all_gages_h12

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

# this map of all sites in same HUC 12
m1 <- mapview(gAll, cex=6, col.regions="purple",
              layer.name="Gages") +
  mapview(bugs, cex=6, col.regions="orange",
          layer.name="Bugs Stations") +
  mapview(algae, col.regions="green", cex=6,
          layer.name="Algae Stations") +
  mapview(h12, col.regions="dodgerblue", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="HUC12") 



m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# check projections are same
st_crs(gAll) == st_crs(h12)
st_crs(bioData) == st_crs(h12)


#### bio

all_bugs_h12 <- st_join(bugs,left = TRUE, h12[c("HUC_12")])
all_algae_h12 <- st_join(algae,left = TRUE, h12[c("HUC_12")])


## check data
dim(all_bugs_h12) ## 520
dim(all_algae_h12) ## 257
dim(all_gages_h12) ## 339
head(all_gages_h12)

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

unique(bugs_gages_h12$county)

## sf object with only coords for bugs - bug layer1 (i.e. change geometry to bug site)
bugs_gages_h12_bug_cds <- bugs_gages_h12 %>%
  select(remote_id,HUC_12:COMID.y) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID = COMID.y)

class(bugs_gages_h12_bug_cds)
head(bugs_gages_h12_bug_cds)

## join both bug layers together for all bug sites in same HUC as one gage
# all_bugs_gages_huc <- bind_rows(bugs_gages_h12_bug_cds, bugs_cal_gages_h12_bug_cds)
all_bugs_gages_huc <- distinct(bugs_gages_h12_bug_cds)

# number of unique?
length(unique(factor(bugs_gages_h12$HUC_12))) # h12=27
length(unique(bugs_gages_h12$masterid)) # bug sites = 106
length(unique(bugs_gages_h12$remote_id)) # gages = 32


####### Algae
algae_gages_h12 <- left_join(all_gages_h12, all_algae_h12_df, by = "HUC_12") %>% 
  distinct() %>% filter(!is.na(masterid))

## sf object with only coords for algae - algae layer1
algae_gages_h12_algae_cds <- algae_gages_h12 %>%
  select(remote_id, HUC_12:COMID.y) %>% as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID = COMID.y)

all_algae_gages_h12 <- distinct(algae_gages_h12_algae_cds)

# number of unique?
length(unique(factor(algae_gages_h12$HUC_12))) # h12=18
length(unique(algae_gages_h12$masterid)) # bug sites = 41
length(unique(algae_gages_h12$remote_id)) # gages = 21


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


Gages_sel_loc <- c(unique(algae_gages_h12$remote_id),
                   unique(bugs_gages_h12$remote_id))

length(Gages_sel_loc) ## 667

all_gages_h12

Gages_sel_loc

### filter gages to bio sites
all_gages_h12_sel <- all_gages_h12 %>%
  filter(remote_id %in% Gages_sel_loc) %>%
  as.data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) %>%
  na.omit()

dim(all_gages_h12_sel) ## 31

class(all_gages_h12_sel)

write.csv(all_gages_h12_sel, "output_data/02_bio_new_gages_same_huc.csv")
# write.csv(all_cal_gages_h12_sel, "output_data/02_bio_cal_gages_same_huc.csv")

### HUCs with bio

HUCs_sel <- c(unique(algae_gages_h12$HUC_12),
              unique(bugs_gages_h12$HUC_12))

length(unique(HUCs_sel)) ## 27

## filter HUCs with bio
h12 <- h12 %>%
  filter(HUC_12 %in% HUCs_sel)


# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

# this map of all sites in same HUC 12
m1 <- mapview(all_bugs_gages_huc, cex=6, col.regions="orange",
              layer.name="Bugs Stations") +
  mapview(all_algae_gages_h12, col.regions="green", cex=6,
          layer.name="Algae Stations") +
  mapview(h12, col.regions="dodgerblue", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="HUC12") +
  mapview(all_gages_h12_sel, col.regions="red", cex=2, color="red",
          layer.name="Gages") 



m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

## save all layers

st_write(h12, "output_data/02_h12_selected_new_gages.shp",append=FALSE) ## h12s with gages and bio
st_write(all_gages_h12_sel, "output_data/02_local_gages_sites_same_huc_as_bio_new_gages.shp",append=FALSE) ## local gages 
st_write(all_algae_gages_h12,  "output_data/02_algae_sites_same_huc_as_gages_new_gages.shp",append=FALSE) ## algae sites
st_write(all_bugs_gages_huc,  "output_data/02_bug_sites_same_huc_as_gages_new_gages.shp",append=FALSE) ## bug sites

# Overlapping sites -------------------------------------------------------

## which transect sites over lap directly with bio

## join bugs and algae with transects

bugs_trans <- st_join(all_bugs_gages_huc, nc_fin_h12 )
algae_trans <- st_join(all_algae_gages_h12, nc_fin_h12 )
## nothing matches directly


# GT UPSTREAM FLOWLINES FROM TRANSECTS ----------------------------------------

h12 <- st_read( "output_data/02_h12_selected_new_gages.shp") ## h12s with gages and bio
all_gages_h12_sel <- st_read("output_data/02_local_gages_sites_same_huc_as_bio_new_gages.shp") ## local gages 
all_algae_gages_h12 <- st_read("output_data/02_algae_sites_same_huc_as_gages_new_gages.shp") ## algae sites
all_bugs_gages_huc <- st_read("output_data/02_bug_sites_same_huc_as_gages_new_gages.shp") ## bug sites
#
h12

## TRANSFORM TO UTM datum for flowlines
all_algae_gages_h12 <- st_transform(all_algae_gages_h12, crs=3310) # use CA Teale albs metric
all_bug_gages_h12 <- st_transform(all_bugs_gages_huc, crs=3310)
all_gages_h12_sel <- st_transform(all_gages_h12_sel, crs=3310)
h12 <-  st_transform(h12, crs=3310)

# use a list of comids to make a list to pass to the nhdplusTools function
# first do gages, then see how many bio sites are on lines, then same with transects 
names(all_gages_h12_sel)
names(all_cal_gages_h12_sel)

all_cal_gages_h12_sel <- all_cal_gages_h12_sel %>%
  select(-COMID, Name:Filename, ID, COMID, gagelat, gagelong, HUC_12)   %>%
  # rename(COMID = COMID1) %>% ## remove retrieved comid, stick with original for now
  mutate(COMID = as.integer(COMID))

all_gages_h12_sel2 <- all_gages_h12_sel
head(all_gages_h12_sel2)

st_write(all_gages_h12_sel2, "output_data/02_selected_gages_Oct242022.shp")

data <- st_read("output_data/02_selected_gages_Oct242022.shp")
head(data)
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

length(unique(mainstems_flat_us$nhdplus_comid)) ## 204
length(unique(mainstems_flat_us$comid_origin)) ## 31

# bind together
mainstems_us <- sf::st_as_sf(mainstems_flat_us, use.names = TRUE, fill = TRUE)

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "UM")

length(unique(mainstems_us$nhdplus_comid)) ## 204
length(unique(mainstems_us$comid_origin)) ## 31

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
m1 <- mapview(mainstems_us, color = "navyblue") + 
  mapview(all_bugs_gages_huc, cex=6, col.regions="orange",
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
          layer.name="Gages (All)") 



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

length(unique(mainstems_flat_ds$nhdplus_comid)) ## 214
length(unique(mainstems_flat_ds$comid_origin)) ## 31

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

length(unique(mainstems_flat_dd$nhdplus_comid)) ## 411
length(unique(mainstems_flat_dd$comid_origin)) ## 31

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

# this map of all sites in same HUC 12 with flow lines
m1 <- mapview(mainstems_us, color = "navyblue") +
  mapview(mainstems_ds, color = "navyblue") +
  # mapview(mainstems_dd, color = "black") +
  mapview(all_bugs_gages_huc, cex=6, col.regions="orange",layer.name="Bugs Stations") +
  mapview(all_algae_gages_h12, col.regions="green", cex=6,
          layer.name="Algae Stations") +
  mapview(h12, col.regions="dodgerblue", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="HUC12") +
  mapview(sfer, col.regions="red", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="South Fork Eel") +
  mapview(all_gages_h12_sel, col.regions="red", cex=4, color="red",
          layer.name="Gages (All)") 


m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/ignore/new_gages_paired.html"),
        file = paste0(getwd(), "/ignore/new_gages_paired.png"))

### save out
st_write(mainstems_us, "output_data/02_upstream_flowlines_from_gage_new_gages.shp", append=FALSE)
st_write(mainstems_ds, "output_data/02_downstream_flowlines_from_gage_new_gages.shp", append=FALSE)
st_write(mainstems_dd, "output_data/02_diversions_flowlines_from_gage_new_gages.shp", append=FALSE)


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



