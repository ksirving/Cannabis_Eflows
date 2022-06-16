## select bio sites and survey sites on selected flow lines

library(readr)
library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)
library(nhdplusTools)

## upload data 
h12 <- st_read( "output_data/02_h12_selected.shp") ## h12s with gages and bio
all_gages_h12_sel <- st_read("output_data/02_local_gages_sites_same_huc_as_bio.shp") ## local gages 
all_cal_gages_h12_sel <- st_read("output_data/02_cali_gages_sites_same_huc_as_bio.shp") ## cali gages
all_algae_gages_h12 <- st_read("output_data/02_algae_sites_same_huc_as_gages.shp") ## algae sites
all_bugs_gages_huc <- st_read("output_data/02_bug_sites_same_huc_as_gages.shp") ## bug sites
nc_fin_h12 <- st_read("output_data/02_transect_surveys_same_huc_as_gages_and_bio.shp") ## bug sites

## flowlines
mainstems_us <- st_read( "output_data/02_upstream_flowlines_from_gage.shp")
mainstems_ds <- st_read( "output_data/02_downstream_flowlines_from_gage.shp")
mainstems_dd <- st_read( "output_data/02_diversions_flowlines_from_gage.shp")

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds, mainstems_dd)

## south fork eel
sfer <- st_read("ignore/Web_GIS_Cannabis/Belize_Morphology/SouthForkEel.shp")


## map
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

# get distinct segs only 
names(mainstems_all)
mainstems_distinct <- mainstems_all %>% distinct(nhdpls_, .keep_all=TRUE)

# all algae comids that occur in list of mainstem NHD comids: (n=723)
names(all_algae_gages_h12)
sel_algae_coms_final <- all_algae_gages_h12 %>% 
  filter(COMID %in% mainstems_distinct$nhdpls_)

# distinct comid/algae/gages combinations:
sel_algae_coms_final %>% st_drop_geometry() %>% 
  distinct(masterid, ID) %>% tally() # n=320

# distinct algae COMIDs
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(COMID) %>% tally() #62

# distinct Algae sites
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(masterid) %>% tally() # 74

# distinct gages
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(ID) %>% tally() # 184

# all bug comids that occur in list of mainstem NHD comids: (n=723)
names(all_algae_gages_h12)
sel_bugs_coms_final <- all_bugs_gages_huc %>% 
  filter(COMID %in% mainstems_distinct$nhdpls_)

# distinct comid/bug/gages combinations:
sel_bugs_coms_final %>% st_drop_geometry() %>% 
  distinct(masterid, ID) %>% tally() # n=953

# distinct bug COMIDs
sel_bugs_coms_final %>% st_drop_geometry() %>% distinct(COMID) %>% tally() #127

# distinct bug sites
sel_bugs_coms_final %>% st_drop_geometry() %>% distinct(masterid) %>% tally() # 170

# distinct gages - bugs
sel_bugs_coms_final %>% st_drop_geometry() %>% distinct(ID) %>% tally() # 282

## save out

st_write(sel_bugs_coms_final, "output_data/03_bug_sites_gages.shp")
st_write(sel_algae_coms_final, "output_data/03_algae_sites_gages.shp")

# FINAL MAP -------------------------------------------------------

# create a final map of selected gages and algae + huc12 + flowlines

# get all algae not selected...check why not on map
algae_not_selected <- all_algae_gages_h12 %>% filter(!COMID %in% mainstems_distinct$nhdpls_) # should be 135 (loss of 70% of data)

# get all gages selected (n=98)
algae_gages_selected <- all_gages_h12_sel %>% 
  filter(ID %in% sel_algae_coms_final$ID)

# get the gages not selected (n=82)
algae_gages_not_selected <- all_gages_h12_sel %>% 
  filter(!ID %in% sel_algae_coms_final$ID)

# get the hucs selected (n=198)
algae_hucs_selected <- h12 %>% 
  filter(HUC_12 %in% sel_algae_coms_final$HUC_12)

# get the hucs not selected (n=53)
algae_hucs_not_selected <- h12 %>% 
  filter(!HUC_12 %in% sel_algae_coms_final$HUC_12)

##### Bugs
# get all algae not selected...check why not on map
bugs_not_selected <- all_bugs_gages_huc %>% filter(!COMID %in% mainstems_distinct$nhdpls_) # should be 135 (loss of 70% of data)

# get all gages selected (n=153)
bug_gages_selected <- all_gages_h12_sel %>% 
  filter(ID %in% sel_bugs_coms_final$ID)

# get the gages not selected (n=27)
bug_gages_not_selected <- all_gages_h12_sel %>% 
  filter(!ID %in% sel_bugs_coms_final$ID)

# get the hucs selected (n=75)
bug_hucs_selected <- h12 %>% 
  filter(HUC_12 %in% sel_bugs_coms_final$HUC_12)

# get the hucs not selected (n=33)
bug_hucs_not_selected <- h12 %>% 
  filter(!HUC_12 %in% sel_bugs_coms_final$HUC_12)

## 
mapviewOptions(fgb = FALSE)

# this map of all sites selected U/S and D/S - algae
m2 <- mapview(sel_algae_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected Algae Sites") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  ## algae
  mapview(algae_gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected Algae Gages") +
  # these are all algae or gages in same H12 but not selected
  mapview(algae_gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other Algae Gages") +
  mapview(algae_not_selected, col.regions="gold", color="gray20", cex=3.2,
          layer.name="Other Algae Sites in H12") +
  mapview(algae_hucs_selected, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected Algae HUC12") +
  mapview(algae_hucs_not_selected, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other Algae HUC12") +
  ## south fork eel and survey sites
  mapview(sfer, col.regions="red", alpha.region=0.1,
          color="darkblue", legend=FALSE, layer.name="South Fork Eel") +
  mapview(nc_fin_h12, col.regions="purple", cex=2, color="purple",
          layer.name="Transect Survey Sites")

m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this final map out as:"map_of_final_gages_algae_stations_all_gages"
#mapshot(m2, url = paste0(here::here(),"/figs/03_map_of_final_algae_stations_gages_h12s.html"))

# this map of all sites selected U/S and D/S - bugs
m3 <- mapview(sel_bugs_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected Bug Sites") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  ## bugs
mapview(bug_gages_selected, col.regions="skyblue", cex=7, color="blue2",
        layer.name="Selected Bug Gages") +
# these are all algae or gages in same H12 but not selected
mapview(bug_gages_not_selected, col.regions="slateblue", color="gray20",
        cex=3.2, layer.name="Other Bug Gages") +
mapview(bugs_not_selected, col.regions="green", color="gray20", cex=3.2,
        layer.name="Other Bug Sites in H12") +
mapview(bug_hucs_selected, col.regions="orange", alpha.region=0.1,
        color="orange", legend=F, layer.name="Selected Bug HUC12 ") +
mapview(bug_hucs_not_selected, col.regions="blue", alpha.region=0.1,
        color="darkblue", legend=F, layer.name="Other Bug HUC12") +
## south fork eel and survey sites
mapview(sfer, col.regions="red", alpha.region=0.1,
        color="darkblue", legend=FALSE, layer.name="South Fork Eel") +
  mapview(nc_fin_h12, col.regions="purple", cex=2, color="purple",
          layer.name="Transect Survey Sites")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


