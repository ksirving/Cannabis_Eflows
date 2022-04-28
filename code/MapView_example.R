## map view

library(sf)
library(mapview)

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

# add in any spatial layers and give them names/colours etc
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