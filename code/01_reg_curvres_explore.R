### test SoCal regional curves in north coast

## packages
library(tidyverse)
library(tidylog)
library(sf)

# workflow
# upload raw index values with delta h
# plot index values with a few metrics
# get FFM and bio data in NC
# compare curves or check where points are on probability curves


# Raw data for curves -----------------------------------------------------

asci <- read.csv("input_data/00_asci_delta_formatted_median_Nov2021.csv")
csci <- read.csv("input_data/00_csci_delta_formatted_median_updated_Nov2021.csv")

head(asci)
head(csci)

## format and make long

asci <- asci %>%
  select(-X, -NA.) %>%
  pivot_longer(DS_Dur_WS:Wet_Tim, names_to = "FlowMetric", values_to = "DeltaH") %>%
  mutate(Type = ifelse(DeltaH < 0, "Negative", "Positive"))
  
csci <- csci %>%
  select(-X) %>%
  pivot_longer(DS_Dur_WS:Wet_Tim, names_to = "FlowMetric", values_to = "DeltaH") %>%
  mutate(Type = ifelse(DeltaH < 0, "Negative", "Positive"))



# Plots -------------------------------------------------------------------

ASCI1 <- ggplot(data=asci, aes(x = DeltaH, y= H_ASCI)) +
  # geom_smooth(method = "GLM", se = FALSE ) +
  geom_point() +
  facet_wrap(~FlowMetric, scales = "free_x") +
  labs(title = "Raw Flow-Eco curves", x = "Delta H", y = "ASCI Score") 

ASCI1

CSCI1 <- ggplot(data=csci, aes(x = DeltaH, y= csci)) +
  # geom_smooth(method = "GLM", se = FALSE ) +
  geom_point() +
  facet_wrap(~FlowMetric, scales = "free_x") +
  labs(title = "Raw Flow-Eco curves", x = "Delta H", y = "CSCI Score") 

CSCI1


# Probability curves  ------------------------------------------------------------

## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels


# data ASCI ---------------------------------------------------------------

## upload data
all_asci <- read.csv("input_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv")

## scale probability
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

all_asci <- left_join(all_asci, labels, by ="hydro.endpoints")

head(all_asci)


# data CSCI ---------------------------------------------------------------

## upload data
all_csci <- read.csv("input_data/01_CSCI_neg_pos_logR_metrics_figures_April2021.csv")

## scale probability
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

all_csci <- left_join(all_csci, labels, by ="hydro.endpoints")

head(all_csci)


# data Hydro --------------------------------------------------------------

delta <- read.csv("/Users/katieirving/SCCWRP/Cannabis E-Flows - Documents/General/Data/RawData/Paired_Gages/FFMs/PairedGages_DeltaH_pt1.csv")
head(delta)

length(unique(delta$ID))
sort(unique(delta$wayr))

## get median to match SoCal prob curves

delta_med <- delta %>%
  pivot_longer(d_ds_dur_ws:d_wet_tim, names_to = "FFM", values_to = "values") %>%
  filter(wayr < 2001) %>% ## match the bio years
  group_by(Name, Filename, ID, gagelat, gagelong, COMID, gageid, FFM) %>%
  summarise(MedDelta = median(na.omit(values)))


head(delta_med)


# Paired gages with Bio ---------------------------------------------------

sel_bugs_coms_final <- st_read( "output_data/03_bug_sites_gages.shp")
sel_algae_coms_final <- st_read("output_data/03_algae_sites_gages.shp")

head(sel_bugs_coms_final)

## add identifier column for joining
sel_bugs_coms_final <- sel_bugs_coms_final %>% mutate(Index = "CSCI")
sel_algae_coms_final <- sel_algae_coms_final %>% mutate(Index = "ASCI")

## join bio sites

sel_bio_sites <- bind_rows(sel_bugs_coms_final, sel_algae_coms_final)

head(sel_bio_sites)
dim(sel_bio_sites)
unique(sel_bio_sites$ID)


# Join all data -----------------------------------------------------------

delta_bio <- full_join(delta_med, sel_bio_sites, by = "ID")


