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

head(delta_bio)

# Bio sites data ----------------------------------------------------------

## upload all scores
csciNC <- read.csv("ignore/csci_ca_scores.csv")
asciNC <- read.csv("ignore/asci_ca_scores.csv")

head(csciNC)
head(asciNC)

## format and filter to paired sites

nocalBio <- unique(sel_bio_sites$masterid)

csciNC <- csciNC %>%
  select(stationcode, sampleyear, csci) %>%
  filter(stationcode %in% nocalBio) %>%
  rename(masterid = stationcode)


unique(asciNC$assemblage)

asciNC <- asciNC %>%
  select(sampleid, stationcode, metric, result, sampledate, assemblage) %>%
  filter(assemblage == "Hybrid", metric == "ASCI", stationcode %in% nocalBio) %>%
  pivot_wider(names_from = metric, values_from = result) %>%
  rename(masterid = stationcode) %>%
  na.omit()

head(asciNC)
sum(is.na(asciNC))

## change names to match delta data
delta_bio <- delta_bio %>%
  filter(FFM %in% c("d_ds_dur_ws", "d_ds_mag_50", "d_ds_mag_90", "d_ds_tim", "d_fa_mag", "d_fa_tim",
                    "d_sp_dur","d_sp_mag","d_sp_tim","d_wet_bfl_dur","d_wet_bfl_mag_10", "d_wet_bfl_mag_50", "d_wet_tim")) %>%
  mutate(hydro.endpoints = case_when(FFM == "d_ds_dur_ws" ~ "DS_Dur_WS",
                                    FFM == "d_ds_mag_50" ~ "DS_Mag_50",
                                    FFM == "d_ds_mag_90" ~ "DS_Mag_90",
                                    FFM == "d_ds_tim" ~ "DS_Tim",
                                    
                                    FFM == "d_fa_mag" ~ "FA_Mag",
                                    FFM == "d_fa_tim" ~ "FA_Tim",
                                    
                                    FFM == "d_sp_dur" ~ "SP_Dur",
                                    FFM == "d_sp_mag" ~ "SP_Mag",
                                    FFM == "d_sp_tim" ~ "SP_Tim",
                                    
                                    FFM == "d_wet_bfl_dur" ~ "Wet_BFL_Dur",
                                    FFM == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                    FFM == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50",
                                    FFM == "d_wet_tim" ~ "Wet_Tim"))  


## join to bio data to delta bio, scale and add +ve/-ve type

library(scales)

delta_asci <- delta_bio %>%
  filter(Index == "ASCI") %>%
  right_join(asciNC, by = "masterid") %>% na.omit() %>%
  filter(ASCI <= 0.86) %>%
  mutate(ResultScaled = rescale(ASCI, to = c(0, 1))) %>% 
  mutate(Type = ifelse(MedDelta < 0, "Negative", "Positive"))

head(delta_asci)


delta_csci <- delta_bio %>%
  filter(Index == "CSCI") %>%
  right_join(csciNC , delta_bio, by = "masterid") %>% na.omit() %>%
  group_by(hydro.endpoints) %>%
  filter(csci <= 0.79) %>%
  mutate(ResultScaled = rescale(csci, to = c(0, 1))) %>% 
  mutate(Type = ifelse(MedDelta < 0, "Negative", "Positive"))
  


## scale scores to fit axis? no


# visualise ---------------------------------------------------------------

out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/figures/"

## make original curves
## add points from nocal

all_csci <- all_csci %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.79) 

## define FFM to loop through
HydroEnds <- unique(delta_csci$hydro.endpoints)

HydroEnds
m=12

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_csci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_cscix <- subset(all_csci,hydro.endpoints == paste(HydroEnds[m]))
  all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$hydro),]
  
  ## subset NoCal data
  delta_cscix <- subset(delta_csci, hydro.endpoints == paste(HydroEnds[m]))
 
  
  ## plot
  q3 <- ggplot(all_cscix, aes(x=hydro, y=PredictedProbabilityScaled))+
    geom_path()+
    geom_point(data = delta_cscix, aes(x=MedDelta, y = ResultScaled, col = "red")) +
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous()+
    theme_minimal()+
    theme(legend.position = "none") +
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
  q3
  
  out.filename <- paste0(out.dir,"01_csci_", paste(HydroEnds[m]), "_0.79.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  
}

#### ASCI

all_asci <- all_asci %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.86) 


## define FFM to loop through
HydroEnds <- unique(delta_csci$hydro.endpoints)

HydroEnds
m=12

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_asci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_ascix <- subset(all_asci,hydro.endpoints == paste(HydroEnds[m]))
  all_ascix <- all_ascix[order(all_ascix$PredictedProbabilityScaled, all_ascix$hydro),]
  
  ## subset NoCal data
  delta_ascix <- subset(delta_asci, hydro.endpoints == paste(HydroEnds[m]))
  
  
  ## plot
  q3 <- ggplot(all_ascix, aes(x=hydro, y=PredictedProbabilityScaled))+
    geom_path()+
    geom_point(data = delta_ascix, aes(x=MedDelta, y = ResultScaled, col = "red")) +
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous()+
    theme_minimal()+
    theme(legend.position = "none") +
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
  q3
  
  out.filename <- paste0(out.dir,"01_asci_", paste(HydroEnds[m]), "_0.86.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  
}

