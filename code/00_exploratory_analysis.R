### exploratory analysis

## workflow
# 1 - missingness map of data
# 2 - join data
# 3 - models of bio~hydraulics

## packages
library(tidyverse)
library(tidylog)

## figure directory

out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/figures"

## physical data
load(file = "input_data/SMC_phab_north_coast.csv")

head(phab_1)

write.csv(phab_1, "input_data/SMC_phab_north_coast2.csv")

unique(phab_1$sampledate) ## 2000-2009???
unique(phab_1$masterid) ## 185

## bio data
load(file = "input_data/SMC_bmi_north_coast.csv") ## bug_tax_1
load(file = "input_data/SMC_algae_north_coast.csv") ## alg_tax_1
load(file = "input_data/SMC_cap_prob_north_coast.csv") ## oe_1
head(bug_tax_1)

bug_sites <- bug_tax_1 %>%
  select("masterid",  "latitude", "longitude", "county") %>%
  distinct()

write.csv(bug_sites, "output_data/00_bug_sites_all.csv")

algae_sites <- alg_tax_1 %>%
  select("masterid",  "latitude", "longitude", "county") %>%
  distinct()

write.csv(algae_sites, "output_data/00_algae_sites_all.csv")

# Missingness -------------------------------------------------------------

## physical data only
head(phab_1)

## make wide

names(phab_1)

#ID01 = data.table::rleid(result)

phabWide <- phab_1 %>%
  select( masterid, latitude, longitude, analytename, result, sampledate) %>%
  group_by(masterid,  sampledate, analytename) %>%
  mutate(TransectNum =  row_number()) %>%
  pivot_wider(names_from = "analytename", values_from = "result") %>%
  ungroup() %>%
  select(-c(masterid, latitude, longitude,sampledate, TransectNum))

library(Amelia)
library(reshape2)

missmap(phabWide)

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present (8%)","Missing (92%)")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1)) + #vjust=0.5
    labs(title = "North Coast PHAB missingness",x = "Variables",
         y = "Observations")
}

mis <- ggplot_missing(phabWide)

mis

file.name1 <- paste0(out.dir, "00_NorthCoast_Phab_missingness.jpg")
ggsave(mis, filename=file.name1, dpi=600, height=5, width=6)

citation()
# Bugs --------------------------------------------------------------------

# library(devtools)
# install_github("SCCWRP/CSCI", force=T)
library(CSCI)   

## uplaod traits from csci package
mydf<-loadMetaData()          
head(mydf)

## get columns with FFG, tolerance
traits <- mydf %>%
  select(FinalID:Subphylum, Invasive, Source)

head(bug_tax_1)
head(traits)

## rename to match
species <- bug_tax_1 %>%
  rename(FinalID = finalid)

sum(species$FinalID %in% mydf$Genus) ## 42110

## join taxa and traits 
species_traits <- full_join(species, traits, by="FinalID")

head(species_traits)

length(unique(species_traits$masterid))

names(species_traits)

## only relevant columns and remove duplicates
species_traits <- species_traits %>%
  select(masterid, latitude, longitude, sampledate, FinalID, FunctionalFeedingGroup, ToleranceValue) %>%
  distinct()
## 76,858 rows remaining

## join to phab data
unique(phab_1$analytename)
## metrics with least missing data
metrics <- c("Slope", "Embeddedness", "Substrate Size Class", "Wetted Width",  "Bank Angle" )

phab <- phab_1 %>%
  select( masterid, analytename, result, sampledate) %>%
  filter(analytename %in% metrics) %>%
  group_by(masterid,  sampledate, analytename) %>%
  mutate(TransectNum =  row_number()) #%>%
  # pivot_wider(names_from = "analytename", values_from = "result")

sum(unique(phab$masterid) %in% unique(species_traits$masterid)) ## 173

bio_phab <- full_join(species_traits,phab, by=c("masterid", "sampledate"))

head(bio_phab)


# Capture probability -----------------------------------------------------

head(oe_1)
