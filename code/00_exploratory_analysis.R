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
load(file = "input_data/SMC_phab_cali.csv")

head(phab_ca)

# write.csv(phab_1, "input_data/SMC_phab_north_coast2.csv")

unique(phab_ca$sampledate) ## 2000-2009???
unique(phab_ca$masterid) ## 185

## bio data
load(file = "input_data/SMC_bmi_cali.csv") ## bug_tax_ca
load(file = "input_data/SMC_algae_cali.csv") ## alg_tax_ca
load(file = "input_data/SMC_cap_prob_cali.csv") ## oe_ca
head(bug_tax_ca)

bug_sites <- bug_tax_ca %>%
  select("masterid",  "latitude", "longitude", "county") %>%
  distinct()

write.csv(bug_sites, "output_data/00_bug_sites_all.csv")

algae_sites <- alg_tax_ca %>%
  select("masterid",  "latitude", "longitude", "county") %>%
  distinct()

write.csv(algae_sites, "output_data/00_algae_sites_all.csv")

# Missingness -------------------------------------------------------------

## physical data only
head(phab_ca)

length(unique(phab_ca$masterid))
length(unique(phab_ca$sampledate))
## make wide

names(phab_ca)

#ID01 = data.table::rleid(result)
?distinct
phabWide <- phab_ca %>%
  select(masterid, latitude, longitude, analytename, sampledate,result) %>%
  group_by(masterid, analytename) %>%
  mutate(TransectNum =  row_number()) %>%
  pivot_wider(names_from = "analytename", values_from = "result")# %>%
  
phabWideMis <- phabWide %>%
  ungroup() %>%
    select(-c(masterid, latitude, longitude,sampledate, TransectNum))

  head(phabWide)
  
library(Amelia)
library(reshape2)

missmap(phabWideMis)

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present (4%)","Missing (96%)")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1)) + #vjust=0.5
    labs(title = "California PHAB missingness",x = "Variables",
         y = "Observations")
}

mis <- ggplot_missing(phabWideMis)

mis

file.name1 <- paste0(out.dir, "00_Cali_Phab_missingness.jpg")
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

head(bug_tax_ca)
head(traits)

## rename to match
species <- bug_tax_ca %>%
  rename(FinalID = finalid)

sum(species$FinalID %in% mydf$Genus) ## 370704

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
unique(phab_ca$analytename)
## metrics with least missing data
metrics <- c("Slope", "Embeddedness", "Substrate Size Class", "Wetted Width",  "Bank Angle" )

phab <- phab_ca %>%
  select( masterid, analytename, result, sampledate) %>%
  filter(analytename %in% metrics) %>%
  group_by(masterid,  sampledate, analytename) %>%
  mutate(TransectNum =  row_number()) #%>%
  # pivot_wider(names_from = "analytename", values_from = "result")

sum(unique(phab$masterid) %in% unique(species_traits$masterid)) ## 3224

bio_phab <- full_join(species_traits,phab, by=c("masterid", "sampledate"))

head(bio_phab)
dim(bio_phab)
length(unique(bio_phab$masterid)) ## 5979

# Capture probability -----------------------------------------------------
load("input_data/SMC_cap_prob_cali.csv")

head(oe_ca1)

sum(unique(oe_ca$otu) %in% unique(bio_phab$FinalID)) ## 328
length(unique(oe_ca$masterid)) ## 4305

## get data column from sample id: first separate sampleid
oe_ca1 <- oe_ca %>%
  rename(FinalID = otu) %>%
  separate(sampleid, into= c("site", "date"),extra = "drop", sep= "_") %>%
  separate(date, into = c("M1", "M2", "D1", "D2", "Y1", "Y2", "Y3", "Y4")) %>%
  unite(M1:M2, "Month")


oe_ca2 <- oe_ca1 %>%
  mutate(sampledate = as.factor(date)) 
?unite
str(oe_ca2)

## format date time
oe_ca2$sampledate<-as.POSIXct(oe_ca2$sampledate,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")
  
as.Date(oe_ca2$sampledate)


head(oe_ca1)

?separate

## match bio data with cap prob
bio_phab_cap <- full_join(bio_phab, oe_ca, by=c("masterid", "sampledate", "latitude", "longitude"))  





