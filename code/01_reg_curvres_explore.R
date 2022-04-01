### test SoCal regional curves in north coast

## packages
library(tidyverse)
library(tidylog)

# workflow
# upload raw index values with delta h
# plot index values with a few metrics
# get FFM and bio data in NC
# compare curves or check where points are



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


# North Coast  ------------------------------------------------------------


