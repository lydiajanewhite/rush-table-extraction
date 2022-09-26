library(readr)
library(tidyverse)

# 179

data <- read_csv(here::here("data","179_Brandt2019_plant_cover_data.csv"))

head(data)
unique(data$unit)
unique(data$seed.total) # check using seedless data 
unique(biomass_data$year)  
unique(data$year)
## change treatment names, and make single treatment by joining stressor variables 
## ntrt: CONTROL = ambient, nutrient = enriched, organic_matter = organic -> N0_O0 and N1_O0 and N0_O1
## disturbance: 0 = ambient, 1= physical disturbance,  -> PD0 and PD1

data <- data %>% 
  mutate(nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "#NAME?" ="N1_O0","+C"="N0_O1")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1")) %>% 
  unite("group_id", nutrient, organic_matter, disturbance) %>% 
  unite("location_id", site, block, plot, remove = F)

data <- data %>% 
  select( block, group_id, unit, year, taxa, cover) 

wide<- data %>% 
  pivot_wider(names_from = taxa, values_from = cover)

# how many cases of n > 1? 
test <-data %>%  dplyr::group_by(group_id, block, unit, year, taxa) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
