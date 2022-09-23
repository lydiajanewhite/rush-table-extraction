library(readr)
library(tidyverse)

# 179

data <- read_csv(here::here("data","179_Brandt2019_plant_cover_data.csv"))
biomass_data <- read_csv(here::here("data","179_Brandt2019_plant_biomass_data.csv"))

head(data)
unique(data$unit)
unique(data$seed.total) # check using seedless data 
unique(biomass_data$year)  
unique(data$year)
## change treatment names, and make single treatment by joining stressor variables 
## ntrt: CONTROL = ambient, nutrient = enriched, organic_matter = organic -> N0_O0 and N1_O0 and N0_O1
## disturbance: 0 = ambient, 1= physical disturbance,  -> PD0 and PD1
## 
a <- biomass_data %>%  filter (unit == "2" & year == "2006") %>% 
  group_by(taxa) %>% 
  summarise (mean = mean(mass))


b <- data %>%  filter (unit == "2" & year == "2006")%>% 
  group_by(taxa) %>% 
  summarise (mean = mean(cover))

data <- data %>% 
  mutate(nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "#NAME?" ="N1_O0","+C"="N0_O1")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1")) %>% 
  unite("group_id", nutrient, organic_matter, disturbance) %>% 
  unite("location_id", site, block, plot)

#  then summarise mean sp abundance across treatments and pivot

head(data)
unique(data$group_id)
unique(data$unit)
unique(data$location_id)

data1 <- data %>% 
  select(group_id, unit, year, taxa, cover) %>% 
  group_by(group_id, unit, year, taxa) %>% 
  summarise(cover= mean(cover))

wide <- pivot_wider(data1, names_from = taxa, values_from = cover) 


data <- data %>% 
  select(group_id, unit, year, taxa, cover) 

wider <- data %>% 
  pivot_wider(names_from = taxa, values_from = cover)

