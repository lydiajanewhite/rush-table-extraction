library(readr)
library(tidyverse)

# 179

data1 <- read_csv(here::here("data","179_Brandt2019_plant_cover_data.csv"))
data <- read_csv(here::here("data","seed-add-data-output-full-plant-cover-noseed.csv"))
head(data)
unique(data$unit)
unique(data$seed.total) # check using seedless data 
unique(data1$ntrt)
## change treatment names, and make single treatment by joining stressor variables 
## ntrt: CONTROL = ambient, nutrient = enriched, organic_matter = organic -> N0_O0 and N1_O0 and N0_O1
## disturbance: 0 = ambient, 1= physical disturbance,  -> PD0 and PD1

data <- data %>% 
  mutate(nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "+N" ="N1_O0","+C"="N0_O1")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1")) %>% 
  unite("group_id", nutrient, organic_matter, disturbance) %>% 
  unite("location_id", site, block, plot, subplot, remove = F)

data <- data %>% 
  select(site, block, group_id, unit, subplot, year, taxa, cover) 

wide<- data %>% 
  pivot_wider(names_from = taxa, values_from = cover)

data1 <- data1 %>% 
  mutate(nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "#NAME?" ="N1_O0","+C"="N0_O1")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1")) %>% 
  unite("group_id", nutrient, organic_matter, disturbance) %>% 
  unite("location_id", site, block, plot, remove = F)

data1 <- data1 %>% 
  select( block, group_id, unit, year, taxa, cover) 

wide1<- data1 %>% 
  pivot_wider(names_from = taxa, values_from = cover)


test <- data1 %>% 
  filter (year > 2006)

# how many cases of n > 1? 
test <-data1 %>%  dplyr::group_by(group_id, block, unit, year, taxa) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

###

group_mean <- wide %>% 
  group_by (group_id, year, site) %>% 
  summarise_at(vars("LOTUS STRIGOSUS":"AMSINCKIA MENZIESII"), mean) 
%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("LOTUS STRIGOSUS":"AMSINCKIA MENZIESII"),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")


group_n <- wide %>% 
  group_by (group_id, year, site, unit) %>% 
  summarise_at(vars("LOTUS STRIGOSUS":"AMSINCKIA MENZIESII"), length)

%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

## sometimes 1, 2 or 3 subplots
wide %>%  
  group_by(year, subplot) %>% 
  summarise(n = n())

