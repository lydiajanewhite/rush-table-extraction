### extracting data form species abundance tables ###

library(readr)
library(tidyverse)

# 162
data <- read_csv(here::here("data","162_Davis2019_invertebrate_abundance_data.csv"))
head(data)

## change treatment names, and make single treatment by joining stressor variables 
## sediment: 0 = ambient, H = high -> S0 and S1
## nitrogen: 0N = ambient, CN = chronic, AN = acute  -> N0 and N1 and N2
## phosphorus: 0P = ambient, CP = chronic, AP = acute  -> PH0 and PH1 and PH2

data<-data%>%
  mutate(Nitrogen=recode(Nitrogen, '0N' ="N0", CN="N1",AN="N2"))%>%
  mutate(Phosphorus=recode(Phosphorus, '0P' ="PH0", CP="PH1",AP="PH2")) %>% 
  mutate(Sediment =recode(`Sediment Level`, '0' ="S0", H="S1")) %>% 
  unite("group_id", Sediment, Nitrogen, Phosphorus)
 
#  then summarise mean sp abundance across treatments and pivot

mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Siphonoperla torrentium':'Hydraenea'), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Siphonoperla torrentium':'Hydraenea'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Siphonoperla torrentium':'Hydraenea'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_162 <- inner_join(mean,sd) %>% 
  inner_join(., n)

summary_162$filename <-'162_Davis2019_invertebrate_abundance_data'
## summary<-error_type <-'sd'     can be added  at final stage 
## summary<-plot_type <-'table'

# 179

data <- read_csv(here::here("data","179_Brandt2019_plant_cover_data.csv"))
head(data)
unique(data$ntrt)
unique(data$dist)
unique(data$site)
## change treatment names, and make single treatment by joining stressor variables 
## ntrt: CONTROL = ambient, nutrient = enriched, organic_matter = organic -> N0_O0 and N1_O0 and N0_O1
## disturbance: 0 = ambient, 1= physical disturbance,  -> PD0 and PD1
## 

data<-data%>%
  mutate(nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "nutrient" ="N1_O0","organic_matter"="N0_O1")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1")) %>% 
  unite("group_id", nutrient, organic_matter, disturbance)

#  then summarise mean sp abundance across treatments and pivot



