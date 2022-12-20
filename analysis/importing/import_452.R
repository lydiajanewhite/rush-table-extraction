# 452

library(tidyverse)

data <- read_csv(here::here("data","452_White2018_invertalgae_abundance_td.csv"))
unique(data$Predators)
## change treatment names, and make single treatment by joining stressor variables 
## temperature:  ambient, heated -> T0 and T1
## nutrients: ambient, enriched -> T0 and T1 N0 and N1 
## predators: remove spaces and capitals none, crab, whelk, crab_whelk

data<-data%>%
  mutate(Temperature=recode(Temperature, Ambient ="T0", Heated ="T1"))%>%
  mutate(Nutrient=recode(Nutrient, Ambient ="N0", Enriched="N1")) %>% 
  mutate(Predators =recode(Predators, None = "none", Crab ="crab", Whelk ="whelk", 'Crab & Whelk' ="crabwhelk")) %>% 
  unite("group_id", Temperature, Nutrient, Predators)

#  then summarise mean sp abundance across treatments and pivot

group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('serratus':'caprella sp'), mean)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")%>% 
  mutate(mean = ifelse(is.na(mean), 0, mean))

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('serratus':'caprella sp'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('serratus':'caprella sp'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_452<- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_452$filename <- '452_White2018_invertalgae_abundance'

saveRDS(summary_452, file = "output/individual_datasets/summary_452.rds") 
