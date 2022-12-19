library(tidyverse)

# 765
data <- read_csv(here::here("data","765_Beerman2018_invertebrate_abundance.csv"))
head(data)
unique(data$Treatment)

data <- data %>% 
  filter(!if_all(Mesocosm:Stenophylax, is.na))

## Already single treatment, change treatment names
## reduced water velocity: L -> V0 & V1
## sedimentation: S ->  S0 & S1
## increased salinity: N -> NA0 & NA1 

data <- data %>% 
  mutate(treatment =recode(Treatment, 'C' ="S0_V0_NA0", 
                          'S' ="S1_V0_NA0",'L' ="S0_V1_NA0", 'N' ="S0_V0_NA1",
                          'LN' ="S0_V1_NA1", 'LS' ="S1_V1_NA0", 'NS' ="S1_V0_NA1",
                          'LNS' ="S1_V1_NA1"),
         habitat = recode(Compartment, "0" = "habitat1", "1" ="habitat2",  "2" = "habitat3")) %>% 
  unite("group_id", treatment, habitat, remove = F) 


group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Nematoda":"Diptera pupa"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Nematoda":"Diptera pupa"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Nematoda":"Diptera pupa"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_765 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_765$filename <-'765_Beerman2018_invertebrate_abundance'

saveRDS(summary_765, file = "output/individual_datasets/summary_765.rds") 
