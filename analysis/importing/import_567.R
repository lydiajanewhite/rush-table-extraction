library(tidyverse)

# 567
data <- read_csv(here::here("data","567_Varma2018_plant_biomass.csv"))

## change treatment names, already single treatment N + P
## CONTROL = ambient, N+ = N enriched, P+ = P enriched, NP+ both -> N0_PH0 and N1_PH0 and N0_PH1 and N1_PH1

data <- data %>% 
  mutate(group_id=recode(Treat, "Con" ="N0_PH0", "N+"="N1_PH0", "P+"="N0_PH1", "NP+" = "N1_PH1")) %>% 
  rename(variable = Spp)
  
group_mean <- data %>% 
  group_by (group_id, variable ) %>% 
  summarise(mean =mean (TotalBio)) 

group_sd <- data %>% 
  group_by (group_id, variable ) %>% 
  summarise(sd = sd (TotalBio)) 

group_n <- data %>% 
  group_by (group_id, variable ) %>% 
  summarise(n = n()) 

summary_567 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_567$filename <-'567_Varma2018_plant_biomass'

saveRDS(summary_567, file = "output/individual_datasets/summary_567.rds") 
