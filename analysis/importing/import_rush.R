### extracting data form species abundance tables ###

library(tidyverse)

# RUSH i.e. my experimental data 
data <- read_csv(here::here("data","RUSH_White2023_macroalgae_cover.csv"))
head(data)
data_micro <- read_csv(here::here("data","RUSH_White2023_microalgae_cover.csv"))
head(data_micro)

## treatment names ok but need to separate and add time points - TP

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-6)
}

data <- data %>%  
  mutate(Time = convert_to_categ(Time, "TP"),
        Consumers = recode(Diversity, "A" = "graz0_pred0", "B" = "graz1_pred0", "C" = "graz1_pred1", "D" = "control")) %>% 
  unite("group_id", Nutrients, Sediment, Consumers, Time, remove = F) %>% 
  filter( Diversity !="D")

data_micro <- data_micro %>%  
  mutate(Time = convert_to_categ(Time, "TP"),
         Consumers = recode(Diversity, "A" = "graz0_pred0", "B" = "graz1_pred0", "C" = "graz1_pred1", "D" = "control")) %>% 
  unite("group_id", Nutrients, Sediment, Consumers, Time, remove = F) %>% 
  filter( Diversity !="D")

data <- left_join(data, data_micro) %>% 
  select (-Factor, - Consumers)
#  then summarise mean sp abundance across treatments and pivot

group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Fuc_Ves':'Diatoms'), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Fuc_Ves':'Diatoms'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Fuc_Ves':'Diatoms'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_rush <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n) 

summary_rush$filename <-'0_White2023_algae_cover'

saveRDS(summary_rush, file = "output/individual_datasets/summary_rush.rds") 

