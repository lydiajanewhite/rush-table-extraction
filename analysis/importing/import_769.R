library(readr)
library(tidyverse)

# 769
data <- read_csv(here::here("data","769_Charna-Serna2018_invertebratealgal_abundance_gravel.csv"))
data1 <- read_csv(here::here("data","769_Charna-Serna2018_invertebratealgal_abundance_leaf.csv"))
head(data)

## change treatment names, and then make single treatment by joining stressor variables 
## Nutrients 0 1 -> N0 & N1
## Sediment  0  1 -> S0 & S1
## Insecticide  0  1 -> I0 & I1

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-1)
}

data <- data %>%  
  mutate(Nutrients = convert_to_categ(Nutrients, "N"),
         Sediment = convert_to_categ(Sediment, "S"),
         Insecticide = convert_to_categ(Insecticide, "I"),
         Habitat = recode(Habitat, "gravel" = "habitat1", "leaf" = "habitat2")) %>% 
  unite("group_id", Nutrients, Sediment, Insecticide,  Habitat, remove = F) 

data1 <- data1 %>%  
  mutate(Nutrients = convert_to_categ(Nutrients, "N"),
         Sediment = convert_to_categ(Sediment, "S"),
         Insecticide = convert_to_categ(Insecticide, "I"),
         Habitat = recode(Habitat, "gravel" = "habitat1", "leaf" = "habitat2")) %>% 
  unite("group_id", Nutrients, Sediment, Insecticide,  Habitat, remove = F) 


group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Acari":"Zaitzevia"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_mean1 <- data1 %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Bezzia":"Tanypodinae"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean") %>% 
  rbind(. , group_mean)


group_sd <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Acari":"Zaitzevia"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_sd1 <- data1 %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Bezzia":"Tanypodinae"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd") %>% 
  rbind(. , group_sd)

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Acari":"Zaitzevia"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

group_n1 <- data1 %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Bezzia":"Tanypodinae"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n") %>% 
  rbind(. , group_n)

summary_769 <- inner_join(group_mean1,group_sd1) %>% 
  inner_join(., group_n1)

summary_769$filename <-'769_Charna-Serna2018_invertebratealgal_abundance'

