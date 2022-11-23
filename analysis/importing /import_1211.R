library(tidyverse)

# 1211
data <- read_csv(here::here("data","1211_Mayer-Pinto2016_invertebrate abundance.csv"))
unique(data$Treatment)
unique(data$Time)
head(data)

## change treatment names, already single treatment 
## blank is approapriate control to use because it controls for presence of block 
## but not contaminaints 
## insecticdes of different chemicals -> IC0 & IC1 (carboryl) , 
## IF0 & IF1 (iron phosphate) 
## IM0 & IM1 (Metaldehyde)
                 
data <- data %>% 
  filter (Treatment != "Con") %>% 
  mutate(treatment = recode(Treatment, 'Blank' ="IC0_IF0_IM0", 'Car' ="IC1_IF0_IM0",
                            'Fe' ="IC0_IF1_IM0",  'Metal' ="IC0_IF0_IM1"), 
         site = recode(Site, '1' = "site1", '2' = "site2"),
         habitat = recode(Grasstype, 'Short' = "shortgrass", 'Long' = "longgrass")) %>% 
  unite("group_id", treatment, site, habitat, remove = F) 

group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Oligochaetes":"Insect larvae"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <-  data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Oligochaetes":"Insect larvae"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Oligochaetes":"Insect larvae"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1211 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_1211$filename <-'1211_Mayer-Pinto2016_invertebrate_abundance'
