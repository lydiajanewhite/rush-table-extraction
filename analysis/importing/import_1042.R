library(tidyverse)

# 1042
data <- read_csv(here::here("data","1042_Baekkelie2017_macroinvertebrate_abundance.csv"))
unique(data$Treatment)
unique(data$Time)
head(data)

## change treatment names, already single treatment 
## Nutrients np -> N0 & N1
## reduced water velocity:  nf & lf -> V0 & V1

data <- data %>% 
  mutate(treatment = recode(Treatment, 'C' ="N0_V0", 'NP' ="N1_V0",
                           'F' ="N0_V1", 'NPF' ="N1_V1"),
         time = recode(Time, "july_2015_preflood" = "TP0", "july_2015_postflood" ="TP1", 
                       "september_2015_preflood" = "TP2", "september_2015_postflood" = "TP3")) %>% 
  unite("group_id", treatment, time, remove = F) 

## summarise across treatments, T2 is kinda like T0, i.e. before flood 

group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Baetis rhodani":"Oligochaeta"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <-  data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Baetis rhodani":"Oligochaeta"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Baetis rhodani":"Oligochaeta"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1042 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_1042$filename <-'1042_Baekkelie2017_macroinvertebrate_abundance'

