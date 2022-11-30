library(tidyverse)

# 1334
data <- read_csv(here::here("data","1334_Sorte2015_rockpool_abundance.csv"))
unique(data$Treatment)
unique(data$Time)
head(data)

## change treatment names, already single treatment 
## T0 & T1  warming 
## A0 & A1 acidifcation 

data <- data %>% 
  mutate(treatment = recode(Treatment, 'CO2' ="T0_A1", 'Control' ="T0_A0",
                            'Temp' ="T1_A0",  'Both' ="T1_A1"), 
         time = recode(Time, 'beggining' = "T0", 'end' = "T1")) %>% 
  unite("group_id", treatment, time, remove = F)

group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Anthopleura art.":"Amphipod"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <-  data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Anthopleura art.":"Amphipod"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Anthopleura art.":"Amphipod"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1334 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_1334$filename <-'1334_Sorte2015_rockpool_abundance'