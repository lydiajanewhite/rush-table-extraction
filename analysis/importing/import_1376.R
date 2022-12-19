library(tidyverse)

# 1376
data <- read_csv(here::here("data","1376_Steven2015_bacteria_abundance.csv"))
head(data)

## change treatment names, already single treatment with many variables 
## filter relevant site -> DOEII
## convert to relevant treatment names 
## warming and precipitation -> T0 & T1 and DR0 (precipitation) & DR1 (no precipitation)

data <- data %>%  
  separate(Sample, into = c("site", "treatment"), sep = "_", extra = "merge") %>%  
  filter(site == 'DOEII') %>% 
  separate(treatment, into = c("treata", "treatb"), sep = "_") %>% 
  unite("group_id", treata, treatb, remove = F)%>% 
  mutate(group_id = recode(group_id, 'Control_plot' = "T0_DR1", 'Precipitation_plot' = "T0_DR0",
                           'Warm_plot' = "T1_DR1", 'Warm_and' = "T1_DR0")) 
group_mean <- data %>% 
    group_by (group_id) %>% 
    summarise_at(vars("Acidobacteria":"Cyanobacteria"), mean) %>% 
    pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <-  data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Acidobacteria":"Cyanobacteria"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Acidobacteria":"Cyanobacteria"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1376 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_1376$filename <-'1376_Steven2015_bacteria_abundance'

saveRDS(summary_1376, file = "output/individual_datasets/summary_1376.rds") 
