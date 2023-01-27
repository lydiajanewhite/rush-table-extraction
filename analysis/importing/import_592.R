library(tidyverse)

# 592
data <- read_csv(here::here("data","592_Beermann2018_chironomidae_abundance.csv"))

head(data)
unique(data$treatment)

## change treatment names, and then make single treatment by joining stressor variables 

# C	Control
#	L	Reduced water flow -> V
#	N	Increased chloride concentration -> NA
#	S	Increased fine sediment -> S
#	LN	Reduced water flow + increased chloride concentration
#	NS	Increased chloride concentration + increased fine sediment
#	LS	Reduced water flow + increased fine sediment
#	LNS	Reduced water flow + increased chloride concentration + increased fine sediment

data <- data %>% 
  mutate(treatment = recode(treatment, 'C' ="S0_V0_NA0", 
                            'S' ="S1_V0_NA0", 'N' ="S0_V0_NA1",  'L' ="S0_V1_NA0", 
                            'LN' ="S0_V1_NA1", 'NS' ="S1_V0_NA1",  'LS' ="S1_V1_NA0", 
                            'LNS' ="S1_V1_NA1")) %>% 
  mutate(habitat = str_replace(Sample, "^C[0-9]+$", "hab_1")) %>% 
  mutate(habitat = str_replace(habitat, "^L[0-9]+$", "hab_2")) %>% 
  unite("group_id", treatment, habitat, remove = F) 

#  then summarise mean sp abundance across treatments and pivot

group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('OTU_1':'OTU_165'), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('OTU_1':'OTU_165'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('OTU_1':'OTU_165'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_592 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_592$filename <-'592_Beermann2018_chironomidae_abundance'


saveRDS(summary_592, file = "output/individual_datasets/summary_592.rds") 



