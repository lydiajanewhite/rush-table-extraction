# explore all studies that manipulate sediment and nutrients 

library(tidyverse)
library(here)
library(stringr)

big_table_checked <- read_rds(here::here("output","completetable.rds"))
  
# detect any studies that have nutrients N and sediment S 
# N0 = ambient nutrients, N1 = added nutrients 
# S0 - ambient sediment, S1 = added sediment 
# some studies have additional third or fourth stressors
# some studies have measurements from multiple time points indicated by TP0, TP1, TP2.... 
# some TP are negative TP-1, TP-2,... 
# some studies have presence of consumers manipulated indicated by graz0 pred0

sed_nut <- big_table_checked %>%
  filter(str_detect(group_id, "[SN][0-9]_.*[SN][0-9]",))  

sort(unique(sed_nut$study)) # 13 studies 
sort(unique(sed_nut$group_id)) # all with same format treatment ID 

# first want to detect studies that manipulate ONLY sediment and nutrients 

dual_stressor <- sed_nut %>%
  filter(str_detect(group_id, "^[SN][0-9]_*[SN][0-9]$",)) 

control_metric <- dual_stressor %>% filter(str_detect(group_id, "^[SN][0]_*[SN][0]$",)) %>%  
  group_by(variable)%>%
  summarise(control_mean = mean, control_sd = sd, control_n = n, filename = filename, study = study)

contributions <- left_join(dual_stressor, control_metric) %>% 
  mutate(s =  ((n-1) * sd + (control_n -1) * control_sd ) / (n + control_n - 2), 
         j = 1 - (3 / (4 * (n + control_n - 2)-1)))

contributions_stressor1 <- contributions %>% filter(str_detect(group_id, "^[SN][1-9]_*[SN][0]$",)) %>% 
  separate (group_id, c("group_id", NA), remove = F) %>% 
  separate (group_id,c("stressor_1_type", "stressor_1_level"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate (hedges_g = ((mean - control_mean) / s) * j) %>% 
  mutate (stressor_level = stressor_1_level) %>% 
  rename (x = hedges_g)

contributions_stressor2 <- contributions %>% filter(str_detect(group_id, "^[SN][0]_*[SN][1-9]$",)) %>% 
  separate (group_id, c( NA, "group_id"), remove = F) %>% 
  separate (group_id,c("stressor_2_type", "stressor_2_level"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate (hedges_g = ((mean - control_mean) / s) * j) %>% 
  mutate (stressor_level = stressor_2_level) %>% 
  rename (y = hedges_g)

test <- left_join(contributions_stressor1 %>% select (study, variable, stressor_level, x) , contributions_stressor2%>% select (study, variable, stressor_level, y))

ggplot(test, aes( x=x, y=y)) + 
  geom_point(aes(colour = variable, shape = stressor_level)) 

dual_stressor <- sed_nut %>%
  filter(str_detect(group_id, "^[SN][0-9]_*[SN][0-9]$",)) 

# also need to be able to detect 
# "N1_S1_graz0_pred0_TP-1" 
# "N0_S0_TP1"  

test2 <- sed_nut %>%
  filter(str_detect(group_id, "^[SN][0-9]_*[SN][0-9]_*[SN]")) 


