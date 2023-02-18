# rush data 
# TPs start at 0 but stressors don't start until month 6 and then month 12 is used to calculate resistance 

library(tidyverse)
library(here)
library(stringr)

big_table_checked <- read_rds(here::here("output","completetable.rds"))

sed_nut <- big_table_checked %>%
  filter(str_detect(group_id, "[SN][0-9]_.*[SN][0-9]",)) 

rush <- sed_nut %>% filter (study == 0 ) %>% 
  separate (group_id, c( NA, NA, "pred", "graz", "time"), remove = F) %>% 
  unite ("consumer", pred, graz, remove = F) %>% 
  filter(time == "TP12")

control_metric <- rush  %>% filter(str_detect(group_id, "^[SN][0]_*[SN][0]",)) %>% 
  group_by(variable, group_id)%>%
  summarise(control_mean = mean, control_sd = sd, control_n = n, filename = filename, study = study)

# # # # # # # # # # # # left join not working!!!!!! # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

contributions <- left_join(rush, control_metric) %>% 
  mutate(s =  ((n-1) * sd + (control_n -1) * control_sd ) / (n + control_n - 2), 
         j = 1 - (3 / (4 * (n + control_n - 2)-1)))

contributions_stressor1 <- contributions %>% filter(str_detect(group_id, "^[SN][1-9]_*[SN][0]",)) %>% 
  separate (group_id, c("group_id", NA, NA, NA, NA)) %>% 
  separate (group_id,c("stressor_1_type", "stressor_1_level"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate (hedges_g = ((mean - control_mean) / s) * j) %>% 
  mutate (stressor_level = stressor_1_level) %>% 
  rename (x = hedges_g)

contributions_stressor2 <- contributions %>% filter(str_detect(group_id, "^[SN][0]_*[SN][1-9]",)) %>% 
  separate (group_id, c( NA, "group_id"), remove = F) %>% 
  separate (group_id,c("stressor_2_type", "stressor_2_level"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate (hedges_g = ((mean - control_mean) / s) * j) %>% 
  mutate (stressor_level = stressor_2_level) %>% 
  rename (y = hedges_g)

test <- left_join(contributions_stressor1 %>% select (study, variable, stressor_level, x) , contributions_stressor2%>% select (study, variable, stressor_level, y))

ggplot(test, aes( x=x, y=y)) + 
  geom_point(aes(colour = variable, shape = stressor_level)) 