# explore all studies that manipulate sediment and nutrients 

library(tidyverse)
library(here)
library(stringr)
library(ggpubr)

library(EnvStats)
big_table_checked <- read_rds(here::here("output","completetable.rds"))

# detect any studies that have nutrients N and sediment S 
# N0 = ambient nutrients, N1 = added nutrients 
# S0 - ambient sediment, S1 = added sediment 
# some studies have additional third or fourth stressors
# some studies have measurements from multiple time points indicated by TP0, TP1, TP2.... 
# some TP are negative TP-1, TP-2,... 
# some studies have presence of consumers manipulated indicated by graz0 pred0

sed_nut <- big_table_checked %>%
  filter(str_detect(group_id, "[SN][0-9]_.*[SN][0-9]"))  %>% 
  mutate(group_id = str_replace(group_id, "TP", "tp")) %>% 
  mutate(sd = ifelse(study == 107, se*sqrt(n), sd)) # need to calculate sd for study 107 
 
## could create a new variable with the other groupings that are not N and S and then group by study, response and 'other'
  
sort(unique(sed_nut$study)) # 13 studies 
sort(unique(sed_nut$group_id)) # all with same format treatment ID 

#  x <- sed_nut %>%  
#    filter(str_detect(group_id, "[A-Z]+[1-9]"))  %>% 
#   mutate (control = str_replace_all(group_id, "([A-Z]+)[1-9]", "\\10" ))  # \\1 refers to the first thing in brackets in the previous reg exp followed by zero

x <- sed_nut %>%  
  filter(str_detect(group_id, "[SN]+[1-9]"))  %>% 
  mutate (control = str_replace_all(group_id, "([SN]+)[1-9]", "\\10" ))  # \\1 refers to the first thing in brackets in the previous reg exp followed by zero

### need to edit this with help from Nico 
### for N0_S1_V0 -> control should be N0_S0_V0 
### but for N0_S1_V1 -> control should be N0_S0_V1
### but for N0_S1_V1 -> control should be N0_S0_V1

### only important bit is N0 and S0, controls should differ for different levels of additional stressors same goes with habitats ans grezers 
### other option is to exlcude other stressors 

sort(unique(x$study))
# x <- filter(x,!is.na(sd))  
unique(x$group_id)

# need to filter relevant time points for study 0 and 809 and others? 
#x <- x %>% filter( study == "0" & str_detect(control, "[tp]+[7-15]")) # and if for negatives? [-6-0] ?
#x <- x %>% filter( study == "809" & str_detect(control, "[tp]+[5-8]"))

# something going wrong here - we lose study 809 because no N0_S0_V0, can only analyse in presence of V1

tmp <- inner_join(x, sed_nut, by = c("control" = "group_id", "study", "variable"), suffix = c("", "_control")) %>% 
  select(-filename_control, -plot_type_control) %>% 
  mutate(s =  ((n-1) * sd + (n_control -1) * sd_control ) / (n + n_control - 2), 
         j = 1 - (3 / (4 * (n + n_control - 2)-1))) %>% 
  mutate (hedges_g = ((mean - mean_control) / s) * j) %>% 
  filter(!str_detect(group_id, "[SN][1-9]_.*[SN][1-9]"))

sort(unique(tmp$study))

stressor_1 <- tmp  %>% filter(str_detect(group_id, "N[1-9]")) %>% 
  mutate (stressor = str_extract(group_id, "N(\\d)")) %>% 
  separate (stressor, c("stressor_type", "stressor_level"), sep = "(?<=[A-Za-z])(?=[0-9])") 

stressor_2 <- tmp  %>% filter(str_detect(group_id, "S[1-9]")) %>% 
  mutate (stressor = str_extract(group_id, "S(\\d)")) %>% 
  separate (stressor, c("stressor_type", "stressor_level"), sep = "(?<=[A-Za-z])(?=[0-9])") 

summary_sednut <- inner_join(stressor_1,stressor_2, by = c("study", "variable", "control", "stressor_level"), 
           suffix = c("_N", "_S")) 

ggplot(summary_sednut, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = stressor_level ), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(color = stressor_level), method = "pearson") 

### could also plot in absence of other stressors 
absence <- summary_sednut %>% filter(! str_detect(control, "[A-Z]+[1-9]"))

ggplot(absence, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = stressor_level ), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(color = stressor_level), method = "pearson")

ggplot(absence, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = control ), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(color = control), method = "pearson")

