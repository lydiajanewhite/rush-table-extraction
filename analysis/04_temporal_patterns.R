# explore all studies that have multiple timepoints 
library(tidyverse)
library(here)
library(stringr)
library(ggpubr)
library(ggExtra)
library(cowplot)
library(patchwork)


library(EnvStats)
big_table_checked <- read_rds(here::here("output","completetable.rds"))

# detect any studies that have time points - tp 

temporal <- big_table_checked %>%
  mutate(group_id = str_replace(group_id, "TP", "tp")) %>% 
  filter(str_detect(group_id, "tp")) 

## could create a new variable with the other groupings that are not N and S and then group by study, response and 'other'

sort(unique(temporal$study)) # 13 studies 
sort(unique(temporal$group_id)) # all with same format treatment ID 

sort(unique(filter(temporal, study == '2644')$group_id)) 

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

# need to filter relevant time points for study 0 and 809 and others? # and if for negatives? [-6-0] ?
x <- x %>% filter(!( study == "809" & str_detect(control, "tp[1-4]"))) # looks for tp5, tp6, tp7  and tp8
x <- x %>% filter(!( study == "0" & str_detect(control, "tp[0-6]$")))
x <- x %>% filter(!( study == "2644" & str_detect(control, "tp0")))# dollar means that no characters come afterwards 
# x %>% filter( study == "0" & str_detect(control, "tp[7-9]|tp1[0-5]"))
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

#### in presence and absence of other stressors, multiple time points and other 
ggplot(summary_sednut, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = stressor_level ), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(color = stressor_level), method = "pearson") 

### now plot in absence of other stressors 
absence <- summary_sednut %>% filter(! str_detect(control, "[A-Z]+[1-9]"))
rush <- absence %>%  filter(study == 0)
rest <- absence %>%  filter(study != 0)
study809 <- summary_sednut %>% filter (study == 809) %>% 
  separate(control, c(NA, NA, NA, "habitat","tp"), remove = F)

rush <- rush %>%  separate(control, c(NA, NA, "grazer", "predator", "tp"), remove = F) %>% 
  unite (consumer, grazer, predator) %>% 
  mutate(across(tp, factor, levels=c("tp7","tp8","tp9", "tp10","tp11","tp12", "tp13","tp14","tp15"))) 


ggplot(absence, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = stressor_level ), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(color = stressor_level), method = "pearson")

rush_plot <- ggplot(rush, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = consumer), alpha = 0.6) +
  geom_smooth(method = "lm", aes(colour = consumer), se = TRUE)  +
  facet_wrap(~tp, scales = "free")  + 
  stat_cor(aes(color = consumer), method = "pearson") +
  theme_bw()

rush_plot_pooled <- ggplot(rush, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(), alpha = 0.6) +
  geom_smooth(method = "lm", aes(), se = TRUE)  +
  facet_wrap(~tp, scales = "free")  + 
  stat_cor(aes(), method = "pearson") +
  theme_bw()

rest_plot <- ggplot(rest, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = control), alpha = 0.6) +
  geom_smooth(method = "lm", aes(colour = control), se = TRUE)  +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(), method = "pearson") +
  theme_bw()

study809_plot <- ggplot(study809, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = control), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(), method = "pearson")

ggplot(study809, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = tp), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(colour = tp), method = "pearson")

ggplot(study809, aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = habitat), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(colour = habitat), method = "pearson")


p <- ggplot((filter(rest, study == '529')), aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = stressor_level), alpha = 0.6) +
  stat_cor(aes(colour = stressor_level), method = "pearson") +
  geom_smooth(method = "lm", aes(colour = stressor_level), se = F)

ggExtra::ggMarginal(p, type = "histogram")

ggExtra::ggMarginal(rush_plot, type = "histogram")

plot1 <- ggplot((filter(rest, study == '529')), aes( x = hedges_g_N, y=hedges_g_S)) + 
  geom_point(aes(colour = stressor_level), alpha = 0.6) +
  stat_cor(aes(colour = stressor_level), method = "pearson") +
  geom_smooth(method = "lm", aes(colour = stressor_level), se = F) +
  theme_pubr() +
  theme(legend.position = c(0.8, 0.8)) +
  geom_rug(aes(colour = stressor_level)) 

dens1 <- ggplot((filter(rest, study == '529')), aes(x = hedges_g_N, fill = stressor_level)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none")

dens2 <- ggplot((filter(rest, study == '529')), aes(x = hedges_g_S, fill = stressor_level)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

dens1 + plot_spacer() + plot1 + dens2 + plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
