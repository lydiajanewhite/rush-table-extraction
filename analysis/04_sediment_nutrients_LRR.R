# explore all studies that manipulate sediment and nutrients 

library(tidyverse)
library(here)
library(stringr)
library(ggpubr)
library(ggExtra)
library(cowplot)
library(patchwork)


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

sort(unique(filter(sed_nut, study == '2644')$group_id)) 
filter(sed_nut, study == '2644')
## could create a new variable with the other groupings that are not N and S and then group by study, response and 'other'

sort(unique(sed_nut$study)) # 13 studies 
sort(unique(sed_nut$group_id)) # all with same format treatment ID 

#  x <- sed_nut %>%  
#    filter(str_detect(group_id, "[A-Z]+[1-9]"))  %>% 
#   mutate (control = str_replace_all(group_id, "([A-Z]+)[1-9]", "\\10" ))  # \\1 refers to the first thing in brackets in the previous reg exp followed by zero

x <- sed_nut %>%  
  filter(str_detect(group_id, "[SN]+[1-9]"))  %>% 
  mutate (control = str_replace_all(group_id, "([SN]+)[1-9]", "\\10" ))  # \\1 refers to the first thing in brackets in the previous reg exp followed by zero

filter(x, study == '2644')
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
x <- x %>% filter(!( study == "0" & str_detect(control, "tp[0-6]$"))) # dollar means that no characters come afterwards 

# study2644 <- x %>% filter(study == "2644") %>%  # dollar means that no characters come afterwards 
#   separate(group_id, c("nut", "sed","tp")) %>% 
#   separate(control, c("x", "y", NA)) %>% 
#   unite (control, x, y) %>% 
#   select(-se, -error_type, -sd, -n) %>% 
#   pivot_wider(names_from = tp, values_from = mean) %>% 
#   mutate (mean = (tp1 - tp0)) %>% 
#   select(study, variable, nut, sed, mean, plot_type, filename, control) %>% 
#   mutate (n = 2, sd = 0, se = 0, error_type = NA) %>% 
#   unite (group_id, nut, sed) 
# 
# x <- rbind((filter(x, study != "2644")), study2644)

sort(unique(x$study))

# something going wrong here - we lose study 809 because no N0_S0_V0, can only analyse in presence of V1

tmp <- inner_join(x, sed_nut, by = c("control" = "group_id", "study", "variable"), suffix = c("", "_control")) %>% 
  select(-filename_control, -plot_type_control) %>% 
  mutate (LRR = log(mean/mean_control))%>% 
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
ggplot(summary_sednut, aes( x = LRR_N, y=LRR_S)) + 
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

ggplot(absence, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = stressor_level ), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(color = stressor_level), method = "pearson")

rush_plot <- ggplot(rush, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = consumer), alpha = 0.6) +
  geom_smooth(method = "lm", aes(colour = consumer), se = TRUE)  +
  facet_wrap(~tp, scales = "free")  + 
  stat_cor(aes(color = consumer), method = "pearson") +
  theme_bw()

rush_plot_pooled <- ggplot(rush, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(), alpha = 0.6) +
  geom_smooth(method = "lm", aes(), se = TRUE)  +
  facet_wrap(~tp, scales = "free")  + 
  stat_cor(aes(), method = "pearson") +
  theme_bw()

rest_plot <- ggplot(rest, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = control), alpha = 0.6) +
  geom_smooth(method = "lm", aes(colour = control), se = TRUE)  +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(), method = "pearson") +
  theme_bw()

study809_plot <- ggplot(study809, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = control), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(), method = "pearson")

ggplot(study809, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = tp), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(colour = tp), method = "pearson")

ggplot(study809, aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = habitat), alpha = 0.6) +
  facet_wrap(~study, scales = "free")  + 
  stat_cor(aes(colour = habitat), method = "pearson")


p <- ggplot((filter(rest, study == '529')), aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = stressor_level), alpha = 0.6) +
  stat_cor(aes(colour = stressor_level), method = "pearson") +
  geom_smooth(method = "lm", aes(colour = stressor_level), se = F)

ggExtra::ggMarginal(p, type = "histogram")

ggExtra::ggMarginal(rush_plot, type = "histogram")

plot1 <- ggplot((filter(rest, study == '529')), aes( x = LRR_N, y=LRR_S)) + 
  geom_point(aes(colour = stressor_level), alpha = 0.6) +
  stat_cor(aes(colour = stressor_level), method = "pearson") +
  geom_smooth(method = "lm", aes(colour = stressor_level), se = F) +
  theme_pubr() +
  theme(legend.position = c(0.8, 0.8)) +
  geom_rug(aes(colour = stressor_level)) 

dens1 <- ggplot((filter(rest, study == '529')), aes(x = LRR_N, fill = stressor_level)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none")

dens2 <- ggplot((filter(rest, study == '529')), aes(x = LRR_S, fill = stressor_level)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

dens1 + plot_spacer() + plot1 + dens2 + plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
