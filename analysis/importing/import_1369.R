library(tidyverse)

# 1369
data <- read_csv(here::here("data","1369_McElroy_algae-amphipod_abundance.csv"))
unique(data$Treatment)
unique(data$Time)
head(data)

## change treatment names, already single treatment 
## ambient & heated -> T0 & T1  warming 
## 0 & 1-> N0 & N1 nutrients 
## pred presence -> pred0 and pred1
## pred size -> not categorical, continuous range of sizes, needs binning 

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-1)
}

data <- data %>% 
  mutate(nutrients = convert_to_categ(nutrients, "N"), 
         predators = convert_to_categ(`crab size (g wet mass)`, "pred"), 
         predator = recode(crab_treat, 'absent' ="predO", 'present' ="pred1"),
         temperature = recode(temp_treat, 'ambient' ="TO", 'heated' ="T1")) %>% 
  unite("group_id", nutrients, temperature, predator, remove = F)

group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("small amphipods (abundance)":"U. lactuca (g dry mass)"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <-  data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("small amphipods (abundance)":"U. lactuca (g dry mass)"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("small amphipods (abundance)":"U. lactuca (g dry mass)"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1369 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_1369$filename <-'1369_McElroy_algae-amphipod_abundance'