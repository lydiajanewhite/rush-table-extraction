library(readr)
library(tidyverse)

# 179
data <- read_csv(here::here("data","seed-add-data-output-full-plant-cover-noseed.csv"))
head(data)
unique(data$unit)
unique(data$seed.total) # check using seedless data 

## change treatment names, and make single treatment by joining stressor variables 
## ntrt: CONTROL = ambient, nutrient = enriched, organic_matter = organic -> N0_O0 and N1_O0 and N0_O1
## disturbance is complicated because 3 different years of disturbance, could exclude B and C 
# which excludes 3rd of the data but means all data is available for entirety of the series so can 
# so can calculate temp var etc. 0 = ambient, 1= physical disturbance,  -> PD0 and PD1

data$dist.year[data$subplot=="A"]<-2003
data$dist.year[data$subplot=="B"]<-2004
data$dist.year[data$subplot=="C"]<-2005

data <- data %>% filter(dist.year == 2003)

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-1)
}

data <- data %>% 
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1"),
         site = recode (site, "HASTINGS" = "siteamb", "MCLAUGHLIN" = "sitewet", "SEDGEWICK" = "sitedry"),
         nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "+N" ="N1_O0","+C"="N0_O1"),
         time  = convert_to_categ(year, "TP")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  unite("group_id", nutrient, organic_matter, disturbance, site, time, remove = F) %>% 
  unite("location_id", site, block, plot, subplot, remove = F)


data <- data %>% 
  select(site, block, group_id, unit, subplot, year, taxa, cover, ntrt, dist) 

unique(data$subplot)

wide<- data %>% 
  pivot_wider(names_from = taxa, values_from = cover)

group_mean <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("LOTUS STRIGOSUS":"AMSINCKIA MENZIESII"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("LOTUS STRIGOSUS":"AMSINCKIA MENZIESII"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("LOTUS STRIGOSUS":"AMSINCKIA MENZIESII"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

# could aggregate by site, but they differ in drought characteristics 
# sitedry = Hastings = dryest, sitewet = MCLAUGHLIN = wettest, siteamb = SEDEWICK = medium 
