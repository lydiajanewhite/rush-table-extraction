library(tidyverse)

# 809
data <- read_csv(here::here("data","809_graeber2017_macroinvertebrate_abundance.csv"))
head(data)
tail(data)

unique(data$Phase)
unique(data$Date)
data <- data %>% 
  filter(!is.na (Treatment)) 

## change treatment names, already single treatment 
## Nutrients np -> N0 & N1
## reduced water velocity:  nf & lf -> V0 & V1
## sedimentation: fs ->  S0 & S1
## timepoint: date goes to TP 7 


# need to convert date to recognizable date beforoe convert to cetegorical var

data$date <- as.Date(data$Date, "%d/%m/%Y")


convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-1)
}

data <- data %>% 
  mutate(treatment =recode(Treatment, 'nf' ="N0_S0_V0", 
                           'lf' ="N0_S0_V1",'nf.np' ="N1_S0_V0",
                           'lf.np' ="N1_S0_V1", 'lf.fs' ="N0_S1_V1", 
                           'lf.np.fs' ="N1_S1_V1"),
         habitat = recode(Habitat, "RUN" = "habitat1", "RIF" ="habitat2"),
         time = convert_to_categ (date, "TP")) %>% 
  unite("group_id", treatment, habitat, time, remove = F) 

unique(data$group_id)
unique(data$time)


data <- data %>% 
  filter(time == "TP5" | time == "TP6" | time == "TP7" | time == "TP8") %>% 
  select(Taxon, group_id, Abundance, Channel) 

wide<- data %>% 
  pivot_wider(names_from = Taxon, values_from = Abundance)
head(wide)

## need to summarise at channel level and then use date from TP4 onwards 

group_mean <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Asellus.aquaticus":"Rhyacophila.fasciata"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Asellus.aquaticus":"Rhyacophila.fasciata"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Asellus.aquaticus":"Rhyacophila.fasciata"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_809 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_809$filename <-'809_graeber2017_macroinvertebrate_abundance'

saveRDS(summary_809, file = "output/individual_datasets/summary_809.rds") 

