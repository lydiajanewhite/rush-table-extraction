### extracting data form species abundance tables ###

library(readr)
library(tidyverse)

# 162
data <- read_csv(here::here("data","162_Davis2019_invertebrate_abundance_data.csv"))
head(data)

## change treatment names, and make single treatment by joining stressor variables 
## sediment: 0 = ambient, H = high -> S0 and S1
## nitrogen: 0N = ambient, CN = chronic, AN = acute  -> N0 and N1 and N2
## phosphorus: 0P = ambient, CP = chronic, AP = acute  -> PH0 and PH1 and PH2

data<-data%>%
  mutate(Nitrogen=recode(Nitrogen, '0N' ="N0", CN="N1",AN="N2"))%>%
  mutate(Phosphorus=recode(Phosphorus, '0P' ="PH0", CP="PH1",AP="PH2")) %>% 
  mutate(Sediment =recode(`Sediment Level`, '0' ="S0", H="S1")) %>% 
  unite("group_id", Sediment, Nitrogen, Phosphorus)
 
#  then summarise mean sp abundance across treatments and pivot

group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Siphonoperla torrentium':'Hydraenea'), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Siphonoperla torrentium':'Hydraenea'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Siphonoperla torrentium':'Hydraenea'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_162 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_162$filename <-'162_Davis2019_invertebrate_abundance_data'
## summary<-error_type <-'sd'     can be added  at final stage 
## summary<-plot_type <-'table'

# 179

data <- read_csv(here::here("data","179_Brandt2019_plant_cover_data.csv"))
biomass_data <- read_csv(here::here("data","179_Brandt2019_plant_biomass_data.csv"))

head(data)
unique(data$unit)
unique(data$seed.total) # check using seedless data 
unique(biomass_data$year)  
unique(data$year)
## change treatment names, and make single treatment by joining stressor variables 
## ntrt: CONTROL = ambient, nutrient = enriched, organic_matter = organic -> N0_O0 and N1_O0 and N0_O1
## disturbance: 0 = ambient, 1= physical disturbance,  -> PD0 and PD1
## 
a <- biomass_data %>%  filter (unit == "2" & year == "2006") %>% 
  group_by(taxa) %>% 
  summarise (mean = mean(mass))


b <- data %>%  filter (unit == "2" & year == "2006")%>% 
  group_by(taxa) %>% 
  summarise (mean = mean(cover))

data <- data %>% 
  mutate(nutrient_OM =recode(ntrt, 'CONTROL' ="N0_O0", "#NAME?" ="N1_O0","+C"="N0_O1")) %>% 
  separate(nutrient_OM, c("nutrient", "organic_matter")) %>%
  mutate(disturbance=recode(dist, "0" ="PD0", "1"="PD1")) %>% 
  unite("group_id", nutrient, organic_matter, disturbance) %>% 
  unite("location_id", site, block, plot)

#  then summarise mean sp abundance across treatments and pivot

head(data)
unique(data$group_id)
unique(data$unit)
unique(data$location_id)

data1 <- data %>% 
  select(group_id, unit, year, taxa, cover) %>% 
  group_by(group_id, unit, year, taxa) %>% 
  summarise(cover= mean(cover))

wide <- pivot_wider(data1, names_from = taxa, values_from = cover) 


data <- data %>% 
  select(group_id, unit, year, taxa, cover) 

wider <- data %>% 
  filter(year == "2005") %>% 
  pivot_wider(names_from = taxa, values_from = cover)

# 452
data <- read_csv(here::here("data","452_White2018_invertalgae_abundance_td.csv"))
head(data)
unique(data$Predators)
## change treatment names, and make single treatment by joining stressor variables 
## temperature:  ambient, heated -> T0 and T1
## nutrients: ambient, enriched -> T0 and T1 N0 and N1 
## predators: remove spaces and capitals none, crab, whelk, crab_whelk

data<-data%>%
  mutate(Temperature=recode(Temperature, Ambient ="T0", Heated ="T1"))%>%
  mutate(Nutrient=recode(Nutrient, Ambient ="N0", Enriched="N1")) %>% 
  mutate(Predators =recode(Predators, None = "none", Crab ="crab", Whelk ="whelk", 'Crab & Whelk' ="crabwhelk")) %>% 
  unite("group_id", Temperature, Nutrient, Predators)

#  then summarise mean sp abundance across treatments and pivot

group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('serratus':'caprella sp'), mean)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('serratus':'caprella sp'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('serratus':'caprella sp'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_452<- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_452$filename <- '452_White2018_invertalgae_abundance'


# 529
data <- read_csv(here::here("data","529_Costello2018_periphyton_abundance.csv"))
head(data)
unique(data$TREAT)

## change treatment names, and then make single treatment by joining stressor variables 
## temp  19.28 19.79 20.03 20.21 20.78 20.96 21.48 21.74 22.59 22.97 22.98 23.62-> T0 - T11
## sediment: 0  25  50 100 200 400 800 -> S0 - S6
## nitrogen: 364.750  547.125  729.500  911.875 1094.250 1459.000 1823.750 ->  N0 - N6
## phosphorus: 19  51  82 114 145 208 272 -> PH0 - PH6
## increased salinity: 79.4 158.8 317.6 476.4 635.2 794.0 952.8 -> NA0 - NA6

data%>%
  filter (TREAT != 'Ext') %>%
  mutate(Temperature = recode(TEMP, '19.28' = 'T0', '19.79' = 'T1', '20.03' = 'T2', 
    '20.21' = 'T3', '20.78' = 'T4', '20.96' = 'T5',  '21.48' = 'T6', '21.74' = 'T7', 
    '22.59'= 'T8',  '22.97' = 'T9', '22.98' = 'T10', '23.62' = 'T11'))

# very time consuming

# with vectors?

a <- sort(unique(data$TEMP))
a1<- c( "CL1", "T0", "T1", "T2", "T3", "T4", "T5", "T6" , "T7", "T8", "T9", "T10")
b <- sort(unique(data$TSS))
b1 <- c("S0", "S1", "S2", "S3", "S4", "S5", "S6")
c <- sort(unique(data$NIT))
c1 <- c("N0", "N1", "N2", "N3", "N4", "N5", "N6")
d <- sort(unique(data$PHOS))
e <- sort(unique(data$Cl))

data %>%  
  mutate(temperature = plyr::mapvalues(TEMP, from = a, to = a1),
         sediment = plyr::mapvalues(TSS, from = b, to = b1),
         nutrient = plyr::mapvalues(NIT, from = c, to = c1))


## nico test

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x)))
}

data %>%  
  mutate(temperature = convert_to_categ(TEMP, "T"),
         sediment = convert_to_categ(TSS, "S"),
         nutrient = convert_to_categ(NIT, "N"))

