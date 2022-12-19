library(tidyverse)

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

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-1)
}

convert_to_categ2 <- function(x, y){ #just for temperature 
  paste0(y, as.numeric(factor(x))-2)
}

data <- data %>%  
  mutate(temperature = convert_to_categ2(TEMP, "T"),
         sediment = convert_to_categ(TSS, "S"),
         nutrient = convert_to_categ(NIT, "N"),
         phosphorus = convert_to_categ(PHOS, "PH"),
         salinity = convert_to_categ(Cl, "NA")) %>% 
  mutate(temperature = recode(temperature, 'T-1' ="CL1")) %>% 
  unite("group_id", temperature, sediment, nutrient, phosphorus, salinity) 


#  then summarise mean sp abundance across treatments and pivot
data$Achnanthes
group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Achnanthes':'Mougeotia'), mean)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Achnanthes':'Mougeotia'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Achnanthes':'Mougeotia'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_529<- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_529$filename <- '529_Costello2018_periphyton_abundance'

saveRDS(summary_529, file = "output/individual_datasets/summary_529.rds") 
