library(readr)
library(tidyverse)

# 543
data <- read_csv(here::here("data","543_VandePerre2018_phytoplankton_abundance.csv"))
data2<- read_csv(here::here("data","543_VandePerre2018_protozoa_abundance.csv"))
data3<- read_csv(here::here("data","543_VandePerre2018_zooplankton_abundance.csv"))

data<- inner_join(data, data2) %>% 
  inner_join(., data2)

head(data)
unique(data$'Day after start treatment')
unique(data$`Temperature treatment level (Degrees celsius)`)
unique(data$`P treatment level (micro gram P/L)`)
## change treatment names, and then make single treatment by joining stressor variables 
## time -1  7 14 21 28 35 -> T0 to T5
## zinc  0  75 300 -> ZN0 - ZN2
## phosphorus 10 200 -> PH0 - PH1
## temp "16-19" "21-24" -> T0 - T1 

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-1)
}

data <- data %>%  
  mutate(temperature = convert_to_categ(`Temperature treatment level (Degrees celsius)`, "T"),
         phosphorus = convert_to_categ(`P treatment level (micro gram P/L)`, "PH"),
         zinc  = convert_to_categ( `Zn treatment level (micro gram Zn/L)`, "ZN"), 
         time  = convert_to_categ( `Day after start treatment`, "TP")) %>% 
  unite("group_id", temperature, phosphorus, zinc, time) 

unique(data$group_id)
data[, 117]
#  then summarise mean sp abundance across treatments and pivot

group_mean <-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Diatom colony':'Salpingoeca frequentissima'), mean)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Diatom colony':'Salpingoeca frequentissima'),sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n<-data %>% 
  group_by (group_id) %>% 
  summarise_at(vars('Diatom colony':'Salpingoeca frequentissima'), length) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_543<- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)

summary_543$filename <- '543_VandePerre2018_plankton_abundance'




