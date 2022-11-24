library(tidyverse)

# 1486
data <- read_csv(here::here("data","1486_Mrowicki2015_macroalgal_biomass.csv"))
head(data)

## change treatment names
## temp T0 and T+ -> T0 and T1
## wave W0 and W+ -> ST0 and ST1 (storm intensity) 
## grazers presence and absence -> pred0 and pred1 (some single vs multi sp treats) 

data <- data %>% 
  mutate(warming = recode(Temp, 'T0' ="T0", 'T+' ="T1"),
         storm_intensity = recode (Wave, 'W0' ="W0", 'W+' ="W1"),
         grazer = recode(Graz, 'P' = "Graz1", 'L' = "Graz1", 'G' = "Graz1", 'PLG' = "Graz1", 'n/a' = "Graz0")) %>% 
  unite("group_id", warming, storm_intensity, grazer, remove = F) 
  
## for each sp column there is pre and post biomass - before and after experiment 
##  sp name variables annotated with .pre and .post
## i want to add a new variable of time, with two categories for pre and post -> T0 and T1
## leaving a single variable for each species 