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

# list columns describing samples
sample_var <- grep("\\.pre$|\\.post$",colnames(data),invert = TRUE, value = TRUE)

# list pre experiment measurements
species_var_pre <- grep("\\.pre$",colnames(data), value = TRUE)

# list post experiment measurement
species_var_post <- grep("\\.post$",colnames(data), value = TRUE)

# create a table with sample desc and pre experiment var
pre_table <- select(data,all_of(c(sample_var, species_var_pre))) |>
  rename_with(~ gsub("\\.pre$", "", .x)) |>
  mutate(time = "pre")

# create a table with sample desc and post experiment var
post_table <- select(data,all_of(c(sample_var, species_var_post))) |>
  rename_with(~ gsub("\\.post$", "", .x)) |>
  mutate(time = "post")

# put the tables together
long_data <- bind_rows(pre_table, post_table)

long_data

data <- long_data %>% 
  mutate(time = recode(time, 'pre' ="TP0", 'post' ="TP1")) %>% 
  unite("group_id", group_id, time, remove = F) 

group_mean <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Dic_bm_wet":"Ulv_bm_wet"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")

group_sd <-  data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Dic_bm_wet":"Ulv_bm_wet"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n <- data %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Dic_bm_wet":"Ulv_bm_wet"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1486 <- inner_join(group_mean,group_sd) %>% 
  inner_join(., group_n)
## need micro data..... 
summary_1211$filename <-'1211_Mayer-Pinto2016_invertebrate_abundance'