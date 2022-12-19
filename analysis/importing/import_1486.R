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

summary_1486$filename <-'1486_Mrowicki2015_macroalgal_biomass'

## add micro data 

data2 <- read_csv(here::here("data","1486_Mrowicki2015_microalgal_biomass.csv"))

## change treatment names as above
## filter out intermediate time points, keep first and final sampling only, to 
## align with macroalgae data 

data2 <- data2 %>% 
  mutate(warming = recode(Temp, 'T0' ="T0", 'T+' ="T1"),
         storm_intensity = recode (Wave, 'W0' ="W0", 'W+' ="W1"),
         grazer = recode(Graz, 'P' = "Graz1", 'L' = "Graz1", 'G' = "Graz1", 'PLG' = "Graz1", 'n/a' = "Graz0")) %>%
  filter(Date == '2013-02-01' | Date == '2013-03-11') %>% 
  mutate(Date = as.character(Date)) %>% 
  mutate(time = recode(Date, "2013-02-01" = "TP0", "2013-03-11" = "TP1")) %>%  
  unite("group_id", warming, storm_intensity, grazer, time, remove = F) 

## calculate mean of technical replicate, ie of 3 benthotorch readings 

data2<- data2 %>% 
  group_by (Tank, group_id) %>% 
  summarise_at(vars("Conc_cyano_ug.cm":"Conc_diatm_ug.cm"), mean)%>% 
  pivot_longer(cols = starts_with("Conc"), names_to = "variable", values_to = "mean")

wide<- data2 %>% 
  pivot_wider(names_from = variable, values_from = mean)
head(wide)

group_mean_2 <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Conc_cyano_ug.cm":"Conc_diatm_ug.cm"), mean) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "mean")
  
group_sd_2 <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Conc_cyano_ug.cm":"Conc_diatm_ug.cm"), sd) %>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "sd")

group_n_2 <- wide %>% 
  group_by (group_id) %>% 
  summarise_at(vars("Conc_cyano_ug.cm":"Conc_diatm_ug.cm"), length)%>% 
  pivot_longer(!group_id, names_to = "variable", values_to = "n")

summary_1486_2 <- inner_join(group_mean_2,group_sd_2) %>% 
  inner_join(., group_n_2)

summary_1486_2$filename <-'1486_Mrowicki2015_microalgal_biomass'

summary_1486<-rbind (summary_1486, summary_1486_2)

saveRDS(summary_1486, file = "output/individual_datasets/summary_1486.rds") 