library(tidyverse)
library(here)

figure_data <- read_csv(here::here("data","digitised_data_summaryplot.csv")) %>% 
  select(-r)

summary_table_data <- read_csv(here::here("data","digitised_data_tables.csv"), na = c("na", "NA", "")) %>% 
  mutate(plot_type = "table_summary")

table_data <- list.files("output/individual_datasets", full.names = TRUE) %>% 
  purrr::map(readRDS) %>% 
  bind_rows() %>% 
  mutate(error_type = "sd", plot_type = "table_raw", se = NaN)

big_table <- bind_rows(figure_data, summary_table_data, table_data) %>% 
  separate(filename, into = c("study",NA), remove = F, extra = "drop")

# write_tsv(big_table, file = "output/big_table.tsv") 

test <- big_table %>% 
  filter(is.na(sd)) %>% 
  filter (n > 1) %>% 
  filter(is.na(se)) %>% 
  filter (mean != "0")


unique(big_table$plot_type)

## scatterplots need some formatting 
scatter_data <- read_csv(here::here("data","digitised_data_scatter.csv"))

scatter_data <- scatter_data %>% 
  mutate( plot_type = "scatter")%>% 
  separate(.id, into = c("study",NA), remove = F, extra = "drop") %>% 
  rename (filename = .id, variable = id) 


unique((scatter_data %>% filter(study == "2877" & y_variable == "annelid abundance"))$filename)

study2877 <- scatter_data %>% filter(study == "2877") %>% 
  rename (group_id = variable)

convert_to_categ <- function(x, y){
  paste0(y, as.numeric(factor(x))-2)
}

study2877  <- study2877  %>% 
  rename (variable = y_variable, value = y) %>% 
  group_by(variable, group_id) %>% 
  mutate(timepoint = convert_to_categ(x, "TP")) %>% 
  unite("group_id", group_id, timepoint) %>% 
  mutate(variable = str_replace(variable, "  ", " ")) %>% 
  mutate(variable = str_replace(variable, "chaeborus abundance", "chaoborus abundance")) %>% 
  mutate(error_type = NaN, se = NaN, sd = NaN, n = 2) %>% 
  rename (weeks_post_stressor = x) %>% 
  select ( -col, - group, -pch, -x_variable) # some responses measured at more time points 

head(study2877)
unique(study2877$group_id)

study1970<- scatter_data %>% filter(study == "1970") 

