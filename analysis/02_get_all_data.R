library(tidyverse)
library(here)
library(stringr)

# boxplot and mean-error figure data plus data from tables all in similar format and will be analysed in the same way
# FYI i should add my own study here.... 

figure_data <- read_csv(here::here("data","digitised_data_summaryplot.csv")) %>% 
  select(-r) %>% 
  mutate(filename = str_replace(filename, "233_Brustolin2019", "221_Brustolin2019")) 

summary_table_data <- read_csv(here::here("data","digitised_data_tables.csv"), na = c("na", "NA", "")) %>% 
  mutate(plot_type = "table_summary")

table_data <- list.files("output/individual_datasets", full.names = TRUE) %>% 
  purrr::map(readRDS) %>% 
  bind_rows() %>% 
  mutate(error_type = "sd", plot_type = "table_raw", se = NaN)

big_table <- bind_rows(figure_data, summary_table_data, table_data) %>% 
  separate(filename, into = c("study",NA), remove = F, extra = "drop")

tail(sort(unique(big_table$group_id)))
# V1_S2_N1`  

# correct group ID label in study 

# "CL1_S0_N0_PH0_NA0" ->  "T1_S0_N0_PH0_NA0"

big_table <- big_table %>% 
  mutate(group_id = str_replace(group_id, "CL1_S0_N0_PH0_NA0", "T1_S0_N0_PH0_NA0")) %>% 
  mutate(study = as.numeric(study))

# examples where there is no error - all NAs - checked publication and no error given 
test <- big_table %>% 
  filter(is.na(sd)) %>% 
  filter(is.na(se)) %>% 
  filter (n > 1) %>% 
  filter (mean != "0")

# any cases where they are zeros and not NAs?
big_table %>% 
  filter(mean == "0") %>% 
  filter(sd == "0" & se == "0") 

big_table %>% 
  filter (sd == "0" & se == "0") 

big_table %>% 
  filter(is.na(se)) %>% 
  filter (sd == "0") 

big_table %>% 
  filter(is.na(sd)) %>% 
  filter (se == "0") %>% 
  filter (mean != "0")

head(test)
## scatterplots need some formatting 
scatter_data <- read_csv(here::here("data","digitised_data_scatter.csv"))

scatter_data <- scatter_data %>% 
  mutate( plot_type = "scatter")%>% 
  separate(.id, into = c("study",NA), remove = F, extra = "drop") %>% 
  rename (filename = .id, variable = id) 

unique((scatter_data %>% filter(study == "2877" & y_variable == "annelid abundance"))$filename)

# study 2877 is not technically a scatter plot but i used the scatter extraction tool to select numerical timepoints and then converted them to categorical variable 
# this study can be joined to big table as is essentially raw data and will be analysed in similar way 

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
  mutate(error_type = NaN, se = NaN, sd = NaN, n = 2, 
         mean = value, study = as.numeric(study), error_type = as.character(error_type)) %>% 
  rename (weeks_post_stressor = x) %>% 
  select ( -col, - group, -pch, -x_variable, -weeks_post_stressor, -value) # some responses measured at more time points 


head(study2877)
unique(study2877$group_id)

### add to big table

big_table <- bind_rows (big_table, study2877)
sort(unique(big_table$study))
# this could be pretty similar to final data format and has already been calculated as an effect size 

study1970<- scatter_data %>% filter(study == "1970")  # reduced water flow and insecticide
unique(study1970$variable)

#### check studies against list or relevant studies (as at beginning i extracted some data that didn't fit the refined criteria, i.e. at least 5 responses)
checklist <- read_csv(here::here("data","metaanalysis_progress.csv")) 

big_table_checked <- big_table %>% filter(study %in% checklist$number)

unique(big_table$study) 
sort(unique(big_table_checked$study)) 
unique(scatter_data$study)
sort(unique(checklist$number))

saveRDS(big_table_checked, file = "output/completetable.rds") 

## habitats and sites and consumer notation still needs to be standardized. 
