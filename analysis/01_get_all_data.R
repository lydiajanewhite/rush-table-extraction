library(tidyverse)
library(here)

figure_data <- read_csv(here::here("data","digitised_data_figures.csv"))

summary_table_data <- read_csv(here::here("data","digitised_data_tables.csv"), na = c("na", "NA", ""))
summary_table_data$r <- NaN

table_data <- list.files("output/individual_datasets", full.names = TRUE) %>% 
  purrr::map(readRDS) %>% 
  bind_rows() %>% 
  mutate(error_type = "sd", plot_type = "table", r = NaN, se = NaN)

big_table <- bind_rows(figure_data, summary_table_data, table_data) %>% 
  separate(filename, into = c("study",NA), remove = F, extra = "drop")

write_tsv(big_table, file = "output/big_table.tsv") 

test <- big_table %>% filter(is.na(sd)) %>% 
  filter (n > 1) %>% 
  filter(is.na(se)) %>% 
  filter (mean != "0")

unique(test$study)
test
