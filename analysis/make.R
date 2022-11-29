# need to run all import scripts 
# then collate with other data and reformat 
# then check data and rename/revise some entries 

library(tidyverse)

list.files(here::here("analysis/importing"))
list.files(here::here("data")) 


library(here)
here()

list.files(here::here("analysis/importing")) %>% purrr::map(source)

source(list.files(here::here("analysis/importing"))) 

list.files(here::here("analysis/importing/"), full.names = TRUE) |> purrr::map(source)


