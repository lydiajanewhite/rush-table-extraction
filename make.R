# need to run all import scripts 
# then collate with other data and reformat 
# then check data and rename/revise some entries 

library(tidyverse)
library(here)

list.files("analysis/importing/", full.names = TRUE) |> purrr::map(source,local=TRUE)

