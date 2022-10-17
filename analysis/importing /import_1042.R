library(readr)
library(tidyverse)

# 1042
data <- read_csv(here::here("data","1042_Baekkelie2017_macroinvertebrate_abundance.csv"))
unique(data$Treatment)
unique(data$Time, )