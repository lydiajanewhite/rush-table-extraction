library(readr)
library(tidyverse)

# 809
data <- read_csv(here::here("data","809_graeber2017_macroinvertebrate_abundance.csv"))
head(data)
tail(data)

unique(data$Phase)
data <- data %>% 
  filter(!is.na (Treatment)) 

## change treatment names, already single treatment 
## Nutrients np -> N0 & N1
## reduced water velocity:  nf & lf -> V0 & V1
## sedimentation: fs ->  S0 & S1

data <- data %>% 
  mutate(treatment =recode(Treatment, 'nf' ="N0_S0_V0", 
                           'lf' ="N0_S0_V1",'nf.np' ="N1_S0_V0",
                           'lf.np' ="N1_S0_V1", 'lf.fs' ="N0_S1_V1", 
                           'lf.np.fs' ="N1_S1_V1"),
         habitat = recode(Habitat, "RUN" = "habitat1", "RIF" ="habitat2")) %>% 
  unite("group_id", treatment, habitat, remove = F) 

