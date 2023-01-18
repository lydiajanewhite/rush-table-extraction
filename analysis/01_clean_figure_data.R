# clean figure data  
 
# for figure data, sometimes zeros were clicked, but due to coarseness of the graphs 
#  they come up as vary small numers (relative to maximum axis value)

# to investigate this, we inspect figures where mean values are less than 1% of the maximum value 
figure_data <- read_csv(here::here("data","digitised_data_summaryplot.csv")) %>% 
  select(-r)

figure_data %>% 
  filter(mean < 0.001 & mean >-0.001) %>% 
  filter(sd < 0.001 & sd >-0.001) %>% 
  select(filename) %>% 
  unique()

outliers <- figure_data %>% filter(mean != 0) %>% 
  group_by(filename) %>% 
  summarise(proportion = max(abs(mean))/min(abs(mean)), min = min(abs(mean)), max = max(abs(mean))) %>% 
  arrange(-proportion) 

outliers[1:100,]


# zeros - all above 2468_Sundback2010_diatom_biomass.png i.e. row 43
# 2468_Sundback2010_diatom-plus-flagellates_biom is weird, r extraction needs inspecting, cannot see zero on graph so unsure where extreme value comes from 
# same for 2468_Sundback2010_diatom-plus-flagellates-plus-
# same for 2468_Sundback2010_diatom_biomass.png
# zero 2533_Fukunaga2010_macomona_abundance.png 

# 
