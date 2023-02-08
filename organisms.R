library(adklakedata)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


phy <- adk_data("phyto")

zoo1 <- adk_data("crust")
zoo2 <- adk_data("rotifer")

meteo <- adk_data("met")

meta <- adk_data("meta")

chem <- adk_data("chem")

nutr <- adk_data("nutrient")

secchi <- adk_data("secchi")

tdo <- adk_data("tempdo")


pgen <- select(phy, lake.name, Genus) %>% 
  mutate(Genus = trimws(Genus)) %>% 
  filter(Genus != "unknown") %>% 
  unique() %>% 
  mutate(pres = 1) %>% 
  tidyr::spread(key = lake.name, value = pres, fill = 0)

cgen <- zoo1 %>% 
  filter(org.l > 0, Genus != "unknown") %>% 
  select(lake.name, Genus) %>% unique() %>% 
  mutate(pres = 1) %>% 
  tidyr::spread(key = lake.name, value = pres, fill = 0)

rgen <- zoo2 %>% 
  filter(org.l > 0, Genus != "unknown") %>% 
  select(lake.name, Genus) %>% unique() %>%
  mutate(pres = 1) %>% 
  tidyr::spread(key = lake.name, value = pres, fill = 0)


splist <- rbind(pgen, cgen, rgen)

t(rbind(pgen, cgen, rgen)[,-1]) %>% dist() %>% hclust() %>% plot()



phy %>% 
  select(Phylum:Species, biovol.um3.per.cell) %>% 
  mutate(Genus = trimws(Genus)) %>% 
  filter(Genus != "unknown") %>% 
  unique() %>% 
  group_by(Phylum, Class, Order, Family, Genus) %>% 
  summarize(biovol_um3_cell = median(biovol.um3.per.cell), n = n()) %>% 
  print(n = 200)

zoo1 %>% 
  select(Group:ug_WWperind) %>% 
  unique() %>% 
  filter(Genus != "unknown") %>% 
  group_by(Group, Taxa, Genus) %>% 
  summarize(ug_WW_ind = median(ug_WWperind), n = n()) %>% 
  print(n = 30)
  
zoo2 %>% 
  select(Group:ug_WWperind) %>% 
  unique() %>% 
  filter(Genus != "unknown") %>% 
  group_by(Group, Family, Genus) %>% 
  summarize(ug_WW_ind = median(ug_WWperind), n = n()) %>% 
  print(n = 30)
