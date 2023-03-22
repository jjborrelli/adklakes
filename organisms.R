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

t(rbind(pgen, cgen, rgen)[,-1]) %>% vegan::vegdist(method = "jaccard") %>% hclust() %>% 
  plot(ylab = "")


pgen <- select(phy, lake.name, Genus, biovol.um3.per.ml) %>% 
  mutate(Genus = trimws(Genus)) %>% 
  filter(Genus != "unknown") %>% 
  group_by(lake.name, Genus) %>% 
  summarize(pres = mean(biovol.um3.per.ml)) %>% 
  tidyr::spread(key = lake.name, value = pres, fill = 0)

cgen <- zoo1 %>% 
  filter(org.l > 0, Genus != "unknown") %>% 
  select(lake.name, Genus, mgWW.l) %>% group_by(lake.name, Genus) %>% 
  summarize(pres = mean(mgWW.l)) %>%
  tidyr::spread(key = lake.name, value = pres, fill = 0)

rgen <- zoo2 %>% 
  filter(org.l > 0, Genus != "unknown") %>% 
  select(lake.name, Genus, mgWW.l) %>% group_by(lake.name, Genus) %>% 
  summarize(pres = mean(mgWW.l)) %>%
  tidyr::spread(key = lake.name, value = pres, fill = 0)


t(rbind(pgen, cgen, rgen)[,-1]) %>% vegan::vegdist(method = "bray") %>% hclust() %>% 
  plot(ylab = "")



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



unique(phy$Phylum)
phy %>% 
  select(lake.name, Phylum:Species, biovol.um3.per.ml) %>% 
  mutate(Genus = trimws(Genus)) %>% 
  filter(Genus != "unknown") %>%
  group_by(lake.name, Phylum, Class, Order, Family, Genus) %>% 
  summarize(biovol_um3_ml = median(biovol.um3.per.ml), n = n()) %>% 
  filter(Phylum %in% c("Chlorophyta", "Bacillariophyta", "Cyanobacteria", "Charophyta", "Ochrophyta")) %>% 
  ggplot(aes(x = reorder(Genus, biovol_um3_ml), y = biovol_um3_ml/10^6)) + geom_boxplot() + 
  scale_y_log10() + 
  coord_flip() + 
  facet_wrap(~Phylum, scales = "free_y", nrow = 1)


bind_rows(zoo1, zoo2) %>% 
  select(lake.name, Group:ug_WWperind, mgWW.l, org.l) %>% 
  filter(org.l > 0, Genus != "unknown") %>% 
  group_by(lake.name, Group, Taxa, Genus) %>% 
  summarize(mgWW.l = median(mgWW.l), n = n()) %>% 
  ggplot(aes(x = reorder(Genus, mgWW.l), y = mgWW.l)) + geom_boxplot() + 
  scale_y_log10() + 
  coord_flip() + 
  facet_wrap(~Group, scales = "free_y", nrow = 1)



tdo %>% filter(lake.name %in% c("Willis")) %>% 
  ggplot(aes(x = depth, y = temp)) + geom_line(aes(group = date)) + 
  geom_point() +
  scale_x_reverse() + 
  coord_flip() + 
  facet_grid(lake.name~month(date))


kdval <- secchi %>% group_by(lake.name, year) %>% 
  summarize(kd = mean(1.7/secchi, na.rm = TRUE))

hist(kdval$kd)
print(arrange(kdval, desc(kd)), n = 30)

ggplot(kdval, aes(x = year, y = kd, group = lake.name)) + geom_line() + geom_point()

ggplot(mutate(chem, lake.name = factor(lake.name, levels = arrange(kdval, desc(kd))$lake.name)),
       aes(x = ymd(date), y = DOC, group = lake.name)) + 
  geom_point() + geom_smooth(method = "gam") + facet_wrap(~lake.name)



library(rglobi)

allgenera <- c(unique(phy$Genus), unique(zoo1$Genus), unique(zoo2$Genus))
imat <- get_interactions(taxon = allgenera[!grepl("unknown", tolower(allgenera))],
                         interaction.type = "eats")
m2 <- sapply(imat[,-1], function(x) unlist(x))

# 
# splist <- read.csv("../../OneDrive/HAVENS51AdirondackLakesCSVs/species.csv")
# lf <- list.files("../../OneDrive/HAVENS51AdirondackLakesCSVs/WEBS_INIT/")
# 
# amats <- list()
# for(i in seq_along(lf)){
#   amats[[i]] <- read.csv(paste0("../../OneDrive/HAVENS51AdirondackLakesCSVs/WEBS_INIT/", lf[i]), row.names = 1)
# }
# 
# spints <- reshape2::melt(as.matrix(amats[[2]])) %>%
#   mutate(Var2 = as.numeric(gsub("X", "", Var2))) %>%
#   filter(value == 1) %>% unique()
# 
# spints %>%
#   filter(!Var1 %in% c(28:29), !Var2 %in% c(28:29)) %>%
#   left_join(splist, by = c("Var1" = "NodeID")) %>%
#   left_join(splist, by = c("Var2" = "NodeID")) %>%
#   mutate(from = paste(Genus.x, Species.x), to = paste(Genus.y, Species.y)) %>%
#   select(Var1, Var2, from, to) %>%
#   mutate(from = case_when(Var1 == 220 ~ "benthic detritus", Var1 == 221 ~ "periphyton", TRUE ~ from))
