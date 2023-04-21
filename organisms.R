library(adklakedata)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(rLakeAnalyzer)

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
  dplyr::select(Phylum:Species, biovol.um3.per.cell) %>% 
  mutate(Genus = trimws(Genus)) %>% 
  filter(Genus != "unknown") %>% 
  unique() %>% 
  group_by(Phylum, Class, Order, Family, Genus) %>% 
  summarize(biovol_um3_cell = median(biovol.um3.per.cell), n = n()) %>% 
  print(n = 200)

zoo1 %>% 
  dplyr::select(Group:ug_WWperind) %>% 
  unique() %>% 
  filter(Genus != "unknown") %>% 
  group_by(Group, Taxa, Genus) %>% 
  summarize(ug_WW_ind = median(ug_WWperind), n = n()) %>% 
  print(n = 30)
  
zoo2 %>% 
  dplyr::select(Group:ug_WWperind) %>% 
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



tdo %>% filter(lake.name %in% c("Big Moose")) %>% 
  ggplot(aes(x = depth, y = temp)) + geom_line(aes(group = date)) + 
  geom_point() +
  scale_x_reverse() + 
  coord_flip() + 
  facet_grid(lake.name~month(date))

tdo %>% group_by(lake.name, date, depth) %>% 
  summarize(temp = mean(temp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(lake.name, date) %>% 
  summarize(thermo = thermo.depth(temp, depth),
            diff = max(temp) - min(temp)) %>% 
  mutate(date = ymd_hms(date)) %>%
  filter(diff >= 2) %>% 
  ggplot(aes(x = yday(date), y = thermo)) + geom_point() + facet_wrap(~lake.name)


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


zoo1 %>% filter(Genus %in% allgenera[!grepl("unknown", tolower(allgenera))][181:222]) %>% 
  group_by(Genus) %>% summarize(mean_mass = mean(ug_WWperind)) %>% 
  mutate(upper = Param_reginvert[[2]][1] + Param_reginvert[[2]][2] * log10(mean_mass),
         lower = Param_reginvert[[3]][1] + Param_reginvert[[3]][2] * log10(mean_mass), 
         cent = lower + (upper - lower)/2)

phymass <- phy %>% group_by(Genus) %>% summarize(mean_mass = mean(biovol.um3.per.cell/10^6)) 

range(phymass$mean_mass)
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



library(TSclust)

docts <- chem %>% select(lake.name, date, Color.PtCo) %>% spread(key = lake.name, value = Color.PtCo) %>% 
  group_by(year = year(ymd(date))) %>% select(-date) %>% summarize_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% select(-year)

dist_ts <- diss(docts, METHOD = "CORT") 

hc <- stats::hclust(dist_ts, method="complete") 
hclus <- stats::cutree(hc, k = 2) %>% # hclus <- cluster::pam(dist_ts, k = 2)$clustering has a similar result
  as.data.frame(.) %>%
  dplyr::rename(.,cluster_group = .) %>%
  tibble::rownames_to_column("type_col")

hcdata <- ggdendro::dendro_data(hc)
names_order <- hcdata$labels$label
# Use the folloing to remove labels from dendogram so not doubling up - but good for checking hcdata$labels$label <- ""

p1 <- hcdata %>%
  ggdendro::ggdendrogram(., rotate=FALSE, leaf_labels=FALSE)

p2 <- docts %>%
  dplyr::mutate(index = 1:nrow(docts)) %>%
  tidyr::gather(key = type_col,value = value, -index) %>%
  dplyr::full_join(., hclus, by = "type_col") %>% 
  mutate(type_col = factor(type_col, levels = hcdata$labels$label)) %>% 
  ggplot(aes(x = index, y = value, colour = cluster_group)) +
  geom_line() +
  facet_wrap(~type_col, nrow = 1, strip.position="top") + 
  guides(color=FALSE) +
  theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_blank())

gp1<-ggplotGrob(p1)
gp2<-ggplotGrob(p2) 

gridExtra::grid.arrange(gp1, gp2, ncol=1, heights=c(3,3))



# DTwarp cluster plot based on
## https://damien-datasci-blog.netlify.app/post/time-series-clustering-with-dynamic-time-warp/
docts <- nutr %>% filter(!is.na(TotalP.ug.L)) %>% 
  select(lake.name, date, TotalP.ug.L) %>% 
  group_by(lake.name, date) %>% 
  summarize(TotalP.ug.L = mean(TotalP.ug.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key = lake.name, value = TotalP.ug.L) %>% 
  group_by(year = year(ymd(date))) %>% select(-date) %>% summarize_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% select(-year)

docts <- docts[,!is.nan(colSums(docts))]
dist_ts <- diss(docts, METHOD = "DTWARP") 

hc <- stats::hclust(dist_ts, method="complete") 
hclus <- stats::cutree(hc, k = 2) %>% # hclus <- cluster::pam(dist_ts, k = 2)$clustering has a similar result
  as.data.frame(.) %>%
  dplyr::rename(.,cluster_group = .) %>%
  tibble::rownames_to_column("type_col")

hcdata <- ggdendro::dendro_data(hc)
names_order <- hcdata$labels$label
# Use the folloing to remove labels from dendogram so not doubling up - but good for checking hcdata$labels$label <- ""

p1 <- hcdata %>%
  ggdendro::ggdendrogram(., rotate=FALSE, leaf_labels=FALSE)

p2 <- docts %>%
  dplyr::mutate(index = 1:nrow(docts)) %>%
  tidyr::gather(key = type_col,value = value, -index) %>%
  dplyr::full_join(., hclus, by = "type_col") %>% 
  mutate(type_col = factor(type_col, levels = hcdata$labels$label)) %>% 
  ggplot(aes(x = index, y = value, colour = cluster_group)) +
  geom_line() +
  facet_wrap(~type_col, nrow = 1, strip.position="top") + 
  guides(color=FALSE) +
  theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_blank())

gp1<-ggplotGrob(p1)
gp2<-ggplotGrob(p2) 

gridExtra::grid.arrange(gp1, gp2, ncol=1, heights=c(3,3))

