
# https://cran.rstudio.com/web/packages/nhdR/index.html
library(nhdR)
library(sf)
library(dplyr)
library(ggplot2)

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
# nhdny <- st_read("NHD_H_New_York_State_Shape/Shape/", "NHDArea")
# 
# https://geodata.lib.utexas.edu/catalog/cugir-007739
adkalt <- st_read("C:/Users/borre/Downloads/cugir-007739/cugir-007739/", "blueline")
# 
# install.packages("nhdR")
# 
# test <- nhd_get(state = "NY")
# nhd_list("NY")
# 
# testALL <- nhd_load("NY", "NHDWaterbody")
# 
# st_crop(test,apa) %>% saveRDS("./nhd/adk_nhd.rds")

testwbd <- nhd_load("NY", "WBDHU12")
testwbd10 <- nhd_load("NY", "WBDHU10")

test <- readRDS("nhd/adk_nhd.rds")
apa <- st_transform(adkland, st_crs(test))


test %>% 
  ggplot() + geom_sf(color = "blue", fill = "blue") + 
  geom_sf(data = st_transform(adkalt, st_crs(test)), linewidth = 2)


# adkwater <- st_intersection(st_transform(adkalt, st_crs(test)), test)
# saveRDS(st_intersection(adkwater, st_transform(adkalt, st_crs(adkwater))), "nhd/adk_nhd2.rds")
adkwater <- readRDS("nhd/adk_nhd2.rds")


ggplot() +
  geom_sf(data = filter(states, ID == "new york")) + 
  #geom_sf(data = test, color = "grey50", fill = "grey50") +
  geom_sf(data = st_transform(adkalt, st_crs(test)), linewidth = 1.2) + 
  geom_sf(data = adkwater, color = "blue", fill = "blue") + 
  theme_void()

ggplot() +
  #geom_sf(data = filter(states, ID == "new york")) + 
  #geom_sf(data = testALL, color = "grey40", fill = "grey40") +
  geom_sf(data = st_transform(adkalt, st_crs(test)), linewidth = 1.2, fill = NA) + 
  geom_sf(data = adkwater, color = "blue", fill = "blue") + 
  theme_void()

#ggsave("newyorkwaterALL_adk.png", height = 8, width = 12)
#ggsave("adk_waters.png", height = 8, width = 8)

# adkwaterbd0 <- st_crop(testwbd, st_transform(adkalt, st_crs(testwbd)))
# adkwaterbd <- st_intersection(st_transform(adkalt, st_crs(testwbd)), testwbd)
# adkwaterbd10 <- st_intersection(st_transform(adkalt, st_crs(testwbd10)), testwbd10)

# saveRDS(adkwaterbd, "nhd/adk_nhd_wb12_adk.rds")
# saveRDS(adkwaterbd10, "nhd/adk_nhd_wb10_adk.rds")
adkwaterbd <- readRDS("nhd/adk_nhd_wb12_adk.rds")
adkwaterbd10 <- readRDS("nhd/adk_nhd_wb10_adk.rds")

wbdplot <- ggplot() +
  #geom_sf(data = st_transform(adkalt, st_crs(test)), linewidth = 1.2) + 
  geom_sf(data = adkwaterbd10, color = "black") +
  geom_sf(data = st_transform(adkalt, st_crs(test)), linewidth = 1.2, fill = NA) + 
  geom_sf(data = adkwater, color = "blue", fill = "blue") + 
  theme_minimal() + 
  labs(x = "", y = "", fill = "") +
  ggspatial::annotation_north_arrow(style = north_arrow_nautical(), 
                                    pad_y = unit(1, "cm"), pad_x = unit(1.2, "cm")) + 
  ggspatial::annotation_scale()

#ggsave("adkwaterboundary_hu10.png", height = 8, width = 8)

library(FedData)
library(raster)
library(rgdal)
library(terra)

nlcd <- get_nlcd(
  template = adkalt,
  label = "adk",
  year = 2019,
  dataset = c("landcover", "impervious", "canopy"),
  landmass = "L48",
  extraction.dir = "./FedData/",
  raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
  force.redo = FALSE
)

plot(nlcd)

adknlcd <- st_transform(adkalt, st_crs(nlcd))
nlcd2 <- crop(nlcd, adknlcd)
nlcd3 <- mask(nlcd2, adknlcd)
# 
# lgwa <- st_read("C:/Users/borre/OneDrive/Documents/JP_R/maps/LGWatershed/", "LGWatershedArea")
# nlcd2 <- crop(nlcd, st_transform(lgwa, st_crs(nlcd)))
# nlcd3 <- mask(nlcd2, st_transform(lgwa, st_crs(nlcd)))

plot(nlcd3)

nlcdd <- as.data.frame(nlcd3, xy = TRUE) %>% 
  filter(!is.na(Class_Class))
head(nlcdd)

classlevels <- c("Barren Land (Rock/Sand/Clay)",
  "Developed, Open Space", 
  "Developed, Low Intensity", 
  "Developed, Medium Intensity", 
  "Developed High Intensity", 
  "Pasture/Hay", 
  "Cultivated Crops",
  "Grassland/Herbaceous", 
  "Shrub/Scrub",
  "Deciduous Forest",
  "Evergreen Forest", 
  "Mixed Forest", 
  "Woody Wetlands", 
  "Emergent Herbaceous Wetlands", 
  "Open Water")
colorcombos <- dplyr::select(nlcdd, Class_Color, Class_Class) %>% unique() 
colorcombos$Class_Class <- factor(colorcombos$Class_Class, levels = classlevels)
colorcombos <- arrange(colorcombos, Class_Class)

nlcdd$Class_Class <- factor(nlcdd$Class_Class, levels = classlevels) 

lcplot <- ggplot((nlcdd), aes(x = x, y = y)) + 
  geom_raster(aes(fill = Class_Class)) + 
  scale_fill_manual(values = colorcombos$Class_Color, label = colorcombos$Class_Class) +
  coord_sf(crs = crs(nlcd3)) + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(x = "", y = "", fill = "") #+
  # ggspatial::annotation_north_arrow(style = north_arrow_nautical(), 
  #                                   pad_y = unit(1, "cm"), pad_x = unit(1.2, "cm")) + 
  # ggspatial::annotation_scale()

lcplot

#ggsave("adklandcover.png", height = 8, width = 12)

#ggsave("adk_wbd_lc.png", height = 8, width = 16)


nlcdd %>% 
  group_by(Class_Class) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% mutate(per = n/sum(n)*100) %>% 
  ggplot(aes(x = (Class_Class), y = per, fill = Class_Class)) + geom_bar(stat = "identity")+ 
  scale_fill_manual(values = colorcombos$Class_Color, label = colorcombos$Class_Class) +
  coord_flip() + 
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none") + 
  labs(x = "", y = "Percent Cover")


ggplot((nlcdd)) + 
  geom_raster(aes(x = x, y = y, fill = Class_Class)) + 
  #geom_sf(data = st_transform(adkwaterbd10, st_crs(nlcd3)), fill = NA, color = "black") + 
  scale_fill_manual(values = colorcombos$Class_Color, label = colorcombos$Class_Class) +
  coord_sf(crs = crs(nlcd3)) + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(x = "", y = "", fill = "") +
  ggspatial::annotation_north_arrow(style = north_arrow_nautical(),
                                    pad_y = unit(1, "cm"), pad_x = unit(1.2, "cm")) +
  ggspatial::annotation_scale()


#ggsave("adklandcoverWBD.png", height = 8, width = 12)
#ggsave("adklandcover2.png", height = 8, width = 8)

nlcdd %>% 
  mutate(class = case_when(Class_Class == "Barren Land (Rock/Sand/Clay)" ~ "Barren", 
                           Class_Class == "Deciduous Forest" ~ "Forest", 
                           Class_Class == "Developed High Intensity" ~ "Developed", 
                           Class_Class == "Developed, Low Intensity" ~ "Developed",
                           Class_Class == "Developed, Medium Intensity" ~ "Developed",
                           Class_Class == "Developed, Open Space" ~ "Developed", 
                           Class_Class == "Emergent Herbaceous Wetlands" ~ "Wetland", 
                           Class_Class == "Evergreen Forest" ~ "Forest", 
                           Class_Class == "Grassland/Herbaceous" ~ "Grassy Scrub", 
                           Class_Class == "Mixed Forest" ~ "Forest", 
                           Class_Class == "Open Water" ~ "Water", 
                           Class_Class == "Pasture/Hay" ~ "Farm", 
                           Class_Class == "Shrub/Scrub" ~ "Grassy Scrub", 
                           Class_Class == "Woody Wetlands" ~ "Wetland", 
                           Class_Class == "Cultivated Crops" ~ "Farm"), 
         class = factor(class, levels = c("Barren", "Developed", "Farm", "Grassy Scrub", "Water", "Wetland", "Forest"))) %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = class)) + 
  #geom_sf(data = st_transform(adkwaterbd10, st_crs(nlcd3)), fill = NA, color = "black") + 
  scale_fill_manual(values = coldf$color, label = coldf$class) +
  coord_sf(crs = crs(nlcd3)) + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(x = "", y = "", fill = "")


## LAND COVER ##########################################
########################################################

nlcd3
dim(adkwaterbd10)
dim(adkwaterbd)

n2df <- list()
for(i in 1:nrow(adkwaterbd10)){
  wbd_i <- adkwaterbd10[i,]
  a <- st_transform(wbd_i, st_crs(nlcd))
  n1 <- crop(nlcd3, a)
  n2 <- mask(n1, a)
  
  n2df[[i]] <- as.data.frame(n2, xy = TRUE) %>% 
    filter(!is.na(Class_Class)) %>% 
    mutate(huc10 = wbd_i$huc10, hutype = wbd_i$hutype, hu_name = wbd_i$name,
           humod = wbd_i$humod, hu_length = wbd_i$shape_Length, hu_area = wbd_i$shape_Area)
  
  print(i)
}

n2df <- data.table::rbindlist(n2df) 
n2df2 <- n2df %>%
  group_by(huc10, hu_length, hu_area, Class_Class) %>%
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(huc10, hu_length, hu_area) %>%
  mutate(percent = n/sum(n))


lcmatrix <- n2df2 %>% ungroup() %>% 
  dplyr::select(huc10, Class_Class, percent) %>% 
  tidyr::spread(key = Class_Class, value = percent, fill = 0) %>% 
  dplyr::select(-huc10)

biplot(princomp(lcmatrix))
loadings(princomp(lcmatrix))
nmds <- vegan::metaMDS(lcmatrix, trymax = 1000)
plot(nmds)


tidyr::gather(lcmatrix) %>% ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_boxplot() + coord_flip() + 
  scale_y_log10()


n2df2 %>% ungroup() %>% 
  dplyr::select(huc10, Class_Class, percent, hu_area) %>%
  mutate(Class_Class = factor(Class_Class, levels = colorcombos$Class_Class)) %>%  
  ggplot(aes(x = reorder(huc10, hu_area), y = percent, fill = Class_Class)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  scale_fill_manual(values = colorcombos$Class_Color, label = colorcombos$Class_Class)




n2df3 <- n2df2 %>% ungroup() %>% 
  dplyr::select(huc10, Class_Class, percent, hu_area) %>%
  mutate(class = case_when(Class_Class == "Barren Land (Rock/Sand/Clay)" ~ "Barren", 
                           Class_Class == "Deciduous Forest" ~ "Forest", 
                           Class_Class == "Developed High Intensity" ~ "Developed", 
                           Class_Class == "Developed, Low Intensity" ~ "Developed",
                           Class_Class == "Developed, Medium Intensity" ~ "Developed",
                           Class_Class == "Developed, Open Space" ~ "Developed", 
                           Class_Class == "Emergent Herbaceous Wetlands" ~ "Wetland", 
                           Class_Class == "Evergreen Forest" ~ "Forest", 
                           Class_Class == "Grassland/Herbaceous" ~ "Grassy Scrub", 
                           Class_Class == "Mixed Forest" ~ "Forest", 
                           Class_Class == "Open Water" ~ "Water", 
                           Class_Class == "Pasture/Hay" ~ "Farm", 
                           Class_Class == "Shrub/Scrub" ~ "Grassy Scrub", 
                           Class_Class == "Woody Wetlands" ~ "Wetland", 
                           Class_Class == "Cultivated Crops" ~ "Farm"), 
         class = factor(class, levels = c("Barren", "Developed", "Farm", "Grassy Scrub", "Water", "Wetland", "Forest"))) %>% 
  group_by(huc10, hu_area, class) %>% summarize(percent = sum(percent))

coldf <- data.frame(class = unique(n2df3$class), color = c("grey40", "green4", "grey60", "lightblue", "green3", "blue", "chocolate")) %>% 
  mutate(class = factor(class, levels = c("Barren", "Developed", "Farm", "Grassy Scrub", "Water", "Wetland", "Forest"))) %>% arrange(class)
ggplot(n2df3, aes(x = reorder(huc10, hu_area), y = percent*100, fill = class)) + 
  geom_bar(stat = "identity") + #coord_flip() + 
  scale_fill_manual(values = coldf$color, label = coldf$class) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(fill = "", y = "%")



## NAMES ###############################################
########################################################
adkwater %>% filter(!is.na(gnis_id))

sum(!is.na(adkwater$gnis_id))
length(adkwater$gnis_id)

head(rev(sort(table(adkwater$gnis_name))), 15)

# adkwaternames <- sort((adkwater$gnis_name))
# adkwaternames <- gsub("Lakes", "", adkwaternames)
# adkwaternames <- gsub("Lake", "", adkwaternames)
# adkwaternames <- gsub("Ponds", "", adkwaternames)
# adkwaternames <- gsub("Pond", "", adkwaternames)
# adkwaternames <- gsub("Reservoir", "", adkwaternames)
# adkwaternames <- gsub("River", "", adkwaternames)
# awndf <- data.frame(names = tolower(trimws(adkwaternames)))
# awndf <- awndf %>% group_by(names) %>% summarize(n = n()) %>% arrange(desc(n))
# par(mar = c(0,0,0,0))
# wordcloud(words = awndf$names, freq = awndf$n, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"),
#           scale=c(6,1))

adkwater %>% 
  filter(gnis_name == "Round Pond")


g <- purrr::map(filter(adkwater, grepl("Round", gnis_name))$gnis_id,
                function(x) {
                  ggplot() +
                    geom_sf(data = filter(adkwater, gnis_id == x)) +
                    guides(fill = FALSE) + 
                    theme_void()
                })

round_area <- purrr::map_dbl(filter(adkwater, grepl("Round", gnis_name))$gnis_id,
                             function(x) {
                               st_area(filter(adkwater, gnis_id == x))
                             })

circum <- purrr::map_dbl(filter(adkwater, grepl("Round", gnis_name))$gnis_id,
                         function(x) {
                           max(st_length(st_cast(filter(adkwater, gnis_id == x), to = "LINESTRING")))
                         })
# circum^2/(4*pi)
# 
# (4 * pi * round_area)/circum^2
g2 <- cowplot::plot_grid(plotlist = g[order((4 * pi * round_area)/circum^2)], ncol = 7)
g2



g <- purrr::map(filter(adkwater, grepl("Long", gnis_name))$gnis_id,
                function(x) {
                  ggplot() +
                    geom_sf(data = filter(adkwater, gnis_id == x)) +
                    guides(fill = FALSE) + theme_void()
                })


long_length <- purrr::map_dbl(filter(adkwater, grepl("Long", gnis_name))$gnis_id,
           function(x) {
             max(st_distance(st_cast(st_cast(filter(adkwater, gnis_id == x), to =  "MULTIPOINT"), to = "POINT")))
           })

g2 <- cowplot::plot_grid(plotlist = g[order(long_length)], ncol = 9)
g2

g <- purrr::map(filter(adkwater, gnis_name == "Little Moose Lake")$gnis_id,
                function(x) {
                  ggplot() +
                    geom_sf(data = filter(adkwater, gnis_id == x)) +
                    guides(fill = FALSE)
                })
g2 <- cowplot::plot_grid(plotlist = g)
g2



round_area_all <- purrr::map(adkwater$gnis_id[!is.na(adkwater$gnis_id)],
                             function(x) {
                               max(st_area(filter(adkwater, gnis_id == x)))
                             })

circum_all <- purrr::map(adkwater$gnis_id[!is.na(adkwater$gnis_id)],
                         function(x) {
                           max(st_length(st_cast(st_cast(filter(adkwater, gnis_id == x), to = "MULTILINESTRING"), to = "LINESTRING")))
                         })

hist((4 * pi * unlist(round_area_all))/unlist(circum_all)^2, freq = FALSE, ylim = c(0,3))
hist((4 * pi * round_area)/circum^2, col = "blue", freq = FALSE, add = TRUE)


mean((4 * pi * unlist(round_area_all))/unlist(circum_all)^2)
mean((4 * pi * round_area)/circum^2)

t.test((4 * pi * unlist(round_area_all))/unlist(circum_all)^2, (4 * pi * round_area)/circum^2)


all_length <- purrr::map(adkwater$gnis_id[!is.na(adkwater$gnis_id)],
                              function(x) {
                                max(st_distance(st_cast(st_cast(filter(adkwater, gnis_id == x), to =  "MULTIPOINT"), to = "POINT")))
                              })

par(mfrow = c(2,1))
hist(log(unlist(all_length)), freq = FALSE, xlim = c(0,12))
hist(log(long_length), col = "blue", freq = FALSE, xlim = c(0,12))
t.test(unlist(all_length), long_length)

adkwater$gnis_name[!is.na(adkwater$gnis_name)][grepl("Flow",adkwater$gnis_name[!is.na(adkwater$gnis_name)])]


filter(adkwater, gnis_name == "Grass River Flow") %>% 
  ggplot() + geom_sf()

hydroprop <- data.frame(length = unlist(all_length), 
                        circumference = unlist(circum_all),
                        area = unlist(round_area_all),
                        name = adkwater$gnis_name[!is.na(adkwater$gnis_name)],
                        id = adkwater$gnis_id[!is.na(adkwater$gnis_id)],
                        ftype = adkwater$ftype[!is.na(adkwater$gnis_id)],
                        fcode = adkwater$fcode[!is.na(adkwater$gnis_id)],
                        nhd_length = adkwater$SHAPE_Length[!is.na(adkwater$gnis_id)],
                        nhd_area = adkwater$SHAPE_Area[!is.na(adkwater$gnis_id)])


head(hydroprop)
# write.csv(unique(hydroprop), "nhd/adk_nhd_wbdproperties.csv")

t.test(hydroprop$length[grepl("Long", hydroprop$name)],
       hydroprop$length[!grepl("Long", hydroprop$name)])

t.test(testALL$SHAPE_Length[(grepl("Long",testALL$gnis_name))],
       testALL$SHAPE_Length[!(grepl("Long",testALL$gnis_name)) & !is.na(testALL$gnis_name)])



############################################
## DATA FROM MAX

adkl <- st_read("data/Data/adk_lakes/", "adk_lakes")
ggplot(adkl) + geom_sf() + 
  geom_sf(data = filter(adkl, area_ha >= 1000), color = "blue", fill = "blue") + 
  geom_sf(data = filter(adkl, area_ha >= 100, area_ha < 1000), color = "black", fill = "black")


nhd <- st_read("data/Data/NHD/", "merged_NHD")
ggplot(nhd) + geom_sf()
