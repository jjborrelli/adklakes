library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)

# ADK Boundary
adkalt <- st_read("C:/Users/borre/Downloads/cugir-007739/cugir-007739/", "blueline")
adkalt2 <- st_transform(adkalt, "WGS84")

# Elevation 
adkel <- terra::rast("data/ADK_Data/Data/Elevation/elev_mosaic.tif")

# LAGOS data
lagos_adk <- readRDS("data/lagos_adk")

# ADK lake data summary
adklakeSUMMARY <- readRDS("adkl11.rds")
adkl <- st_read("data/ADK_Data/Data/adk_lakes/", "adk_lakes")
adkl <- st_set_crs(adkl, st_crs(adklakeSUMMARY))

d_rnt <- read.csv("data/roadNtrail_dist.csv")
adklSUM2 <- left_join(adklakeSUMMARY, d_rnt, by = "Permanent_")


adktt <- read.csv("../lakeshapes/adk_temp_trends.csv")

# NLCD
# nlcdd <- arrow::read_parquet("data/landcover.parquet")
nlcd3 <- readRDS("data/adk_nlcd3.rds")


adkhu06 <- readRDS("nhd/adk_nhd_wb06_adk.rds")
adkhu08 <- readRDS("nhd/adk_nhd_wb08_adk.rds")
adkhu10 <- readRDS("nhd/adk_nhd_wb10_adk.rds")
adkhu12 <- readRDS("nhd/adk_nhd_wb12_adk.rds")


adkhu06 %>% ggplot() + geom_sf()



adk6char %>% dplyr::select(huc6, nlakes, areasqkm) %>% mutate(nprop = nlakes/sum(nlakes))

hu6.a <- adk6char[1, ]

adkhu06 %>% ggplot() + geom_sf() + 
  geom_sf(data = hu6.a, fill = "blue") + theme_void()

hu6aLAKES <- st_intersection(adklSUM2, hu6.a)
hu6aLAKES %>% ggplot() + geom_sf(fill = "blue") + 
  geom_sf(data = hu6.a, fill = NA) + theme_void()


length(unique(hu6aLAKES$Permanent_))/length(unique(adklSUM2$Permanent_)) 

hist(hu6aLAKES$elevation_est)
hist(log10(adklSUM2$area_ha), freq = FALSE)
hist(log10(hu6aLAKES$area_ha), col = "blue", add = TRUE, freq = FALSE)

pA <- ggplot(adklSUM2, aes(x = elevation_est)) + geom_density() + 
  geom_density(data = hu6aLAKES, aes(x = elevation_est), color = 'blue')

pB <- ggplot(adklSUM2, aes(x = log10(area_ha))) + geom_density() + 
  geom_density(data = hu6aLAKES, aes(x = log10(area_ha)), color = 'blue')

ggplot(adklSUM2, aes(x = log10(area_ha), y = elevation_est)) + geom_point(alpha = 0.3) + 
  geom_point(data = hu6aLAKES, aes(x = log10(area_ha), y = elevation_est), color = 'blue')

pA | pB

hu6aLAKES %>% #as.data.frame() %>% #dplyr::select(aeap:awi) %>% 
  mutate(tot = aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi) %>% 
  filter(tot > 0) %>% 
  ggplot() + geom_sf() + facet_wrap(~lagosname1)

hu6aLAKES %>% ggplot() + geom_sf(fill = "blue") + 
  geom_sf(data = hu6.a, fill = NA) + 
  geom_sf(data = st_intersection(adk12char, hu6.a[1,]), fill = NA) + 
  theme_void()


a6l <- hu6aLAKES %>% #as.data.frame() %>%
  dplyr::select(Permanent_, aeap:awi, area_ha, GNIS_Name, lagosname1) %>% 
  unique() %>% 
  mutate(tot = aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi) %>% 
  filter(tot > 0)
length(unique(a6l$Permanent_))

a6l %>% as.data.frame() %>% 
  dplyr::select(GNIS_Name, lagosname1, altm, aeap, awi, als) %>% 
  filter(awi == 1)  %>%
  # filter(altm == 1) %>%
  # filter(als == 1, awi == 1) 
  I()


rd.a <- filter(adklSUM2, Permanent_ %in% hu6aLAKES$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, roaddist1_m:traildist1_m) %>% 
  group_by(Permanent_) %>% 
  mutate(minroad = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>% 
  arrange(minroad) %>% 
  print(n = 130)

filter(hu6aLAKES, Permanent_ %in% rd.a$Permanent_[rd.a$minroad < 100])$GNIS_Name

named_a <- filter(hu6aLAKES, Permanent_ %in% rd.a$Permanent_[rd.a$minroad < 250]) %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha) %>% arrange(GNIS_Name, lagosname1) %>% 
  unique() %>% 
  filter(!is.na(GNIS_Name) | !is.na(lagosname1)) %>% 
  arrange(area_ha) %>% 
  print(n = 285)

named_a2 <- named_a %>% st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha, lat, lon)

na3 <- left_join(named_a2, unique(dplyr::select(as.data.frame(lagos_adk), nhdid, maxdepth)), by = c("Permanent_" = "nhdid")) 

na3 %>% ggplot(aes(x = area_ha, y = maxdepth)) + geom_point()

################################################################################
################################################################################


hu6.b <- adk6char[2, ]


adkhu06 %>% ggplot() + geom_sf() + 
  geom_sf(data = hu6.b, fill = "blue") + theme_void()

hu6bLAKES <- st_intersection(adklSUM2, hu6.b)
hu6bLAKES %>% ggplot() + geom_sf(fill = "blue") + 
  geom_sf(data = hu6.b, fill = NA) + theme_void()

length(unique(hu6bLAKES$Permanent_))/length(unique(adklSUM2$Permanent_)) * 100

pA <- ggplot(adklSUM2, aes(x = elevation_est)) + geom_density() + 
  geom_density(data = hu6bLAKES, aes(x = elevation_est), color = 'blue')

pB <- ggplot(adklSUM2, aes(x = log10(area_ha))) + geom_density() + 
  geom_density(data = hu6bLAKES, aes(x = log10(area_ha)), color = 'blue')

pA | pB

b6l <- hu6bLAKES %>% #as.data.frame() %>%
  dplyr::select(Permanent_, aeap:awi, area_ha, GNIS_Name, lagosname1) %>% 
  unique() %>% 
  mutate(tot = aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi) %>% 
  filter(tot > 0)
length(unique(b6l$Permanent_))

b6l %>% as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, altm, aeap, awi) %>% 
  # filter(awi == 1)  %>%
  filter(altm == 1) %>%
  # filter(als == 1, awi == 1) 
  I()


quantile(b6l$area_ha, probs = seq(0,1,.1))

b6l %>% 
  filter(altm == 1) %>% 
  filter(Permanent_ %in% adktt$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(GNIS_Name, lagosname1, area_ha) %>% 
  arrange(desc(area_ha))


b6lID <- b6l %>% as.data.frame() %>%
  filter(altm == 1) 

hu6bLAKES %>% ggplot() + 
  geom_sf(fill = "lightblue") + 
  geom_sf(data = b6l[b6l$altm == 1,], fill = "blue") + 
  geom_sf(data = b6l[b6l$altm == 0 & b6l$awi == 1,], fill = "green4") + 
  geom_sf(data = hu6.b, fill = NA) + 
  geom_sf(data = st_intersection(adk10char, hu6.b[1,]), fill = NA) + 
  geom_sf(data = st_intersection(adk12char, hu6.b[1,]), fill = NA, lty = 2) + 
  theme_void()

b6l[b6l$altm == 1,]$area_ha

(sort(b6l[b6l$altm == 0 & b6l$awi == 1,]$area_ha))
(sort(b6l[b6l$altm == 1,]$area_ha))

lagos_adk %>% filter(nhdid %in% b6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = doc)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~lagosname1) + 
  theme_bw() + 
  labs(x = "date")
lagos_adk %>% filter(nhdid %in% b6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = colort)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm", aes(group = lagoslakeid)) + 
  facet_wrap(~lagosname1) + 
  theme_bw() + 
  labs(x = "date")

lagos_adk %>% filter(nhdid %in% b6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  group_by(nhdid, lagosname1) %>% 
  mutate(yr = year(ymd(sampledate))) %>% 
  nest() %>% 
  mutate(fit = purrr::map(data, ~lm(doc ~ yr, data = .x)), 
         slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  left_join(dplyr::select(as.data.frame(b6l), Permanent_, area_ha), 
            by = c("nhdid" = "Permanent_")) %>% 
  arrange(desc(area_ha))
  

rd.b <- filter(adklSUM2, Permanent_ %in% hu6bLAKES$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, roaddist1_m:traildist1_m) %>% 
  group_by(Permanent_) %>% 
  mutate(minroad = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>% 
  arrange(minroad) %>% 
  print(n = 130)

named_b <- filter(hu6bLAKES, Permanent_ %in% rd.b$Permanent_[rd.b$minroad < 250]) %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha) %>% arrange(GNIS_Name, lagosname1) %>% 
  unique() %>% 
  filter(!is.na(GNIS_Name) | !is.na(lagosname1)) %>% 
  arrange(area_ha) %>% 
  print(n = 285)

named_b2 <- named_b %>% st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha, lat, lon)

nb3 <- left_join(named_b2, unique(dplyr::select(as.data.frame(lagos_adk), nhdid, maxdepth)), by = c("Permanent_" = "nhdid")) 

nb3 %>% ggplot(aes(x = area_ha, y = maxdepth)) + geom_point()

################################################################################


hu6.c <- adk6char[3, ]

adkhu06 %>% ggplot() + geom_sf() + 
  geom_sf(data = hu6.c, fill = "blue") + theme_void()

hu6cLAKES <- st_intersection(adklSUM2, hu6.c)
hu6cLAKES %>% ggplot() + geom_sf(fill = "blue") + 
  geom_sf(data = hu6.c, fill = NA) + theme_void()

length(unique(hu6cLAKES$Permanent_))/length(unique(adklSUM2$Permanent_)) * 100

pA <- ggplot(adklSUM2, aes(x = elevation_est)) + geom_density() + 
  geom_density(data = hu6cLAKES, aes(x = elevation_est), color = 'blue')

pB <- ggplot(adklSUM2, aes(x = log10(area_ha))) + geom_density() + 
  geom_density(data = hu6cLAKES, aes(x = log10(area_ha)), color = 'blue')

pA | pB


c6l <- hu6cLAKES %>% #as.data.frame() %>%
  dplyr::select(Permanent_, aeap:awi, area_ha, GNIS_Name, lagosname1) %>% 
  unique() %>% 
  mutate(tot = aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi) %>% 
  filter(tot > 0)
length(unique(c6l$Permanent_))

c6l %>% as.data.frame() %>% 
  dplyr::select(GNIS_Name, lagosname1, altm, aeap, awi, als) %>% 
  # filter(awi == 1)  %>%
  # filter(altm == 1) %>%  
  filter(als == 1, awi == 1) 


quantile(c6l$area_ha, probs = seq(0,1,.1))

sort(c6l$area_ha[c6l$awi == 1])



hu6cLAKES %>% ggplot() + 
  geom_sf(fill = "lightblue") + 
  geom_sf(data = c6l[c6l$altm == 1,], fill = "blue") + 
  geom_sf(data = c6l[c6l$altm == 1 & c6l$awi == 1,], color = "green4", fill = NA) + 
  geom_sf(data = c6l[c6l$altm == 0 & c6l$awi == 1,], fill = "green4") + 
  # geom_sf(data = hu6.c, fill = NA) + 
  geom_sf(data = st_intersection(adkhu10, hu6.c), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.c), fill = NA, lty = 2) + 
  theme_void()

c6lID <- c6l %>% as.data.frame() %>%
  filter(altm == 1) 

lagos_adk %>% filter(nhdid %in% c6lID$Permanent_) %>% 
  # filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = doc)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~paste(lagosname1, nhdid)) + 
  theme_bw() + 
  labs(x = "date")
lagos_adk %>% filter(nhdid %in% c6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = colort)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm", aes(group = lagoslakeid)) + 
  facet_wrap(~lagosname1) + 
  theme_bw() + 
  labs(x = "date")

lagos_adk %>% filter(nhdid %in% c6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  group_by(nhdid, lagosname1) %>% 
  mutate(yr = year(ymd(sampledate))) %>% 
  nest() %>% 
  mutate(fit = purrr::map(data, ~lm(doc ~ yr, data = .x)), 
         slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  left_join(dplyr::select(as.data.frame(c6l), Permanent_, area_ha), 
            by = c("nhdid" = "Permanent_")) %>% 
  arrange(desc(area_ha))



rd.c <- filter(adklSUM2, Permanent_ %in% hu6cLAKES$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, roaddist1_m:traildist1_m) %>% 
  group_by(Permanent_) %>% 
  mutate(minroad = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>% 
  arrange(minroad) %>% 
  print(n = 130)

named_c <- filter(hu6cLAKES, Permanent_ %in% rd.c$Permanent_[rd.c$minroad < 250]) %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha) %>% arrange(GNIS_Name, lagosname1) %>% 
  unique() %>% 
  filter(!is.na(GNIS_Name) | !is.na(lagosname1)) %>% 
  arrange(area_ha) %>% 
  print(n = 285)

filter(hu6cLAKES, grepl("Raquette", GNIS_Name)) %>% ggplot() + geom_sf() + facet_wrap(~GNIS_Name)

named_c2 <- named_c %>% st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha, lat, lon)

nc3 <- left_join(named_c2, unique(dplyr::select(as.data.frame(lagos_adk), nhdid, maxdepth)), by = c("Permanent_" = "nhdid")) 

nc3 %>% ggplot(aes(x = area_ha, y = maxdepth)) + geom_point()

################################################################################

hu6.d <- adk6char[4, ]

adkhu06 %>% ggplot() + geom_sf() + 
  geom_sf(data = hu6.d, fill = "blue") + theme_void()

hu6dLAKES <- st_intersection(adklSUM2, hu6.d)
hu6dLAKES %>% ggplot() + geom_sf(fill = "blue") + 
  geom_sf(data = hu6.d, fill = NA) + theme_void()

length(unique(hu6dLAKES$Permanent_))/length(unique(adklSUM2$Permanent_)) * 100

pA <- ggplot(adklSUM2, aes(x = elevation_est)) + geom_density() + 
  geom_density(data = hu6dLAKES, aes(x = elevation_est), color = 'blue')

pB <- ggplot(adklSUM2, aes(x = log10(area_ha))) + geom_density() + 
  geom_density(data = hu6dLAKES, aes(x = log10(area_ha)), color = 'blue')

pA | pB


d6l <- hu6dLAKES %>% #as.data.frame() %>%
  dplyr::select(Permanent_, aeap:awi, area_ha, GNIS_Name, lagosname1) %>% 
  unique() %>% 
  mutate(tot = aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi) %>% 
  filter(tot > 0)
length(unique(d6l$Permanent_))

d6l %>% as.data.frame() %>% 
  dplyr::select(GNIS_Name, lagosname1, altm, aeap, awi, als) %>% 
  # filter(awi == 1)  %>%
  # filter(altm == 1) %>%
  filter(altm == 1, awi == 1) %>%
  I()


quantile(d6l$area_ha, probs = seq(0,1,.1))
sort(d6l$area_ha[d6l$altm == 1])
sort(d6l$area_ha[d6l$awi == 1])

hu6dLAKES %>% ggplot() + 
  geom_sf(fill = "lightblue") + 
  geom_sf(data = d6l[d6l$altm == 1,], fill = "blue") + 
  geom_sf(data = d6l[d6l$altm == 1 & d6l$awi == 1,], color = "green4", fill = NA) + 
  geom_sf(data = d6l[d6l$altm == 0 & d6l$awi == 1,], fill = "green4") + 
  # geom_sf(data = hu6.c, fill = NA) + 
  geom_sf(data = st_intersection(adkhu10, hu6.d), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.d), fill = NA, lty = 2) + 
  theme_void()


d6lID <- d6l %>% as.data.frame() %>%
  filter(altm == 1) 

lagos_adk %>% filter(nhdid %in% d6lID$Permanent_) %>% 
  # filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = doc)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~paste(lagosname1, nhdid)) + 
  theme_bw() + 
  labs(x = "date")
lagos_adk %>% filter(nhdid %in% d6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = colort)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm", aes(group = lagoslakeid)) + 
  facet_wrap(~lagosname1) + 
  theme_bw() + 
  labs(x = "date")

lagos_adk %>% filter(nhdid %in% d6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  group_by(nhdid, lagosname1) %>% 
  mutate(yr = year(ymd(sampledate))) %>% 
  nest() %>% 
  mutate(fit = purrr::map(data, ~lm(doc ~ yr, data = .x)), 
         slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  left_join(dplyr::select(as.data.frame(d6l), Permanent_, area_ha), 
            by = c("nhdid" = "Permanent_")) %>% 
  arrange(desc(area_ha))


rd.d <- filter(adklSUM2, Permanent_ %in% hu6dLAKES$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, roaddist1_m:traildist1_m) %>% 
  group_by(Permanent_) %>% 
  mutate(minroad = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>% 
  arrange(minroad) %>% 
  print(n = 130)

named_d <- filter(hu6dLAKES, Permanent_ %in% rd.d$Permanent_[rd.d$minroad < 250]) %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha) %>% arrange(GNIS_Name, lagosname1) %>% 
  unique() %>% 
  filter(!is.na(GNIS_Name) | !is.na(lagosname1)) %>% 
  arrange(area_ha)

named_d2 <- named_d %>% st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha, lat, lon)

nd3 <- left_join(named_d2, unique(dplyr::select(as.data.frame(lagos_adk), nhdid, maxdepth)), by = c("Permanent_" = "nhdid")) 

nd3 %>% ggplot(aes(x = area_ha, y = maxdepth)) + geom_point()

################################################################################

hu6.e <- adk6char[5, ]

adkhu06 %>% ggplot() + geom_sf() + 
  geom_sf(data = hu6.e, fill = "blue") + theme_void()

hu6eLAKES <- st_intersection(adklSUM2, hu6.e)
hu6eLAKES %>% ggplot() + geom_sf(fill = "blue") + 
  geom_sf(data = hu6.e, fill = NA) + theme_void()

length(unique(hu6eLAKES$Permanent_))/length(unique(adklSUM2$Permanent_)) * 100

pA <- ggplot(adklSUM2, aes(x = elevation_est)) + geom_density() +
  geom_density(data = hu6eLAKES, aes(x = elevation_est), color = 'blue')

pB <- ggplot(adklSUM2, aes(x = log10(area_ha))) + geom_density() +
  geom_density(data = hu6eLAKES, aes(x = log10(area_ha)), color = 'blue')

pA | pB


e6l <- hu6eLAKES %>% #as.data.frame() %>%
  dplyr::select(Permanent_, aeap:awi, area_ha, GNIS_Name, lagosname1) %>%
  unique() %>%
  mutate(tot = aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi) %>%
  filter(tot > 0)
length(unique(e6l$Permanent_))

e6l %>% as.data.frame() %>%
  dplyr::select(GNIS_Name, lagosname1, altm, aeap, awi, als) %>%
  # filter(awi == 1)  %>%
  # filter(altm == 1) %>%
  filter(als == 1, awi == 1) %>%
  I()
# 
# 
quantile(e6l$area_ha, probs = seq(0,1,.1))
# 
sort(c6l$area_ha[c6l$altm == 1])
sort(c6l$area_ha[c6l$awi == 1])

e6lID <- e6l %>% as.data.frame() %>%
  filter(altm == 1) 

hu6eLAKES %>% ggplot() + 
  geom_sf(fill = "lightblue") + 
  geom_sf(data = e6l[e6l$altm == 1,], fill = "blue") + 
  geom_sf(data = e6l[e6l$altm == 1 & e6l$awi == 1,], color = "green4", fill = NA) + 
  geom_sf(data = e6l[e6l$altm == 0 & e6l$awi == 1,], fill = "green4") + 
  # geom_sf(data = hu6.c, fill = NA) + 
  geom_sf(data = st_intersection(adkhu10, hu6.e), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.e), fill = NA, lty = 2) + 
  theme_void()



lagos_adk %>% filter(nhdid %in% e6lID$Permanent_) %>% 
  # filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = doc)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~paste(lagosname1, nhdid)) + 
  theme_bw() + 
  labs(x = "date")
lagos_adk %>% filter(nhdid %in% e6lID$Permanent_) %>% 
  filter(year(ymd(sampledate)) > 1990) %>% 
  ggplot(aes(x = ymd(sampledate), y = colort)) + 
  geom_point(aes(group = lagoslakeid)) + 
  geom_smooth(method = "lm", aes(group = lagoslakeid)) + 
  facet_wrap(~lagosname1) + 
  theme_bw() + 
  labs(x = "date")


rd.e <- filter(adklSUM2, Permanent_ %in% hu6eLAKES$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, roaddist1_m:traildist1_m) %>% 
  group_by(Permanent_) %>% 
  mutate(minroad = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>% 
  arrange(minroad) %>% 
  print(n = 130)

named_e <- filter(hu6eLAKES, Permanent_ %in% rd.e$Permanent_[rd.e$minroad < 250]) %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha) %>% arrange(GNIS_Name, lagosname1) %>% 
  unique() %>% 
  filter(!is.na(GNIS_Name) | !is.na(lagosname1)) %>% 
  arrange(area_ha) %>% 
  print(n = 285)

named_e2 <- named_e %>% st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, als, altm, awi, area_ha, lat, lon)

ne3 <- left_join(named_e2, unique(dplyr::select(as.data.frame(lagos_adk), nhdid, maxdepth)), by = c("Permanent_" = "nhdid")) 

ne3 %>% ggplot(aes(x = area_ha, y = maxdepth)) + geom_point()

################################################################################


dim(na3)
dim(nb3)
dim(nc3)
dim(nd3)
dim(ne3)

na3 <- na3 %>% mutate(hu = hu6.a$huc6)
nb3 <- nb3 %>% mutate(hu = hu6.b$huc6)
nc3 <- nc3 %>% mutate(hu = hu6.c$huc6)
nd3 <- nd3 %>% mutate(hu = hu6.d$huc6)
ne3 <- ne3 %>% mutate(hu = hu6.e$huc6)

bind_rows(na3, nb3, nc3, nd3, ne3) %>% dim()

quantile(nb3$area_ha)
quantile(hu6bLAKES$area_ha)

par(mfrow = c(2,1), mar = c(3,4,1,1))
hist(log10(hu6bLAKES$area_ha), freq = FALSE, xlim = c(-2.5, 4), main = "", xlab = "")
hist(log10(nb3$area_ha), freq = FALSE, col = "blue", xlim = c(-2.5, 4), main = "", xlab = "Log10 Area (ha)")


par(mfrow = c(2,1), mar = c(3,4,1,1))
hist(log10(hu6cLAKES$area_ha), freq = FALSE, xlim = c(-3, 4), main = "", xlab = "")
hist(log10(nc3$area_ha), freq = FALSE, col = "blue", xlim = c(-3, 4), main = "", xlab = "Log10 Area (ha)")


par(mfrow = c(2,1), mar = c(3,4,1,1))
hist(log10(hu6dLAKES$area_ha), freq = FALSE, xlim = c(-3.5, 6), main = "", xlab = "")
hist(log10(nd3$area_ha), freq = FALSE, col = "blue", xlim = c(-3.5, 6), main = "", xlab = "Log10 Area (ha)")


par(mfrow = c(2,1), mar = c(3,4,1,1))
hist(log10(hu6eLAKES$area_ha), freq = FALSE, xlim = c(-2, 5), main = "", xlab = "")
hist(log10(ne3$area_ha), freq = FALSE, col = "blue", xlim = c(-2, 5), main = "", xlab = "Log10 Area (ha)")



################################################################################

filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_) %>% 
  ggplot(aes(x = mdep, y = coef_HT)) + geom_point()

filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_) %>% 
  ggplot(aes(x = mdep, y = coef_HT)) + geom_point()

filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_) %>% 
  ggplot(aes(x = mdep, y = coef_HT)) + geom_point()

filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_) %>% 
  ggplot(aes(x = mdep, y = coef_HT)) + geom_point()




pc1b <- filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_) %>%
  dplyr::select(coef_ET:coef_CB, elevation_est:trend) %>% scale() %>% 
  as.matrix() %>% princomp() 
biplot(pc1b)
plot(hclust(dist(pc1b$scores)))
cutree(hclust(dist(pc1b$scores)), 6) %>%  table()

pc1c <- filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_) %>%
  dplyr::select(coef_ET:coef_CB, elevation_est:trend) %>% scale() %>% 
  as.matrix() %>% princomp() 
biplot(pc1c)
plot(hclust(dist(pc1c$scores)))
cutree(hclust(dist(pc1c$scores)), 6) %>%  table()

pc1d <- filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_) %>%
  dplyr::select(coef_ET:coef_CB, elevation_est:trend) %>% scale() %>% 
  as.matrix() %>% princomp() 
biplot(pc1d)
plot(hclust(dist(pc1d$scores)))
cutree(hclust(dist(pc1d$scores)), 6) %>%  table()

pc1e <- filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_) %>%
  dplyr::select(coef_ET:coef_CB, elevation_est:trend) %>% scale() %>% 
  as.matrix() %>% princomp() 
biplot(pc1e)
plot(hclust(dist(pc1e$scores)))
cutree(hclust(dist(pc1e$scores)), 6) %>%  table()






b6stt <- b6l %>% 
  filter(altm == 1 | awi == 1) %>% 
  filter(Permanent_ %in% adktt$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, area_ha) %>% 
  arrange(desc(area_ha))

adktt %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + geom_histogram()

rd.b %>% filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram()

hu6bLAKES %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  left_join(mutate(adktt, Permanent_ = as.character(Permanent_)), by = "Permanent_") %>% 
  ggplot() + geom_sf(aes(fill = coef_SS)) + 
  geom_sf(data = hu6.b, fill = NA) +
  geom_sf(data = st_intersection(adkhu10, hu6.b), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.b), fill = NA, lty = 2) + 
  theme_void()

p1 <- rd.b %>% filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram(data = rd.b, alpha = 0.5) + 
  geom_histogram(fill = "blue") + theme_bw()
p2 <- adktt %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = coef_ET)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p3 <- adktt %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p4 <- adktt %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = coef_SS)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p5 <- adktt %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = coef_CB)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p6 <- adktt %>% 
  filter(Permanent_ %in% b6stt$Permanent_) %>% 
  ggplot(aes(x = trend)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6bLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()

(p1 | p2) / (p3 | p4) / (p5 | p6) 



##############


c6stt <- c6l %>% 
  filter(altm == 1 | awi == 1) %>% 
  filter(Permanent_ %in% adktt$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, area_ha) %>% 
  arrange(desc(area_ha))

adktt %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + geom_histogram()

rd.c %>% filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram()

hu6cLAKES %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  left_join(mutate(adktt, Permanent_ = as.character(Permanent_)), by = "Permanent_") %>% 
  ggplot() + geom_sf(aes(fill = coef_SS)) + 
  geom_sf(data = hu6.c, fill = NA) +
  geom_sf(data = st_intersection(adkhu10, hu6.c), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.c), fill = NA, lty = 2) + 
  theme_void()

p1 <- rd.c %>% filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram(data = rd.c, alpha = 0.5) + 
  geom_histogram(fill = "blue") + theme_bw()
p2 <- adktt %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = coef_ET)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p3 <- adktt %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p4 <- adktt %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = coef_SS)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p5 <- adktt %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = coef_CB)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p6 <- adktt %>% 
  filter(Permanent_ %in% c6stt$Permanent_) %>% 
  ggplot(aes(x = trend)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6cLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()

(p1 | p2) / (p3 | p4) / (p5 | p6) 


##############


d6stt <- d6l %>% 
  filter(altm == 1 | awi == 1) %>% 
  filter(Permanent_ %in% adktt$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, area_ha) %>% 
  arrange(desc(area_ha))

adktt %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + geom_histogram()

rd.d %>% filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram()

hu6dLAKES %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  left_join(mutate(adktt, Permanent_ = as.character(Permanent_)), by = "Permanent_") %>% 
  ggplot() + geom_sf(aes(fill = coef_SS)) + 
  geom_sf(data = hu6.d, fill = NA) +
  geom_sf(data = st_intersection(adkhu10, hu6.d), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.d), fill = NA, lty = 2) + 
  theme_void()

p1 <- rd.d %>% filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram(data = rd.d, alpha = 0.5) + 
  geom_histogram(fill = "blue") + theme_bw()
p2 <- adktt %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = coef_ET)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p3 <- adktt %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p4 <- adktt %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = coef_SS)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p5 <- adktt %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = coef_CB)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p6 <- adktt %>% 
  filter(Permanent_ %in% d6stt$Permanent_) %>% 
  ggplot(aes(x = trend)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6dLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()

(p1 | p2) / (p3 | p4) / (p5 | p6) 


##############


e6stt <- e6l %>% 
  filter(altm == 1 | awi == 1) %>% 
  filter(Permanent_ %in% adktt$Permanent_) %>% 
  as.data.frame() %>% 
  dplyr::select(Permanent_, GNIS_Name, lagosname1, area_ha) %>% 
  arrange(desc(area_ha))

adktt %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + geom_histogram()

rd.e %>% filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram()

hu6eLAKES %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  left_join(mutate(adktt, Permanent_ = as.character(Permanent_)), by = "Permanent_") %>% 
  ggplot() + geom_sf(aes(fill = coef_SS)) + 
  geom_sf(data = hu6.e, fill = NA) +
  geom_sf(data = st_intersection(adkhu10, hu6.e), fill = NA) + 
  geom_sf(data = st_intersection(adkhu12, hu6.e), fill = NA, lty = 2) + 
  theme_void()

p1 <- rd.e %>% filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = minroad)) + geom_histogram(data = rd.e, alpha = 0.5) + 
  geom_histogram(fill = "blue") + theme_bw()
p2 <- adktt %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = coef_ET)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p3 <- adktt %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = coef_HT)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p4 <- adktt %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = coef_SS)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p5 <- adktt %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = coef_CB)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()
p6 <- adktt %>% 
  filter(Permanent_ %in% e6stt$Permanent_) %>% 
  ggplot(aes(x = trend)) + 
  geom_histogram(data = filter(adktt, Permanent_ %in% hu6eLAKES$Permanent_), alpha = 0.5) + 
  geom_histogram(fill = "blue") + 
  theme_bw()

(p1 | p2) / (p3 | p4) / (p5 | p6) 


####

