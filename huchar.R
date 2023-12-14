library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)

################################################################################
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


# NLCD
# nlcdd <- arrow::read_parquet("data/landcover.parquet")
nlcd3 <- readRDS("data/adk_nlcd3.rds")

# ERA5
# era5 <- arrow::read_parquet("data/era5_adk_1992-2022.parquet")

# NLDAS
# nldas <- read.csv("data/ADK_Data/Data/nldas_adk_monthly.csv")


################################################################################

sf_use_s2(FALSE)
################################################################################
# NHD HU06
adkhu06 <- readRDS("nhd/adk_nhd_wb06_adk.rds")

hu6lcdf <- hulcdf(adkhu06, nlcd3)

hu6lcdf <- do.call(rbind, hu6lcdf) %>% 
  mutate(huc6 = adkhu06$huc6)

hu6eldf <- huelevdf(adkhu06, adkel)
hu6eldf <- do.call(rbind, hu6eldf) %>% 
  mutate(huc6 = adkhu06$huc6)

hu6lsdf <- hulake(adkhu06, adklakeSUMMARY)


adk6char <- left_join(adkhu06, hu6lcdf, by = "huc6") %>% 
  left_join(hu6eldf, by = "huc6") %>% 
  left_join(hu6lsdf, by = "name")
# 
# adkpc6 <- dplyr::select(as.data.frame(adk6char), developed_open_space:developed_high_intensity) %>% 
#   as.matrix() %>% 
#   princomp()
# biplot(adkpc6)
# 
# ggplot(adk6char, aes(fill = nlakes)) + geom_sf() + theme_void()

# NHD HU08

adkhu08 <- readRDS("nhd/adk_nhd_wb08_adk.rds")

hu8lcdf <- hulcdf(adkhu08, nlcd3)

hu8lcdf <- do.call(rbind, hu8lcdf) %>% 
  mutate(huc8 = adkhu08$huc8)

hu8eldf <- huelevdf(adkhu08, adkel)
hu8eldf <- do.call(rbind, hu8eldf) %>% 
  mutate(huc8 = adkhu08$huc8)


hu8lsdf <- hulake(adkhu08, adklakeSUMMARY)


adk8char <- left_join(adkhu08, hu8lcdf, by = "huc8") %>% 
  left_join(hu8eldf, by = "huc8") %>% 
  left_join(hu8lsdf, by = "name")
# 
# 
# ggplot(adk8char, aes(fill = nlakes/sum(nlakes))) + geom_sf() + theme_void() 
# 
# 
# adkpc8 <- dplyr::select(as.data.frame(adk8char), developed_open_space:developed_high_intensity, ends_with("forest")) %>% 
#   as.matrix() %>% 
#   princomp()
# biplot(adkpc8)
# 
# 
# adkpc8 <- dplyr::select(as.data.frame(adk8char), 
#                         ends_with("depth"), ends_with("area"), 
#                         mean_lake_median_tp:mean_lake_median_tp, 
#                         mean_lake_median_doc, 
#                         mean_lake_median_chla, -AREA, -shape_Area) %>%
#   filter(!is.nan(mean_lake_median_chla)) %>% 
#   as.matrix() %>% scale() %>% 
#   princomp()


# NHD HU10

adkhu10 <- readRDS("nhd/adk_nhd_wb10_adk.rds")

hu10lcdf <- hulcdf(adkhu10, nlcd3)

hu10lcdf <- bind_rows(hu10lcdf) %>% 
  mutate(huc10 = adkhu10$huc10)

hu10eldf <- huelevdf(adkhu10, adkel)
hu10eldf <- bind_rows(hu10eldf) %>% 
  mutate(huc10 = adkhu10$huc10)

hu10lsdf <- hulake(adkhu10, adklakeSUMMARY)


adk10char <- left_join(adkhu10, hu10lcdf, by = "huc10") %>% 
  left_join(hu10eldf, by = "huc10") %>% 
  left_join(hu10lsdf, by = "name")
# 
# ggplot(adk12char, aes(fill = nlakes)) + geom_sf() + theme_void() |
# ggplot(adk12char, aes(fill = tlaksam)) + geom_sf() + theme_void() |
# ggplot(adk12char, aes(fill = tlaksam/nlakes)) + geom_sf() + theme_void()
# 
# adkpc10 <- dplyr::select(as.data.frame(adk10char), 
#                          deciduous_forest:emergent_herbaceous_wetlands,
#                          ends_with("forest")) %>% 
#   mutate_all(zoo::na.fill, fill = 0) %>% 
#   as.matrix() %>% 
#   princomp()
# biplot(adkpc10, choices = 2:3)
# 
# 
# adkpc10 <- dplyr::select(as.data.frame(adk10char), 
#                         ends_with("depth"), ends_with("area"), 
#                         mean_lake_median_tp:mean_lake_median_tp, 
#                         mean_lake_median_doc, 
#                         mean_lake_median_chla, -AREA, -shape_Area) %>%
#   filter(!is.na(mean_lake_median_chla)) %>% 
#   as.matrix() %>% scale() %>% 
#   princomp()
# biplot(adkpc10)
# 
# 
# ggplot(adk8char) + geom_sf(aes(fill = tlaksam)) + 
#   geom_sf(data = st_zm(adklakeSUMMARY), fill = NA, color = "black") + 
#   theme_void() + 
#   labs(fill = "# lakes")
# 
# ggplot(adkalt2) + geom_sf() + 
#   geom_point(data = mutate(adk80s, DATE = mdy(DATE)), aes(x = -LONG_NAD83DD, y = LAT_NAD83DD)) + 
#   theme_void() + facet_grid(month(DATE) ~ year(DATE))
# 



# NHD HU12

adkhu12 <- readRDS("nhd/adk_nhd_wb12_adk.rds")

hu12lcdf <- hulcdf(adkhu12, nlcd3)

hu12lcdf <- bind_rows(hu12lcdf) %>% 
  mutate(huc12 = adkhu12$huc12)

hu12eldf <- huelevdf(adkhu12, adkel)
hu12eldf <- bind_rows(hu12eldf) %>% 
  mutate(huc12 = adkhu12$huc12)

hu12lsdf <- hulake(adkhu12, adklakeSUMMARY)


adk12char <- left_join(adkhu12, hu12lcdf, by = "huc12") %>% 
  left_join(hu12eldf, by = "huc12") %>% 
  left_join(hu12lsdf, by = "name")
# 
# ggplot(adk12char, aes(fill = nlakes)) + geom_sf() + theme_void()
# ggplot(adk12char, aes(fill = tlaksam/nlakes)) + geom_sf() + theme_void()
# 
# 
# ggplot(adk10char, aes(fill = mean_lake_median_chla)) + geom_sf() + theme_void()
# ggplot(adk10char, aes(fill = mean_lake_median_tp)) + geom_sf() + theme_void()
# ggplot(adk10char, aes(fill = mean_lake_median_doc)) + geom_sf() + theme_void()
# 
# ggplot(adk10char, aes(x = mean_lake_median_tp, y = mean_lake_median_chla)) + geom_point()
# ggplot(adk10char, aes(x = mean_lake_median_tp, y = mean_lake_median_chla)) + geom_point()
# 
# ggplot(filter(adklakeSUMMARY, !is.na(meanvalue_chla)), 
#        aes(x = meanvalue_tp, y = meanvalue_chla)) + geom_point() 
# ggplot(filter(adklakeSUMMARY, !is.na(meanvalue_chla)), 
#        aes(x = meanvalue_doc, y = meanvalue_chla)) + geom_point() 
# 
# adklakeSUMMARY %>% as.data.frame() %>% 
#   select(meanvalue_chla, meanvalue_doc, meanvalue_tp, meanvalue_tn) %>%
#   # filter(!is.na(meanvalue_chla)) %>% 
#   GGally::ggpairs()
# 
# adklakeSUMMARY %>% as.data.frame() %>% 
#   select(medianvalue_chla, medianvalue_doc, medianvalue_tp, medianvalue_tn) %>%
#   # filter(!is.na(meanvalue_chla)) %>% 
#   GGally::ggpairs()
# 
# adklakeSUMMARY %>% as.data.frame() %>% 
#   select(n_chla, n_doc, n_tp, n_tn,medianvalue_chla, medianvalue_doc, medianvalue_tp, medianvalue_tn) %>% 
#   filter(!is.na(n_chla) | !is.na(n_doc) | !is.na(n_tp) | !is.na(n_tn)) %>% 
#   ggplot(aes(x = n_tn)) + geom_histogram() + theme_bw()

################################################################################
als_morpho <- read_excel("data/ALS84/REC3_DATA_TABLE.xls")

# a8 <- dplyr::select(filter(adk80s, year(mdy(DATE)) == 1984,month(mdy(DATE)) == 7), SO4:SCONDUCT)
a8 <- dplyr::select(adk80s, SO4:SCONDUCT)
biplot(princomp(scale(a8[complete.cases(a8),])))

adk80s2 <- mutate(adk80s, PONDNO = stringr::str_pad(POND, 6, "left", "0")) %>% 
  left_join(als_morpho, by = "PONDNO") #%>% 
  # filter(year(mdy(DATE)) == 1984, month(mdy(DATE)) == 7)

dm1 <- dist(a8[complete.cases(a8),])
dg1 <- dist(adk80s2[complete.cases(a8), c("LONG_NAD83DD", "LAT_NAD83DD", "ELEV")])
dg2 <- dist(adk80s2[complete.cases(a8), c("WAREA", "SAREA", "VOLUME", "MAXDEPTH", "MEANDEPTH", "SHORE", "LAREA", "RUNOFF", "FRATE", "WASA", "VOLD")])
dg3 <- dist(adk80s2[complete.cases(a8), c("LONG_NAD83DD", "LAT_NAD83DD", "ELEV", "WAREA", "SAREA", "VOLUME", "MAXDEPTH", "MEANDEPTH", "SHORE", "LAREA", "RUNOFF", "FRATE", "WASA", "VOLD")])

plot(as.matrix(dm1)[lower.tri(as.matrix(dm1))] ~ as.matrix(dg1)[lower.tri(as.matrix(dg1))])
plot(as.matrix(dm1)[lower.tri(as.matrix(dm1))] ~ as.matrix(dg2)[lower.tri(as.matrix(dg2))])
plot(log(as.matrix(dm1)[lower.tri(as.matrix(dm1))]) ~ log(as.matrix(dg3)[lower.tri(as.matrix(dg3))]))

pheatmap::pheatmap(cor(a8, use = "pairwise.complete.obs"))
pheatmap::pheatmap(cor(dplyr::select(as.data.frame(lagos_adk), doc, tp, chla, secchi, 
                                     tn, colort,nh4), 
                       use = "pairwise.complete.obs")) 
################################################################################

hist(adk80s$DOC)
points(x = adklakeSUMMARY$meanvalue_doc[adklakeSUMMARY$altm == 1], y  = rep(200, 55))
points(x = adklakeSUMMARY$maxvalue_doc[adklakeSUMMARY$altm == 1], y  = rep(150, 55))
points(x = adklakeSUMMARY$meanvalue_doc[adklakeSUMMARY$aeap == 1], y  = rep(350, 29))
points(x = adklakeSUMMARY$maxvalue_doc[adklakeSUMMARY$aeap == 1], y  = rep(300, 29))
points(x = adklakeSUMMARY$meanvalue_doc[adklakeSUMMARY$ny_cslap == 1], y  = rep(500, 63))
points(x = adklakeSUMMARY$maxvalue_doc[adklakeSUMMARY$ny_cslap == 1], y  = rep(450, 63))
text(x = 25, y = 325, "AEAP")
text(x = 25, y = 175, "ALTM")
text(x = 25, y = 475, "CSLAP")

ggplot(adk80s, aes(x = DOC)) + stat_ecdf() + 
  stat_ecdf(data = filter(adklakeSUMMARY, aeap == 1), aes(x = meanvalue_doc), color = "blue") +
  stat_ecdf(data = filter(adklakeSUMMARY, aeap == 1), aes(x = maxvalue_doc), color = "blue", lty = 2) + 
  stat_ecdf(data = filter(adklakeSUMMARY, altm == 1), aes(x = meanvalue_doc), color = "green4") +
  stat_ecdf(data = filter(adklakeSUMMARY, altm == 1), aes(x = maxvalue_doc), color = "green4", lty = 2) + 
  stat_ecdf(data = filter(adklakeSUMMARY, ny_cslap == 1), aes(x = meanvalue_doc), color = "chocolate4") +
  stat_ecdf(data = filter(adklakeSUMMARY, ny_cslap == 1), aes(x = maxvalue_doc), color = "chocolate4", lty = 2) + 
  theme_bw()

# hist(adk80s$TOTAL_P2*1000)
hist(log10(adklakeSUMMARY$meanvalue_tp[adklakeSUMMARY$als == 1]))
points(x = log10(adklakeSUMMARY$meanvalue_tp[adklakeSUMMARY$altm == 1]), y  = rep(250, 55))
points(x = log10(adklakeSUMMARY$meanvalue_tp[adklakeSUMMARY$aeap == 1]), y  = rep(150, 29))
points(x = log10(adklakeSUMMARY$maxvalue_tp[adklakeSUMMARY$altm == 1]), y  = rep(300, 55))
points(x = log10(adklakeSUMMARY$maxvalue_tp[adklakeSUMMARY$aeap == 1]), y  = rep(100, 29))

text(x = 2, y = 150, "AEAP")
text(x = 2, y = 250, "ALTM")



ggplot(adk80s, aes(x = TOTAL_P2 * 1000)) + stat_ecdf() + 
  stat_ecdf(data = filter(adklakeSUMMARY, aeap == 1), aes(x = meanvalue_tp), color = "blue") +
  stat_ecdf(data = filter(adklakeSUMMARY, aeap == 1), aes(x = maxvalue_tp), color = "blue", lty = 2) + 
  stat_ecdf(data = filter(adklakeSUMMARY, altm == 1), aes(x = meanvalue_tp), color = "green4") +
  stat_ecdf(data = filter(adklakeSUMMARY, altm == 1), aes(x = maxvalue_tp), color = "green4", lty = 2) + 
  stat_ecdf(data = filter(adklakeSUMMARY, ny_cslap == 1), aes(x = meanvalue_tp), color = "chocolate4") +
  stat_ecdf(data = filter(adklakeSUMMARY, ny_cslap == 1), aes(x = maxvalue_tp), color = "chocolate4", lty = 2) + 
  scale_x_log10() + 
  theme_bw() + 
  labs(x = "TP")


hist((adklakeSUMMARY$meanvalue_chla[adklakeSUMMARY$als == 1]))
points(x = adklakeSUMMARY$meanvalue_chla[adklakeSUMMARY$altm == 1], y  = rep(60, 55))
points(x = adklakeSUMMARY$meanvalue_chla[adklakeSUMMARY$aeap == 1], y  = rep(20, 29))
points(x = adklakeSUMMARY$maxvalue_chla[adklakeSUMMARY$altm == 1], y  = rep(70, 55))
points(x = adklakeSUMMARY$maxvalue_chla[adklakeSUMMARY$aeap == 1], y  = rep(10, 29))
text(x = 22, y = 20, "AEAP")
text(x = 22, y = 60, "ALTM")


ggplot(filter(adklakeSUMMARY, als == 1), aes(x = meanvalue_chla)) + stat_ecdf() + 
  stat_ecdf(data = filter(adklakeSUMMARY, aeap == 1), aes(x = meanvalue_chla), color = "blue") +
  stat_ecdf(data = filter(adklakeSUMMARY, aeap == 1), aes(x = maxvalue_chla), color = "blue", lty = 2) + 
  stat_ecdf(data = filter(adklakeSUMMARY, altm == 1), aes(x = meanvalue_chla), color = "green4") +
  stat_ecdf(data = filter(adklakeSUMMARY, altm == 1), aes(x = maxvalue_chla), color = "green4", lty = 2) + 
  stat_ecdf(data = filter(adklakeSUMMARY, ny_cslap == 1), aes(x = meanvalue_chla), color = "chocolate4") +
  stat_ecdf(data = filter(adklakeSUMMARY, ny_cslap == 1), aes(x = maxvalue_chla), color = "chocolate4", lty = 2) + 
  scale_x_log10() + 
  theme_bw() + 
  labs(x = "Chla")


library(adklakedata)
nut <- adk_data("nutr")
chem <- adk_data("chem")

ggplot(nut, aes(x = ymd(date), y = chl.a.ug.L)) + geom_point() + 
  facet_wrap(~lake.name, scales = "free_y") + 
  labs(x = "Date", y = "Chl a") + 
  theme_bw()
ggplot(chem, aes(x = ymd(date), y = DOC)) + geom_point() + 
  facet_wrap(~lake.name, scales = "free_y") + 
  labs(x = "Date", y = "DOC") + 
  theme_bw()


aeapCHLAtrnd <- nut %>% group_by(lake.name) %>% 
  tidyr::nest() %>% 
  mutate(fitCHLA = purrr::map(data, function(x) lm(chl.a.ug.L ~ year(ymd(date)), data = x))) %>% 
  mutate(pCHLA = purrr::map_dbl(fitCHLA, function(x) summary(x)$coef[2,4]),
         sCHLA = purrr::map_dbl(fitCHLA, function(x) coef(x)[2]))

aeapDOCtrnd <- chem %>% group_by(lake.name) %>% 
  tidyr::nest() %>% 
  mutate(fitDOC = purrr::map(data, function(x) lm(DOC ~ year(ymd(date)), data = x))) %>% 
  mutate(pDOC = purrr::map_dbl(fitDOC, function(x) summary(x)$coef[2,4]),
         sDOC = purrr::map_dbl(fitDOC, function(x) coef(x)[2]))


hist(c(lagTRND$sDOC[lagTRND$pDOC <= 0.05], aeapDOCtrnd$sDOC[aeapDOCtrnd$pDOC <= 0.05]), 
     main = "Linear DOC Trend", xlab = "Trend", ylab = "Freq")
length(c(lagTRND$sDOC[lagTRND$pDOC <= 0.05], aeapDOCtrnd$sDOC[aeapDOCtrnd$pDOC <= 0.05]))
nrow(lagTRND) + nrow(aeapDOCtrnd)

hist(c(lagTRND$sCHLA[lagTRND$pCHLA <= 0.05], aeapCHLAtrnd$sCHLA[aeapCHLAtrnd$pCHLA <= 0.05]), 
     main = "Linear CHLA Trend", xlab = "Trend", ylab = "Freq")
length(c(lagTRND$sCHLA[lagTRND$pCHLA <= 0.05], aeapCHLAtrnd$sCHLA[aeapCHLAtrnd$pCHLA <= 0.05]))
nrow(lagTRND) + nrow(aeapCHLAtrnd)


sf_use_s2(FALSE)


pilotlakes <- read.csv("data/SCALE_pilot.csv")

alsp <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$als == 1,])) + 
  theme_void() + 
  labs(title = "ALS")

altmp <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$altm == 1,])) + 
  theme_void() + 
  labs(title = "ALTM")

aeapp <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$aeap == 1,])) + 
  theme_void() + 
  labs(title = "AEAP")

cslapp <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$ny_cslap == 1,])) + 
  theme_void() + 
  labs(title = "CSLAP")

lcip <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$nylci == 1,])) + 
  theme_void() + 
  labs(title = "NY LCI")

amapp <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$emap == 1,])) + 
  theme_void() + 
  labs(title = "EMAP")

elsp <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$els == 1,])) + 
  theme_void() + 
  labs(title = "ELS")

timep <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$epa_time == 1,])) + 
  theme_void() + 
  labs(title = "TIME")

awip <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[adklakeSUMMARY$awi == 1,])) + 
  theme_void() + 
  labs(title = "AWI")

pilot <- ggplot(adkalt2) + geom_sf() +
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake),])) + 
  theme_void() + 
  labs(title = "Pilot Candidates")


(alsp | altmp | aeapp |cslapp | awip) / (lcip | amapp | elsp | timep | pilot)



p.boat <- ggplot(adkalt2) + geom_sf(fill = NA) +
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake),]), color = "grey40", size = 1) + 
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake) & adklakeSUMMARY$Boat == "X",]), color = "blue", size = 2) + 
  theme_void() + 
  labs(title = "Boat Access")

p.dom <- ggplot(adkalt2) + geom_sf(fill = NA) +
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake),]), color = "grey40", size = 1) + 
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake) & adklakeSUMMARY$DOMsampling == "X",]), color = "blue", size = 2) + 
  theme_void() + 
  labs(title = "DOM")

p.dna <- ggplot(adkalt2) + geom_sf(fill = NA) +
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake),]), color = "grey40", size = 1) + 
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake) & adklakeSUMMARY$eDNA == "X",]), color = "blue", size = 2) + 
  theme_void() + 
  labs(title = "eDNA")

p.remote <- ggplot(adkalt2) + geom_sf(fill = NA) +
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake),]), color = "grey40", size = 1) + 
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake) & adklakeSUMMARY$RemoteSense == "X",]), color = "blue", size = 2) + 
  theme_void() + 
  labs(title = "Remote Sensing")


p.iso <- ggplot(adkalt2) + geom_sf(fill = NA) +
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake),]), color = "grey40", size = 1) + 
  geom_sf(data = st_centroid(adklakeSUMMARY[!is.na(adklakeSUMMARY$Lake) & adklakeSUMMARY$Isotop == "X",]), color = "blue", size = 2) + 
  theme_void() + 
  labs(title = "Isotopes")


p.boat | p.dom | p.dna | p.remote | p.iso

# NHD HU12


################################################################################
srcprog <- read.csv("nhd/sourceprogram10873.csv")
filter(srcprog, programid %in% unique(lagos_adk$programid))

samprog <- adklakeSUMMARY %>% 
  select(Permanent_, lagosname1, aeap:awi) %>% 
  mutate(total = (aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi)) %>% 
  filter(total != 0) %>% unique() %>%  
  arrange(desc(total)) 

samprog2 <- adklakeSUMMARY %>% as.data.frame() %>%  
  select(Permanent_, lagosname1, aeap:awi) %>% 
  mutate(total = (aeap + als + epa_time + ny_cslap + els + emap + nylci + altm + awi)) %>% 
  filter(total != 0) %>% unique() %>%  
  arrange(desc(total)) 

# 1409 lakes have been sampled across 9 sampling programs

alsum <- adklakeSUMMARY %>% 
  filter(Permanent_ %in% samprog$Permanent_) %>%
  select(Permanent_, PONDNO, area_ha, elevation_est, best_maxdepth:Depth_avg, 
         trend, minvalue_doc, meanvalue_doc, maxvalue_doc, n_doc, strt_doc, end_doc,
         minvalue_secchi, meanvalue_secchi, maxvalue_secchi) %>% 
  mutate(pt = st_centroid(geometry)) %>%
  mutate(long = unlist(purrr::map(pt,1)), lat = unlist(purrr::map(pt,2))) %>%
  unique()

filter(samprog, total > 1) %>% 
  ggplot() + geom_sf(aes(fill = factor(total))) + 
  geom_sf(data = adkalt2, fill = NA)


lagos_adk %>% group_by(lagoslakeid) %>% 
  summarize(n = n()) %>% arrange(desc(n))


alsdep <- als_morpho %>% select(PONDNO, ELEV, SAREA, MAXDEPTH, MEANDEPTH)
test <- left_join(alsum, alsdep, by = "PONDNO") %>% left_join(samprog2, by = "Permanent_")
test$ELEV
table(test$als, is.na(test$ELEV))

test
summary(test)

test2 <- filter(test, best_maxdepth >= 4 | MAXDEPTH >= 4)
test3 <- test2 %>% as.data.frame() %>%
  select(Permanent_, PONDNO, total, area_ha, elevation_est, lat, long,
         trend, meanvalue_doc, n_doc) 

depth_mean_t2 <- rowMeans(as.data.frame(test2)[,c("best_meandepth","Depth_avg", "MEANDEPTH")], na.rm = TRUE)
depth_max_t2 <- rowMeans(as.data.frame(test2)[,c("best_maxdepth", "MAXDEPTH")], na.rm = TRUE)

test3$depth_mean <- depth_mean_t2
test3$depth_max <- depth_max_t2

filter(lagos_adk, nhdid %in% test3$Permanent_) %>% 
  select(nhdid, sampledate, doc) %>% 
  filter(!is.na(doc)) %>% 
  mutate(sampledate = ymd(sampledate)) %>% 
  group_by(nhdid) %>% 
  nest() %>% 
  mutate(n = purrr::map_dbl(data, nrow)) %>% 
  filter(n > 3) %>% 
  mutate(fit = purrr::map(data, function(x){lm(doc ~ year(sampledate), data = x)}))

lagosDOCstrt <- filter(lagos_adk, nhdid %in% test3$Permanent_) %>% 
  as.data.frame() %>% 
  select(nhdid, sampledate, doc) %>% 
  filter(!is.na(doc)) %>% 
  mutate(sampledate = ymd(sampledate)) %>% 
  group_by(nhdid) %>% 
  filter(sampledate == min(sampledate))




adk80s <- read.csv("data/NY_ADIRONDACK_1984.csv")
filter(adklakeSUMMARY, !is.na(trend), als == 1)

adk80s$PONDNO <- stringr::str_pad(adk80s$POND, 6, "left", "0")

als_phy2 <- filter(als_physic, !is.na(SECCHI)) %>% 
  group_by(PONDNO) %>% summarize(SECCHI = mean(SECCHI))

adkchar1 <- left_join(adklakeSUMMARY, adk80s, by = "PONDNO") %>% 
  select(-trend, -p.value) %>% 
  left_join(select(test, lagoslakeid, trend, p.value), by = c("lagoslakeid.x" = "lagoslakeid")) %>% 
  filter(!is.na(trend)) %>% 
  left_join(als_morpho, by = "PONDNO") %>% 
  left_join(als_phy2, by = "PONDNO") %>%
  select(Permanent_, PONDNO, GNIS_Name, elevation_est, ELEV,
         best_maxdepth, MaxDepth, MAXDEPTH, 
         LAREA, SAREA, area_ha,
         best_meandepth, MEANDEPTH, Depth_avg,
         DOC, SECCHI, meanvalue_doc, trend, p.value)

plot((1.7/adkchar1$SECCHI), 0.15*adkchar1$DOC^1.08)
abline(a = 0, b = 1)

plot(adkchar1$trend[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5] ~ adkchar1$best_maxdepth[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5])
abline(lm(adkchar1$trend[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5] ~ adkchar1$best_maxdepth[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5]))
summary(lm(adkchar1$trend[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5] ~ adkchar1$best_maxdepth[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5]))

atest <- adkchar1[adkchar1$p.value < 0.05 & adkchar1$best_maxdepth >= 5 & adkchar1$best_maxdepth < 40,]

summary(lm(trend ~ best_maxdepth, data = atest))

adkchar1 %>% 
  filter(p.value < 0.05, best_maxdepth >= 5, best_maxdepth < 40) %>% 
  ggplot(aes(x = SECCHI, y = trend)) + geom_point() + 
  geom_smooth(method = "lm")

lag1 <- lagos_adk %>% as.data.frame() %>%
  group_by(nhdid) %>% filter(!is.na(doc)) %>%
  filter(sampledate == min(sampledate)) %>%
  select(nhdid, sampledate, doc, secchi)


adkchar2 <- adkchar1 %>% 
  mutate(mdep = coalesce(MAXDEPTH, as.numeric(MaxDepth)), 
         mdep = case_when(is.na(mdep) ~ best_maxdepth, 
                          TRUE ~ mdep)) %>% 
  select(-best_maxdepth, -MaxDepth, -MAXDEPTH, -ELEV, -SAREA, -LAREA) %>% 
  mutate(meandep = case_when(!is.na(MEANDEPTH) ~ MEANDEPTH,
                             !is.na(best_meandepth) & is.na(MEANDEPTH) & !is.na(Depth_avg) ~ (best_meandepth),
                             !is.na(best_meandepth) & is.na(MEANDEPTH) & is.na(Depth_avg) ~ best_meandepth)) %>% 
  select(-best_meandepth, -MEANDEPTH, -Depth_avg) %>% 
  left_join(lag1, by = c("Permanent_" = "nhdid")) %>% 
  select(Permanent_:area_ha, mdep, meandep, DOC, doc, meanvalue_doc, SECCHI, secchi, trend)


plot(adkchar2$DOC ~ adkchar2$doc)
max(0.15*adkchar1$DOC^1.08,na.rm = T)

adkchar3 <- adkchar2 %>% 
  mutate(sec = coalesce(SECCHI, secchi)) %>% 
  filter(!is.na(sec)) %>% 
  filter(mdep >= 5, mdep > meandep) %>% 
  unique() 
coords <- st_coordinates(st_centroid(adkchar3))
  
adkchar3 %>% as.data.frame() %>% 
  select(Permanent_, elevation_est, area_ha, mdep, meandep, sec, trend) %>% 
  mutate(lat = coords[,2], lon = coords[,1]) %>% 
  write.csv("adk_lakes4sim.csv")



# roads 

roads <- st_read("C:/Users/borre/Downloads/TRAN_New_York_State_Shape/Shape/",
                 "Trans_RoadSegment_0")
roads2 <- st_read("C:/Users/borre/Downloads/TRAN_New_York_State_Shape/Shape/",
                 "Trans_RoadSegment_1")

trail1 <- st_read("C:/Users/borre/Downloads/TRAN_New_York_State_Shape/Shape/",
                  "Trans_TrailSegment")

adk_roads <- st_intersection(st_transform(roads, "WGS84"), adkalt2)
adk_roads2 <- st_intersection(st_transform(roads2, "WGS84"), adkalt2)
adk_trails <- st_intersection(st_transform(trail1, "WGS84"), adkalt2)

ggplot(adkalt2) + geom_sf(fill = NA) + 
  geom_sf(data = st_zm(adklakeSUMMARY), fill = "blue", color = "blue") + 
  geom_sf(data = adk_roads) + 
  geom_sf(data = adk_roads2, color = "grey40") + 
  geom_sf(data = adk_trails, color = "green4") + 
  theme_minimal()

# rdist <- st_distance(adk_roads, adklakeSUMMARY)


sf_use_s2(FALSE)
lakesdf <- adklakeSUMMARY %>% dplyr::select(Permanent_, geometry) %>% unique() %>% st_zm()

dflist <- list()
for(i in 1:nrow(lakesdf)){
  templake <- lakesdf[i, ] %>% st_transform("EPSG:3857")
  buff <- st_buffer(templake, 2000)
  buffLC <- st_transform(buff, st_crs(adk_roads))
  rds1 <- st_intersection(adk_roads, buffLC)
  rds2 <- st_intersection(adk_roads2, buffLC)
  tr1 <- st_intersection(adk_trails, buffLC)
  d1 <- st_distance(st_transform(templake, st_crs(rds1)), rds1)
  d2 <- st_distance(st_transform(templake, st_crs(rds2)), rds2)
  d3 <- st_distance(st_transform(templake, st_crs(tr1)), tr1)
  
  dflist[[i]] <- data.frame(r1 = min(d1), r2 = min(d2), t1 = min(d3))
  print(i)
}


ggplot(buffLC) + geom_sf() + 
  geom_sf(data = st_transform(templake, st_crs(adk_roads))) +
  geom_sf(data = tr1, color = "green4") + geom_sf(data = rds1) + geom_sf(data = rds2)


hist(data.table::rbindlist(dflist)$r1[!is.infinite(data.table::rbindlist(dflist)$r1)])


d_rnt <- data.table::rbindlist(dflist) %>% 
  mutate(Permanent_ = lakesdf$Permanent_, roaddist1_m = as.vector(r1),
         roaddist2_m = as.vector(r2), traildist1_m = as.vector(t1)) %>% 
  select(Permanent_:traildist1_m) %>% 
  mutate(roaddist1_m = case_when(is.infinite(roaddist1_m) ~ NA, TRUE ~ roaddist1_m),
         roaddist2_m = case_when(is.infinite(roaddist2_m) ~ NA, TRUE ~ roaddist2_m),
         traildist1_m = case_when(is.infinite(traildist1_m) ~ NA, TRUE ~ traildist1_m)) %>%
  arrange((roaddist1_m))# %>% filter(roaddist1_m < 50)

left_join(adklakeSUMMARY, d_rnt, by = "Permanent_")

d_rnt
# write.csv(d_rnt, "data/roadNtrail_dist.csv")


lakes1 <- left_join(adklakeSUMMARY, d_rnt, by = "Permanent_") %>% 
  select(Permanent_, PONDNO, lagosname1, area_ha, elevation_est, Shore_len, 
         Shore_dev, Wshd_area, best_maxdepth, best_meandepth, aeap:awi, 
         roaddist1_m, roaddist2_m, traildist1_m) %>% unique()
sum(is.na(lakes1$Wshd_area))

n2r <- d_rnt %>% 
  filter(!is.na(roaddist1_m) & !is.na(roaddist2_m),
         roaddist1_m <= 100 | roaddist2_m <= 100)

filter(st_zm(adklakeSUMMARY), Permanent_ %in% n2r$Permanent_) %>% ggplot() + geom_sf()


left_join(adklakeSUMMARY, d_rnt, by = "Permanent_") %>% 
  filter(!is.na(roaddist1_m) & !is.na(roaddist2_m)) %>% 
  mutate(minrd = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>%
  filter(minrd <= 100) %>% 
  ggplot(aes(x = log10(area_ha), y = minrd)) + geom_point()

left_join(adklakeSUMMARY, d_rnt, by = "Permanent_") %>% 
  filter(!is.na(roaddist1_m) & !is.na(roaddist2_m)) %>% 
  mutate(minrd = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>% 
  ggplot(aes(x = (elevation_est), y = minrd)) + geom_point()


left_join(adklakeSUMMARY, d_rnt, by = "Permanent_") %>% 
  # filter(!is.na(roaddist1_m) & !is.na(roaddist2_m)) %>% 
  mutate(minrd = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>%
  mutate(close = !is.na(roaddist1_m) & !is.na(roaddist2_m)) %>%  
  ggplot(aes(x = close, y = log10(area_ha))) + geom_violin() + 
  geom_boxplot(fill = NA, width = 0.3) 

left_join(adklakeSUMMARY, d_rnt, by = "Permanent_") %>% 
  # filter(!is.na(roaddist1_m) & !is.na(roaddist2_m)) %>% 
  mutate(minrd = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>%
  mutate(close = !is.na(roaddist1_m) & !is.na(roaddist2_m)) %>%  
  ggplot(aes(x = close, y = log10(area_ha))) + geom_violin(fill = "grey40") + 
  ggforce::geom_sina(method = "counts", size = 0.5, alpha = 0.5) + 
  geom_boxplot(fill = NA, width = 0.3, color = "skyblue") + 
  theme_bw()


left_join(adklakeSUMMARY, d_rnt, by = "Permanent_") %>% 
  # filter(!is.na(roaddist1_m) & !is.na(roaddist2_m)) %>% 
  mutate(minrd = min(roaddist1_m, roaddist2_m, na.rm = TRUE)) %>%
  mutate(close = !is.na(roaddist1_m) & !is.na(roaddist2_m)) %>%  
  ggplot(aes(x = close, y = log10(area_ha))) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = .2)


pno <- filter(adklakeSUMMARY, !is.na(PONDNO)) %>% 
  as.data.frame() %>% dplyr::select(Permanent_, PONDNO) %>% unique()

g <- ggplot() + geom_sf(data = adkalt2) + 
  geom_sf(data = st_zm(adklakeSUMMARY)) + 
  geom_sf(data = filter(adklakeSUMMARY, !is.na(PONDNO)), aes(fill = as.numeric(PONDNO))) +
  geom_point(data = filter(adk80s_A, !PONDNO %in% pno$PONDNO), 
             aes(x = -LONG_NAD83DD, y = LAT_NAD83DD, fill = as.numeric(PONDNO))) + 
  geom_point(data = filter(adk80s_A, PONDNO %in% pno$PONDNO), 
             aes(x = -LONG_NAD83DD, y = LAT_NAD83DD, fill = as.numeric(PONDNO)), color = "white")

plotly::ggplotly(g)
