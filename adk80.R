library(adklakedata)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(sf)
library(ggridges)

meta <- adk_data("meta")

adkalt <- st_read("C:/Users/borre/Downloads/cugir-007739/cugir-007739/", "blueline") %>% 
  st_transform("WGS84")

adk80s <- read.csv("data/NY_ADIRONDACK_1984.csv")
als_physic <- readxl::read_excel("data/ALS84/REC9_DATA_TABLE.xls")

adk80s_A <- select(adk80s, POND, PONDNAME, DATE, LONG_NAD83DD, LAT_NAD83DD, DOC, TRUCOLOR) %>% 
  mutate(PONDNO = stringr::str_pad(POND, 6, "left", "0")) %>% 
  select(PONDNO, PONDNAME, DATE, LONG_NAD83DD, LAT_NAD83DD, DOC, TRUCOLOR)
adk80s_B <- filter(als_physic, PONDNO %in% adk80s_A$PONDNO) %>% 
  select(PONDNO, ADATE, TIME, STATION, DEPTH, TEMPF, SECCHI, CLOUD)
write.csv(adk80s_A, "C:/Users/borre/Desktop/adksurvey84.csv")
write.csv(adk80s_B, "C:/Users/borre/Desktop/adksurvey84profiles.csv")

als_morpho <- read_excel("data/ALS84/REC3_DATA_TABLE.xls")

ggplot(adkalt) + geom_sf() +
  geom_point(data = meta, aes(x = long, y = lat)) + 
  theme_void()

ggplot(adkalt) + geom_sf() +
  geom_point(data = adk80s, aes(x = -LONG_NAD83DD, y = LAT_NAD83DD), size = .5) + 
  theme_void()


a84 <- adk80s[grepl("1987", adk80s$DATE),]

pc <- princomp(scale(a84[complete.cases(a84),c("DOC", "TOTAL_P1", "Ca", "NH4", "FIELDPH", "TRUCOLOR", "SCONDUCT")]))
pc$loadings
biplot(pc)

pc <- princomp(scale(a84[complete.cases(a84),6:ncol(a84)]))
pc$loadings
biplot(pc)

plot(a84[complete.cases(a84),6:ncol(a84)])


library(mgcv)
adk80s2 <- adk80s[!is.na(adk80s$TOTAL_P1) & adk80s$TOTAL_P1 > 0,]
fit <- gam(DOC ~ s(LONG_NAD83DD, LAT_NAD83DD), data = adk80s2, family = Gamma(link = "log"))
gratia::draw(fit)
ndf <- expand.grid(LONG_NAD83DD = seq(73.2935, 75.31968, by = 0.005),
           LAT_NAD83DD = seq(43.05235, 44.87791, by = 0.005))
ndf <- as.data.frame(ndf)

rfit <- broom::augment(fit, newdata = ndf, type.predict = "response") 
  
library(raster)
rastfit <- rasterFromXYZ(mutate(rfit, LONG_NAD83DD = -LONG_NAD83DD)[,1:3])

rfit2 <- mask(rastfit, adkalt) %>% 
  as.data.frame(xy = TRUE) %>% 
  filter(!is.na(.fitted))

ggplot(rfit2) +
  #geom_raster(aes(x = x, y = y, fill = .fitted)) + 
  geom_point(data = adk80s2, aes(x = -LONG_NAD83DD, y = LAT_NAD83DD, color = log(TOTAL_P1))) + 
  geom_sf(data = adkalt, fill = NA, color = "black") + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() + 
  labs(fill = "DOC")
summary(fit)





#####################################
## https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=101&revision=3

lagos1 <- data.table::fread("nhd/epi_waterquality10873.csv")
lagos2 <- data.table::fread("nhd/lakeslimno10873.csv")

# ADK bounding box
# xmin: -75.31831 ymin: 43.05239 xmax: -73.2935 ymax: 44.87769
adkarealakes <- left_join(lagos1, lagos2, by = "lagoslakeid") %>% 
  filter(nhd_long <  -73.2935 & nhd_long > -75.31831, 
         nhd_lat < 44.87769, nhd_lat > 43.05239)

ggplot(filter(states, ID == "new york")) + geom_sf() + 
  geom_point(data = filter(adkarealakes, year(ymd(sampledate)) == 2010),
             aes(x = nhd_long, y = nhd_lat))


ggplot(adkalt) + geom_sf() + 
  geom_point(data = adkarealakes,
             aes(x = nhd_long, y = nhd_lat)) + 
  theme_void()

library(mgcv)
library(gganimate)

adklakes1 <- adkarealakes %>% mutate(year = year(ymd(sampledate))) %>% 
  filter(year > 1985)

fit <- gam(chla ~ s(year) + s(nhd_lat, nhd_long, bs = "re"), data = adklakes1, family = Gamma(link = "log")) 


ndf <- expand.grid(nhd_long = seq(-75.31968, -73.2935, by = 0.005),
                   nhd_lat = seq(43.05235, 44.87791, by = 0.005),
                   year = 1986:2012)
ndf <- as.data.frame(ndf)

rfit <- broom::augment(fit, newdata = ndf, type.predict = "response") 

library(tidyr)
library(raster)

# adkalt2 <- st_transform(adkalt, "WGS84")
rfit2 <- list()
for(i in seq_along(1986:2012)){
  rastfit <- rasterFromXYZ(filter(rfit, year == (1986:2012)[i])[,c(1,2,4)])
  
  rfit2[[i]] <- mask(rastfit, (adkalt)) %>% 
    as.data.frame(xy = TRUE) %>% 
    filter(!is.na(.fitted)) %>% 
    mutate(year = (1986:2012)[i])
  
  print(i)
}

data.table::rbindlist(rfit2) %>%
  ggplot(aes(x = x, y = y, fill = .fitted)) + 
  geom_raster() +
  scale_fill_viridis_c() +
  labs(title = "{closest_state}") + 
  transition_states(year, transition_length = 1, state_length = 2)




rfit2 <- mask(rastfit, adkalt) %>% 
  as.data.frame(xy = TRUE) %>% 
  filter(!is.na(.fitted))

ggplot(rfit2) +
  #geom_raster(aes(x = x, y = y, fill = .fitted)) + 
  geom_point(data = adk80s2, aes(x = -LONG_NAD83DD, y = LAT_NAD83DD, color = log(TOTAL_P1))) + 
  geom_sf(data = adkalt, fill = NA, color = "black") + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() + 
  labs(fill = "DOC")


# – NC1: has detection limit and data value is above detection limit and has no qualifier or
# comments
# – NC2: has detection limit and data value is above detection limit, and has qualifier or
# comments
# – NC3: has no detection limit and has qualifier or comments
# – NC4: has no detection limit and has no qualifiers or comments
# – LE1: has detection limit, data value is less than or equal to detection limit, has qualifier
# or comments
# – LE2: has detection limit, data value is less than or equal to detection limit, has no qualifier
# or comments
# – LE3: has no detection limit, < than comes from source program, has qualifier or comments
# – LE4: has no detection limit, < than comes from source program, has no qualifier or
# comments



ggplot(adkarealakes, aes(x = (chla), y = factor(year(ymd(sampledate))))) + 
  geom_density_ridges() + 
  coord_cartesian(xlim = c(0, 30))


ggplot(adkarealakes, aes(x = (secchi), y = factor(year(ymd(sampledate))))) + 
  geom_density_ridges() + 
  coord_cartesian(xlim = c(0, 10))


ggplot(filter(adkarealakes, !grepl("LE", doc_censorcode)), aes(y = (doc), x = ymd(sampledate))) + geom_point() + 
  theme_bw(base_size = 16) + 
  labs(x = "Date", y = "DOC")

ggplot(filter(adkarealakes, !grepl("LE", chla_censorcode)), aes(y = (chla), x = ymd(sampledate))) + geom_point() + 
  theme_bw(base_size = 16) + 
  labs(x = "Date", y = "Chl a")

ggplot(filter(adkarealakes, !grepl("LE", tp_censorcode)), aes(y = (tp), x = ymd(sampledate))) + geom_point() + 
  theme_bw(base_size = 16) + 
  labs(x = "Date", y = "TP")

ggplot(filter(adkarealakes, !grepl("LE", colort_censorcode)), aes(y = (colort), x = ymd(sampledate))) + geom_point() + 
  theme_bw(base_size = 16) + 
  labs(x = "Date", y = "Color T")

ggplot(filter(adkarealakes, !grepl("LE", secchi_censorcode)), aes(y = (secchi), x = ymd(sampledate))) + geom_point() + 
  theme_bw(base_size = 16) + 
  labs(x = "Date", y = "Secchi")


ggplot(filter(adkarealakes, !grepl("LE", tkn_censorcode)), aes(y = (tkn), x = ymd(sampledate))) + geom_point() + 
  theme_bw(base_size = 16) + 
  labs(x = "Date", y = "")

# 
# ggplot(adkarealakes) + 
#   geom_sf(data = adkalt) +
#   geom_point(aes(x = nhd_long, y = nhd_lat)) + 
#   labs(title = "{closest_state}") + 
#   transition_states(year(ymd(sampledate))) + 
#   theme_void()

# anim_save("C:/Users/borre/Desktop/lagos_adk.gif")  


phy <- adklakedata::adk_data("phyto")
phy

phy %>% group_by(lake.name, date, year) %>% 
  summarize(bio = sum(biovol.um3.per.ml)) %>% 
  ggplot(aes(x = year, y = (bio/10^6*1000))) + geom_point() +
  geom_smooth() + scale_y_log10() + 
  facet_wrap(~lake.name)
