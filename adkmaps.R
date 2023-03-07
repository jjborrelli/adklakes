library(sf)
library(dplyr)
library(ggplot2)


states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
hyd <- st_read("data/hydro/", "AreaHydrography")
adk <- st_read("data/AdirondackParkBoundary2017/", "AdirondackParkBoundary2017")
adkland <- st_read("data/apaLandClass202111/", "apaLandClass202111")


# need to convert adk to NAD83 / UTM zone 18N
adk2 <- adk %>% 
  # st_cast("MULTIPOINT") %>% 
  # st_combine() %>% 
  # #st_union() %>% 
  # st_cast("POLYGON") %>% 
  st_transform(st_crs(hyd))


ggplot(filter(states, ID == "new york")) + geom_sf() + 
  geom_sf(data = hyd, fill = "blue", color = "blue") + 
  geom_sf(data = adk2)


apa <- st_transform(adkland, st_crs(hyd))

ggplot(apa) + geom_sf()

st_crop(hyd,apa) %>% 
  ggplot() + geom_sf(color = "blue", fill = "blue") + 
  geom_sf(data = adk2, size = 2)

apahyd <- st_intersection(apa,hyd)
apahyd%>% 
  filter(!grepl("River", NAME), !grepl("Creek", NAME)) %>% 
  # filter(!grepl("Pond", NAME)) %>% 
  ggplot() +  
  geom_sf(data = apa, fill = "green4", color = NA, alpha = 0.5) +
  # geom_sf(data = apa, fill = NA, color = "black") +
  geom_sf(color = "blue", fill = "blue") +
  geom_sf(data = adk2, size = 2) + 
  # theme_bw()
  theme_void()

hyd[(grepl("Big Moose",hyd$NAME)),] %>% 
  filter(BUS_FID_Ar == 4669782.8111) %>% 
  ggplot() +  
  #geom_sf(data = apa, fill = "green4", color = NA, alpha = 0.5) +
  # geom_sf(data = apa, fill = NA, color = "black") +
  geom_sf(color = "blue", fill = "blue") +
  #geom_sf(data = adk2, size = 2) + 
  # theme_bw()
  theme_void() + facet_wrap(~BUS_FID_Ar)

hyd[(grepl("Big Moose",hyd$NAME)),] %>% 
  filter(BUS_FID_Ar == 4669782.8111) %>% 
  


adkwaternames <- unique(st_intersection(apa,hyd)$NAME)

adkwaternames[grepl("Lake", adkwaternames)]
length(adkwaternames)
adkwaternames[!grepl("River", adkwaternames) & !grepl("Creek", adkwaternames)]
