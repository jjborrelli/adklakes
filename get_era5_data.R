
library(ecmwfr)


era_keys <- readRDS("data/era5key.rds")


wf_set_key(user = era_keys$user, key = era_keys$cdskey, service = "cds")


### DOWNLOAD ERA5
c("air_temperature", "air_pressure", 
  "relative_humidity", "surface_downwelling_longwave_flux_in_air", 
  "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", 
  "specific_humidity", "wind_speed")

metvars <- c("2m_temperature", "2m_dewpoint_temperature", "10m_u_component_of_wind",
             "10m_v_component_of_wind", "large_scale_rain_rate", 
             "surface_pressure",
             "large_scale_snowfall_rate_water_equivalent",
             "mean_surface_downward_short_wave_radiation_flux",
             "mean_surface_downward_long_wave_radiation_flux")


request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  # format = "2m_temperature",
  format = "large_scale_rain_rate",
  variable = "large_scale_rain_rate",
  year = as.character(1993),
  month = stringr::str_pad(1:12,2,"left","0"),
  day = stringr::str_pad(1:31,2,"left","0"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", 
           "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", 
           "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  # area = c(45.01157, -79.76718, 40.4852, -71.87756),
  area = c(44.87791, -75.31968, 43.05235, -73.29350), # adk park
  # area = c(43.83786, -73.71255, 43.41753, -73.42483),
  target = "era5/era5_large_scale_rain_rate_1993.nc"
)

t1 <- Sys.time()
file <- wf_request(user = era_keys$user,
                   request = request,
                   transfer = TRUE,
                   path = getwd(),
                   verbose = TRUE)
t2 <- Sys.time()
t2-t1


mlst <- c(#"2m_temperature", 
          #"2m_dewpoint_temperature", 
          #"10m_u_component_of_wind",
          #"10m_v_component_of_wind", 
          "large_scale_rain_rate"#, 
          #"surface_pressure",
          #"large_scale_snowfall_rate_water_equivalent",
          #"mean_surface_downward_short_wave_radiation_flux",
          #"mean_surface_downward_long_wave_radiation_flux"
          )
for(x in mlst){
  mvar <- x
  for(i in 1994:2012){
    
    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type   = "reanalysis",
      format = "netcdf",
      variable = mvar,
      year = as.character(i),
      month = stringr::str_pad(1:10,2,"left","0"),
      day = stringr::str_pad(1:31,2,"left","0"),
      time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00",
               "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", 
               "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
               "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
      # area is specified as N, W, S, E
      #area = c(45.01157, -79.76718, 40.4852, -71.87756),
      area = c(44.87791, -75.31968, 43.05235, -73.29350),
      target = paste0("data/era5/era5_", mvar, "_", i,".nc")
    )
    
    t1 <- Sys.time()
    file <- wf_request(user = era_keys$user,
                       request = request,
                       transfer = TRUE,
                       path = getwd(),
                       verbose = TRUE, 
                       time_out = 15000)
    t2 <- Sys.time()
    t2-t1
    
    Sys.sleep(60*2)
  }
}




###############################

library(ncdf4)
library(sf)
library(ggplot2)
library(lubridate)
library(dplyr)

files <- list.files("data/era5", pattern = "2m_dewpoint")
dpdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"d2m")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  dpdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>% 
    mutate(dewpt_2m = value) %>% 
    select(lat, lon, datetime, dewpt_2m)
}
dpdata <- data.table::rbindlist(dpdata)




files <- list.files("data/era5", pattern = "2m_temperature")
tmpdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"t2m")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  tmpdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>% 
    mutate(temp_2m = value) %>% 
    select(lat, lon, datetime, temp_2m)
}


tmpdata <- data.table::rbindlist(tmpdata)
# 
# test %>% 
#   filter(year(datetime) == 2012) %>% 
#   ggplot() + 
#   geom_raster(aes(x = lon, y = lat, fill = temp_2m-273.15)) +
#   geom_sf(data = st_transform(adkalt, "WGS84"), fill = NA, color = "black", linewidth = 1.2) + 
#   scale_fill_viridis_c(option = "B") + 
#   labs(title = "{frame_time}") + 
#   transition_time(datetime)


files <- list.files("data/era5", pattern = "10m_u_component")
wndudata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"u10")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  wndudata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>%  
    mutate(wind_u = value) %>% 
    select(lat, lon, datetime, wind_u)
}
wndudata <- data.table::rbindlist(wndudata)


files <- list.files("data/era5", pattern = "10m_v_component")
wndvdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"v10")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  wndvdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>%  
    mutate(wind_v = value) %>% 
    select(lat, lon, datetime, wind_v)
}
wndvdata <- data.table::rbindlist(wndvdata)





files <- list.files("data/era5", pattern = "rain_rate")
raindata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"lsrr")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  raindata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>%  
    mutate(precip = value) %>% 
    select(lat, lon, datetime, precip)
}
raindata <- data.table::rbindlist(raindata)


files <- list.files("data/era5", pattern = "downward_long_wave")
dwldata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"msdwlwrf")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  dwldata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>%  
    mutate(downward_longwave = value) %>% 
    select(lat, lon, datetime, downward_longwave)
}
dwldata <- data.table::rbindlist(dwldata)


files <- list.files("data/era5", pattern = "downward_short_wave")
dwsdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"msdwswrf")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  dwsdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>%   
    mutate(downward_shortwave = value) %>% 
    select(lat, lon, datetime, downward_shortwave)
}
dwsdata <- data.table::rbindlist(dwsdata)

files <- list.files("data/era5", pattern = "surface_pressure")
spresdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("data/era5/", files[i]))
  data <- ncvar_get(nc,"sp")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  spresdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var2], lon = lon[Var1], datetime = times[Var3]) %>%   
    mutate(surface_press = value) %>% 
    select(lat, lon, datetime, surface_press)
}
spresdata <- data.table::rbindlist(spresdata)
# 
# files <- list.files("era5", pattern = "snowfall")
# snowdata <- list()
# for(i in seq_along((files))){
#   nc <- nc_open(paste0("era5/", files[i]))
#   data <- ncvar_get(nc,"lssfr")
#   
#   lat <- ncvar_get(nc,'latitude')
#   lon <- ncvar_get(nc,'longitude')
#   times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
#   
#   snowdata[[i]] <- reshape2::melt(data) %>% 
#     mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
#     group_by(datetime) %>% summarize(snow = mean(value))
# }

# relhum from: https://www.omnicalculator.com/physics/relative-humidity
met <- read.csv("LakeEnsemblR_meteo_adk_sub.csv")
head(met)

era5met <- dwsdata %>%
  left_join(dwldata, by = c("datetime", "lat", "lon")) %>%
  left_join(dpdata, by = c("datetime", "lat", "lon")) %>%
  left_join(wndudata, by = c("datetime", "lat", "lon")) %>%
  left_join(wndvdata, by = c("datetime", "lat", "lon")) %>%
  left_join(raindata, by = c("datetime", "lat", "lon")) %>%
  left_join(tmpdata, by = c("datetime", "lat", "lon")) %>%
  left_join(spresdata, by = c("datetime", "lat", "lon")) %>%
  # left_join(do.call(rbind, snowdata), by = "datetime") %>%
  mutate(wind_spd = sqrt(wind_v^2 + wind_u^2),
         temp_2m = temp_2m - 273.15, dewpt_2m = dewpt_2m - 273.15,
         relhum = 100 * ((exp((17.625 * dewpt_2m)/(243.04+dewpt_2m))/
                            exp((17.625 * temp_2m)/(243.04+temp_2m))))) %>%
  select(lat, lon, datetime, downward_shortwave,downward_longwave, temp_2m,
         relhum, wind_spd, precip, surface_press)

colnames(era5met) <- c("lat", "lon", colnames(met)[-8])


# arrow::write_parquet(era5met, sink = "data/era5_adk_1992-2012.parquet")
