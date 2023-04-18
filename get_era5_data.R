
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
  format = "2m_temperature",
  variable = metvars,
  year = as.character(1990),
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
  target = "era5_airtemp2m_1990.nc"
)

t1 <- Sys.time()
file <- wf_request(user = era_keys$user,
                   request = request,
                   transfer = TRUE,
                   path = getwd(),
                   verbose = TRUE)
t2 <- Sys.time()
t2-t1


mlst <- c("2m_temperature", "2m_dewpoint_temperature", 
          "10m_u_component_of_wind",
          "10m_v_component_of_wind", "large_scale_rain_rate", 
          "surface_pressure",
          "large_scale_snowfall_rate_water_equivalent",
          "mean_surface_downward_short_wave_radiation_flux",
          "mean_surface_downward_long_wave_radiation_flux")
for(x in mlst){
  mvar <- x
  for(i in 1992:2012){
    
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

files <- list.files("era5", pattern = "2m_dewpoint")
dpdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"d2m")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  dpdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(dewpt_2m = mean(value))
}





files <- list.files("era5", pattern = "2m_temperature")
tmpdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"t2m")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  tmpdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(temp_2m = mean(value))
}



files <- list.files("era5", pattern = "10m_u_component")
wndudata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"u10")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  wndudata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(wind_u = mean(value))
}



files <- list.files("era5", pattern = "10m_v_component")
wndvdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"v10")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  wndvdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(wind_v = mean(value))
}





files <- list.files("era5", pattern = "rain_rate")
raindata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"lsrr")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  raindata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(precip = mean(value))
}



files <- list.files("era5", pattern = "downward_long_wave")
dwldata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"msdwlwrf")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  dwldata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(downward_longwave = mean(value))
}



files <- list.files("era5", pattern = "downward_short_wave")
dwsdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"msdwswrf")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  dwsdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(downward_shortwave = mean(value))
}


files <- list.files("era5", pattern = "surface_pressure")
spresdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"sp")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  spresdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(surface_press = mean(value))
}

files <- list.files("era5", pattern = "snowfall")
snowdata <- list()
for(i in seq_along((files))){
  nc <- nc_open(paste0("era5/", files[i]))
  data <- ncvar_get(nc,"lssfr")
  
  lat <- ncvar_get(nc,'latitude')
  lon <- ncvar_get(nc,'longitude')
  times <- lubridate::hours(ncvar_get(nc, 'time')) + ymd_hms("1900-01-01 00:00:00")
  
  snowdata[[i]] <- reshape2::melt(data) %>% 
    mutate(lat = lat[Var1], lon = lon[Var2], datetime = times[Var3]) %>% 
    group_by(datetime) %>% summarize(snow = mean(value))
}

# relhum from: https://www.omnicalculator.com/physics/relative-humidity
met <- read.csv("LakeEnsemblR_meteo_standard.csv")
head(met)

era5met <- do.call(rbind, dwsdata) %>% 
  left_join(do.call(rbind, dwldata), by = "datetime") %>% 
  left_join(do.call(rbind, dpdata), by = "datetime") %>% 
  left_join(do.call(rbind, wndudata), by = "datetime") %>% 
  left_join(do.call(rbind, wndvdata), by = "datetime") %>% 
  left_join(do.call(rbind, raindata), by = "datetime") %>% 
  left_join(do.call(rbind, tmpdata), by = "datetime") %>% 
  left_join(do.call(rbind, spresdata), by = "datetime") %>% 
  left_join(do.call(rbind, snowdata), by = "datetime") %>% 
  mutate(wind_spd = sqrt(wind_v^2 + wind_u^2), 
         temp_2m = temp_2m - 273.15, dewpt_2m = dewpt_2m - 273.15,
         relhum = 100 * ((exp((17.625 * dewpt_2m)/(243.04+dewpt_2m))/
                            exp((17.625 * temp_2m)/(243.04+temp_2m))))) %>% 
  select(datetime, downward_shortwave,downward_longwave, temp_2m, 
         relhum, wind_spd, precip, snow, surface_press)

colnames(era5met) <- colnames(met)
