
library(ecmwfr)


# This section sets up the API key obtained after registering an account
## See https://bluegreen-labs.github.io/ecmwfr/ for more details

## I keep my key in an rds object
era_keys <- readRDS("data/era5key.rds")
wf_set_key(user = era_keys$user, key = era_keys$cdskey, service = "cds")

# reanalysis-era5-land
## This section is how I download a single year of hourly data
## use for testing things out, next section gets everything
# DOWNLOAD ERA5
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  # dataset_short_name = "reanalysis-era5-land",
  product_type   = "reanalysis",
  format = "large_scale_rain_rate",
  variable = "large_scale_rain_rate",
  year = as.character(2015),
  month = stringr::str_pad(1:12,2,"left","0"),
  day = stringr::str_pad(1:31,2,"left","0"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", 
           "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", 
           "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(44.87791, -75.31968, 43.05235, -73.29350), # adk park
  target = "era5/era5_large_scale_rain_rate_2015.nc" # this is the path to where the download saves
)

t1 <- Sys.time()
file <- wf_request(user = era_keys$user,
                   request = request,
                   transfer = TRUE,
                   path = getwd(),
                   verbose = TRUE)
t2 <- Sys.time()
t2-t1



# This section is a loop that I use to download all necessary variables to drive LakeEnsemblR

mlst <- c("2m_temperature", 
          "2m_dewpoint_temperature",
          "10m_u_component_of_wind",
          "10m_v_component_of_wind",
          "large_scale_rain_rate", 
          "surface_pressure",
          "large_scale_snowfall_rate_water_equivalent",
          "mean_surface_downward_short_wave_radiation_flux",
          "mean_surface_downward_long_wave_radiation_flux"
          )

# Depending on the size of your area the data may be too big for a single request even within this loop
# This will take a long time to run...
for(x in mlst[-c(1,2,3)]){ # for each variable
  mvar <- x
  # if(x == "10m_v_component_of_wind"){sy = 1981}else{sy = 1980}
  for(i in 1984:1991){ # different download/request for each year
    
    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type   = "reanalysis",
      format = "netcdf",
      variable = mvar,
      year = as.character(i),
      month = stringr::str_pad(1:12,2,"left","0"),
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
    
    Sys.sleep(60*2) # sleeps code for 2 min to avoid making too many requests too fast (may not really be necessary)
  }
}




###############################
## This section puts all the downloaded data together into one data frame

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
dpdata <- data.table::rbindlist(dpdata) %>% unique() %>% arrange(datetime)




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


tmpdata <- data.table::rbindlist(tmpdata) %>% unique() %>% arrange(datetime)


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
wndudata <- data.table::rbindlist(wndudata) %>% unique() %>% arrange(datetime)


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
wndvdata <- data.table::rbindlist(wndvdata) %>% unique() %>% arrange(datetime)





files <- list.files("data/era5", pattern = "large_scale_rain_rate")
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
raindata <- data.table::rbindlist(raindata) %>% unique() %>% arrange(datetime)


files <- list.files("data/era5", pattern = "mean_surface_downward_long_wave")
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
dwldata <- data.table::rbindlist(dwldata) %>% unique() %>% arrange(datetime)


files <- list.files("data/era5", pattern = "mean_surface_downward_short_wave")
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
spresdata <- data.table::rbindlist(spresdata) %>% unique() %>% arrange(datetime)


# Uncomment this if you want to have snowfall in the data
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


era5met <- dwsdata %>%
  left_join(dwldata, by = c("datetime", "lat", "lon")) %>%
  left_join(dpdata, by = c("datetime", "lat", "lon")) %>%
  left_join(wndudata, by = c("datetime", "lat", "lon")) %>%
  left_join(wndvdata, by = c("datetime", "lat", "lon")) %>%
  left_join(raindata, by = c("datetime", "lat", "lon")) %>%
  left_join(tmpdata, by = c("datetime", "lat", "lon")) %>%
  left_join(spresdata, by = c("datetime", "lat", "lon")) %>%
  # left_join(do.call(rbind, snowdata), by = "datetime") %>% # uncomment for snowfall
  mutate(wind_spd = sqrt(wind_v^2 + wind_u^2), # compute wind speed
         temp_2m = temp_2m - 273.15, dewpt_2m = dewpt_2m - 273.15, # convert temps from K to C
         relhum = 100 * ((exp((17.625 * dewpt_2m)/(243.04+dewpt_2m))/ # compute rel humidity
                            exp((17.625 * temp_2m)/(243.04+temp_2m)))),
         precip = (precip * 86400)/24) %>%
  select(lat, lon, datetime, downward_shortwave,downward_longwave, temp_2m,
         relhum, wind_spd, precip, surface_press)


# colnames in the format of LakeEnsemblR
correct_names <- c("datetime", "Shortwave_Radiation_Downwelling_wattPerMeterSquared", 
                   "Longwave_Radiation_Downwelling_wattPerMeterSquared", "Air_Temperature_celsius", 
                   "Relative_Humidity_percent", "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", 
                   "Rainfall_millimeterPerHour", "Surface_Level_Barometric_Pressure_pascal")

colnames(era5met) <- c("lat", "lon", correct_names)

# write.csv(era5met, "data/era5_1992-2022.parquet", row.names = FALSE)
# arrow::write_parquet(era5met, "data/era5_adk_1980-2022.parquet")

era5 <- arrow::read_parquet("data/era5_adk_1980-2022.parquet")

era5 %>% filter(datetime == ymd_hms("1992-01-01 00:00:00")) %>% 
  ggplot(aes(x = lon, y = lat, fill = Air_Temperature_celsius)) + 
  geom_tile()

range(era5$datetime)

era5met <- filter(era5met, year(datetime) < 1992)
