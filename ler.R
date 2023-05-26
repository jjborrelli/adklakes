library(LakeEnsemblR)
library(rLakeAnalyzer)
library(ggplot2)
library(patchwork)
library(adklakedata)
library(dplyr)

t1 <- Sys.time()
# set file name for ler yaml
ler_yaml <- "LakeEnsemblR.yaml"
# only look at lakes with max depth greater than 5 m
meta <- adk_data("meta")
meta <- meta[meta$max.depth > 5,]
era5 <- arrow::read_parquet("data/era5_adk_1992-2012.parquet")
tdo <- adk_data("tempdo")

meta2 <- read.csv("C:/Users/borre/Desktop/adk.csv")
meta2 <- meta2[meta2$max.depth > 5,]

# get secchi depths
secchi <- adk_data("secchi")

for(i in 1:nrow(meta)){
  
  rn <- which.min(as.matrix(dist(rbind(data.frame(lat = meta$lat[i], lon = meta$long[i]),
                                       unique(dplyr::select(era5, lat, lon)))))[-1,1])
  
  latlon <- unique(dplyr::select(era5, lat, lon))[rn,]
  
  ## Set location data (from `meta`)
  input_yaml_multiple(file = ler_yaml, meta$lake.name[i], key1 = "location", key2 = "name")
  input_yaml_multiple(file = ler_yaml, meta$lat[i], key1 = "location", key2 = "latitude")
  input_yaml_multiple(file = ler_yaml, meta$long[i], key1 = "location", key2 = "longitude")
  input_yaml_multiple(file = ler_yaml, meta$elevation.m[i], key1 = "location", key2 = "elevation")
  input_yaml_multiple(file = ler_yaml, meta$max.depth[i], key1 = "location", key2 = "depth")
  
  ## Set hypsographic curve by approximating bathymetry using rLakeAnalyzer
  bathy <- approx.bathy(meta$max.depth[i], meta$SA.ha[i]*1e5, meta$mean.depth[i])
  colnames(bathy) <- c("Depth_meter", "Area_meterSquared")
  write.csv(bathy, "lake_hyps.csv", row.names = FALSE)
  
  ## set hypsography and max depth in yaml
  input_yaml_multiple(file = ler_yaml, "lake_hyps.csv", key1 = "location", key2 = "hypsograph")
  input_yaml_multiple(file = ler_yaml, meta$max.depth[i], key1 = "location", key2 = "init_depth")
  
  
  # compute initial profile
  laketdo <- filter(tdo, lake.name == meta$lake.name[i])
  initlake <- filter(laketdo, date == min(laketdo$date), depth <= meta$max.depth[i]) %>% select(deps = depth, temp)
  write.csv(initlake, "init_profile.csv", row.names = FALSE)
  
  ## set initial prof in yaml
  input_yaml_multiple(file = ler_yaml, "init_profile.csv", key1 = "input", key2 = "init_temp_profile", key3 = "file")
  
  ## compute light ext
  kdval <- secchi %>% group_by(lake.name) %>% 
    summarize(kd = median(1.7/secchi, na.rm = TRUE))
  ## set kd in yaml
  input_yaml_multiple(file = ler_yaml, round(kdval$kd[kdval$lake.name %in% meta$lake.name[i]], 2),
                      key1 = "input", key2 = "light", key3 = "Kw")
  
  ## set start date
  input_yaml_multiple(file = ler_yaml, min(laketdo$date), key1 = "time", key2 = "start")
  
  ## set end date
  input_yaml_multiple(file = ler_yaml, "1999-12-31 00:00:00", key1 = "time", key2 = "stop")
  
  ## Set observations
  ler_obs <- tdo %>% filter(lake.name == meta$lake.name[i], !is.na(temp)) %>% 
    select(datetime = date, Depth_meter = depth, Water_Temperature_celsius = temp)
  
  
  write.csv(ler_obs, "LakeEnsemblR_wtemp_profile_standard.csv")
  
  input_yaml_multiple(file = ler_yaml, "LakeEnsemblR_wtemp_profile_standard.csv", key1 = "observations", key2 = "temperature", key3 = "file")
  
  
  ## Set meteorology driver file
  #met <- read.csv("LakeEnsemblR_meteo_standard_narr.csv")
  # met2 <- met %>% filter(year(ymd_hms(datetime)) >= 1994, year(ymd_hms(datetime)) < 2001)
  # 
  # write.csv(met2, "LakeEnsemblr_meteo_narr_sub.csv", row.names = FALSE)
  lakemets <- era5 %>% filter(lat == latlon$lat[1], lon == latlon$lon[1]) %>% select(-lat, - lon)
  
  # lakemets <- filter(meteo, lake.name == meta$lake.name[i], year(ymd_hms(date)) >= 1994, year(ymd_hms(date)) < 2001) %>% 
  #   left_join(dplyr::select(met, datetime, Surface_Level_Barometric_Pressure_pascal), by = c("date" = "datetime")) %>% 
  #   dplyr::select(-PERMANENT_ID, -lake.name)
  # colnames(lakemets) <- c("datetime",  
  #                         "Shortwave_Radiation_Downwelling_wattPerMeterSquared",
  #                         "Longwave_Radiation_Downwelling_wattPerMeterSquared", 
  #                         "Air_Temperature_celsius", "Relative_Humidity_percent", 
  #                         "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", 
  #                         "Rainfall_millimeterPerHour", "Snowfall_millimeterPerHour", 
  #                         "Surface_Level_Barometric_Pressure_pascal")
  
  write.csv(lakemets, "LakeEnsemblr_meteo_adk_sub.csv", row.names = FALSE)
  
  # input_yaml_multiple(file = ler_yaml, "LakeEnsemblr_meteo_narr_sub.csv", key1 = "meteo", key2 = "file")
  input_yaml_multiple(file = ler_yaml, "LakeEnsemblr_meteo_adk_sub.csv", key1 = "meteo", key2 = "file")
  
  
  ## Set output file
  outfile <- paste0("output_", gsub(" ", "", meta$lake.name[i]))
  input_yaml_multiple(file = ler_yaml, outfile, key1 = "output", key2 = "file")
  
  # params
  ## Assume circle for now
  fetch <- meta2$Length_km[i] * 1000 #sqrt((meta$SA.ha[i] * 1e5)/pi)*2 
  blen <- meta2$Length_km[i] * 1000 #sqrt((meta$SA.ha[i] * 1e5)/pi)*2
  bwid <- meta2$Width_km[i] * 1000 #sqrt((meta$SA.ha[i] * 1e5)/pi)*2
  
  input_yaml_multiple(file = ler_yaml, fetch, key1 = "model_parameters", key2 = "FLake", key3 = "fetch_lk")
  input_yaml_multiple(file = ler_yaml, blen, key1 = "model_parameters", key2 = "GLM", key3 = "bsn_len")
  input_yaml_multiple(file = ler_yaml, bwid, key1 = "model_parameters", key2 = "GLM", key3 = "bsn_wid")
  
  
  # set config
  config_file <- ler_yaml
  model <- c("FLake", "GLM", "GOTM", "Simstrat")
  
  export_config(config_file = config_file, model = model)
  
  # run model ensemble
  run_ensemble(config_file = config_file, model = model, parallel = TRUE)
  
  cali_res <- cali_ensemble(config_file = config_file, num = 200, cmethod = "LHC",
                            parallel = FALSE, model = model, 
                            out_f = paste("cali", meta$lake.name[i], sep = "_"))
  
  saveRDS(cali_res, paste("cali_", meta$lake.name[i], ".rds", sep = ""))
}

t2 <- Sys.time()
t2-t1

# select lake with i
i = 4
# read in simulation data
ncdf <- paste0("output/output_", gsub(" ", "", meta$lake.name[i]), ".nc")

# plot output
plot_heatmap(ncdf) +
  theme_bw(base_size = 12) + 
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + 
  labs(x = "Date", y = "Depth (m)", color = "°C", title = meta$lake.name[i])


calc_fit(ncdf = ncdf, model = model, var = "temp")
analyse_df <- analyse_ncdf(ncdf = ncdf, model = model, spin_up = NULL, drho = 0.1)
# Example plot the summer stratification period
strat_df <- analyse_df$strat
ggplot(strat_df, aes(model, TotStratDur)) +
  geom_col() +
  ylab("Total stratification duration [days]") +
  xlab("") +
  theme_classic()


tdo %>% filter(lake.name == "Rondaxe") %>% 
  ggplot(aes(x = depth, y = temp)) + geom_point() + coord_flip() + scale_x_reverse()





