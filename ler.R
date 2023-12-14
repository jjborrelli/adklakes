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
era5 <- arrow::read_parquet("era5_adk_1980-2022.parquet") %>% data.table::as.data.table()
tdo <- adk_data("tempdo")

meta2 <- read.csv("C:/Users/borre/Desktop/JP/adk.csv")
meta2 <- meta2[meta2$max.depth > 5,]


# get secchi depths
secchi <- adk_data("secchi")

t1 <- Sys.time()
# recently stopped after i = 16
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
  initlake$temp <- 4
  write.csv(initlake, "init_profile.csv", row.names = FALSE)
  
  ## set initial prof in yaml
  input_yaml_multiple(file = ler_yaml, "init_profile.csv", key1 = "input", key2 = "init_temp_profile", key3 = "file")
  
  ## compute light ext
  kdval <- secchi %>% group_by(lake.name) %>% 
    summarize(kd = median(1.7/secchi, na.rm = TRUE))
  
  kdval2 <- secchi %>% mutate(kd = 1.7/secchi) %>% 
    filter(lake.name == meta$lake.name[i]) %>% 
    select(date, kd) %>% mutate(kd = zoo::na.aggregate(kd), kd= round(kd, 3)) %>% 
    arrange(date) %>% 
    rename(datetime = date, Extinction_Coefficient_perMeter = kd) %>% 
    mutate(datetime = paste(datetime, "12:00:00"))
  kdval2 <- rbind(kdval2, data.frame(datetime = c("1980-01-01 12:00:00", "2022-12-31 12:00:00"), 
             Extinction_Coefficient_perMeter = round(kdval$kd[kdval$lake.name %in% meta$lake.name[i]], 3))) %>%
    arrange(datetime)
        
  
  write.csv(kdval2, "kdvals.csv", row.names = FALSE)
  ## set kd in yaml
  # input_yaml_multiple(file = ler_yaml, round(kdval$kd[kdval$lake.name %in% meta$lake.name[i]], 2),
  #                     key1 = "input", key2 = "light", key3 = "Kw")
  input_yaml_multiple(file = ler_yaml, "kdvals.csv",
                      key1 = "input", key2 = "light", key3 = "Kw")
  
  ## set start date
  input_yaml_multiple(file = ler_yaml, "1980-01-01 00:00:00", key1 = "time", key2 = "start")
  
  ## set end date
  input_yaml_multiple(file = ler_yaml, "2022-12-31 00:00:00", key1 = "time", key2 = "stop")
  
  ## Set observations
  ler_obs <- tdo %>% filter(lake.name == meta$lake.name[i], !is.na(temp)) %>% 
    select(datetime = date, Depth_meter = depth, Water_Temperature_celsius = temp)
  
  
  write.csv(ler_obs, "LakeEnsemblR_wtemp_profile_standard.csv", row.names = FALSE)
  
  input_yaml_multiple(file = ler_yaml, "LakeEnsemblR_wtemp_profile_standard.csv", key1 = "observations", key2 = "temperature", key3 = "file")
  
  
  ## Set meteorology driver file
  #met <- read.csv("LakeEnsemblR_meteo_standard_narr.csv")
  # met2 <- met %>% filter(year(ymd_hms(datetime)) >= 1994, year(ymd_hms(datetime)) < 2001)
  # 
  # write.csv(met2, "LakeEnsemblr_meteo_narr_sub.csv", row.names = FALSE)
  lakemets <- era5 %>% filter(lat == latlon$lat[1], lon == latlon$lon[1]) %>% select(-lat, - lon) %>% arrange(datetime)
  
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
  lakemets$datetime <- as.character(format(lakemets$datetime))
  write.csv(lakemets, "LakeEnsemblr_meteo_adk_sub.csv", row.names = FALSE)
  
  # input_yaml_multiple(file = ler_yaml, "LakeEnsemblr_meteo_narr_sub.csv", key1 = "meteo", key2 = "file")
  input_yaml_multiple(file = ler_yaml, "LakeEnsemblr_meteo_adk_sub.csv", key1 = "meteo", key2 = "file")
  
  
  ## Set output file
  outfile <- paste0("output_ice_", gsub(" ", "", meta$lake.name[i]))
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
  model <- c("GLM", "Simstrat")
  
  export_config(config_file = config_file, model = model)
  
  # run model ensemble
  run_ensemble(config_file = config_file, model = model, parallel = TRUE)
  
  # cali_res <- cali_ensemble(config_file = config_file, num = 500, cmethod = "LHC",
  #                           parallel = TRUE, model = model, 
  #                           out_f = paste("cali", meta$lake.name[i], sep = "_"))
  # 
  # saveRDS(cali_res, paste("cali_", meta$lake.name[i], ".rds", sep = ""))
}

t2 <- Sys.time()
t2-t1

# select lake with i
i = 5
# read in simulation data
ncdf <- paste0("output/output_ice_", gsub(" ", "", meta$lake.name[i]), ".nc")

# plot output
plot_heatmap(ncdf, model = "Simstrat") +
  theme_bw(base_size = 12) + 
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + 
  labs(x = "Date", y = "Depth (m)", color = "°C", title = meta$lake.name[i])


model <- c("GLM", "Simstrat")
calc_fit(ncdf = ncdf, model = model, var = "temp")
analyse_df <- analyse_ncdf(ncdf = ncdf, model = model, spin_up = NULL, drho = 0.1)
# Example plot the summer stratification period
strat_df <- analyse_df$strat
ggplot(strat_df, aes(year, TotIceDur)) +
  geom_col() +
  ylab("Total stratification duration [days]") +
  xlab("") +
  theme_classic()


tdo %>% filter(lake.name == "Rondaxe") %>% 
  ggplot(aes(x = depth, y = temp)) + geom_point() + coord_flip() + scale_x_reverse()



rmse <- lapply(list.files(pattern = "cali_")[grepl(".rds", list.files(pattern = "cali_"))], function(y){
  c1 <- readRDS(y)
  sapply(c1, function(x){
    min(read.csv(x$results)$rmse)
  })
}) 

boxplot(do.call(rbind, rmse))


c1 <- readRDS("cali_Big Moose.rds")
min(read.csv(c1$Simstrat$results)$rmse)
res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(c1))
best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)


input_yaml_multiple(file = ler_yaml, 1.0611, key1 = "scaling_factors", key2 = "FLake", key3 = "wind_speed")
input_yaml_multiple(file = ler_yaml, 0.80615, key1 = "scaling_factors", key2 = "FLake", key3 = "swr")
input_yaml_multiple(file = ler_yaml, 0.009822, key1 = "model_parameters", key2 = "FLake", key3 = "c_relax_C")


input_yaml_multiple(file = ler_yaml, 0.55132, key1 = "scaling_factors", key2 = "GLM", key3 = "wind_speed")
input_yaml_multiple(file = ler_yaml, 0.78292, key1 = "scaling_factors", key2 = "GLM", key3 = "swr")
input_yaml_multiple(file = ler_yaml, 1.8503, key1 = "model_parameters", key2 = "GLM", key3 = "mixing.coef_mix_hyp")


input_yaml_multiple(file = ler_yaml, 1.5963, key1 = "scaling_factors", key2 = "GOTM", key3 = "wind_speed")
input_yaml_multiple(file = ler_yaml, 0.52468, key1 = "scaling_factors", key2 = "GOTM", key3 = "swr")
input_yaml_multiple(file = ler_yaml, 05.2533e-6, key1 = "model_parameters", key2 = "GOTM", key3 = "turb_param.k_min")


input_yaml_multiple(file = ler_yaml, 0.59796, key1 = "scaling_factors", key2 = "Simstrat", key3 = "wind_speed")
input_yaml_multiple(file = ler_yaml, 0.7048, key1 = "scaling_factors", key2 = "Simstrat", key3 = "swr")
input_yaml_multiple(file = ler_yaml, 0.0022615, key1 = "model_parameters", key2 = "Simstrat", key3 = "a_seiche")

## Set output file
outfile <- paste0("output_", "BigMoose_cali")
input_yaml_multiple(file = ler_yaml, outfile, key1 = "output", key2 = "file")

export_config(config_file = config_file, model = model)

# run model ensemble
run_ensemble(config_file = config_file, model = model, parallel = TRUE)


plot_heatmap("output/output_BigMoose_cali.nc") +
  theme_bw(base_size = 12) + 
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + 
  labs(x = "Date", y = "Depth (m)", color = "°C", title = meta$lake.name[i])


i = 1
fmd <- list()
nc_cb <- list()
for(i in 1:nrow(meta)){
  ncdf <- paste0("output/output_ice_", gsub(" ", "", meta$lake.name[i]), ".nc")
  
  model <- c("GLM", "Simstrat")
  fitmet <- calc_fit(ncdf = ncdf, model = model, var = "temp")
  fitmetdf <- reshape2::melt(fitmet)
  fitmetdf$lake <- meta$lake.name[i]
  
  fmd[[i]] <- fitmetdf
  
  wt <- load_var(ncdf, var = "temp")
  
  wt2 <- lapply(1:length(wt), function(x){
    wt[[x]] %>% tidyr::gather(key = wtr, value = temp, starts_with("wtr_")) %>% 
      tidyr::separate(wtr, into = c("wtr", "depth_m"), sep = "_") %>% 
      select(-wtr) %>% mutate(model = names(wt[x]), depth_m = as.numeric(depth_m))
  })
  wt2 <- do.call(rbind, wt2)
  
  bathy <- approx.bathy(meta$max.depth[i], meta$SA.ha[i]*1e5, meta$mean.depth[i])
  colnames(bathy) <- c("Depth_meter", "Area_meterSquared")
  nc_cb[[i]] <- wt2 %>% filter(!is.na(temp)) %>% 
    group_by(datetime, model) %>% 
    summarize(cb = center.buoyancy(temp, as.numeric(depth_m)), 
              ss = schmidt.stability(temp, depth_m, bthA = bathy$Area_meterSquared, bthD = bathy$Depth_meter)[,1], 
              et = epi.temperature(temp, depth_m, bthA = bathy$Area_meterSquared, bthD = bathy$Depth_meter),
              ht = hypo.temperature(temp, depth_m, bthA = bathy$Area_meterSquared, bthD = bathy$Depth_meter),
              dt = min(diff(temp))) 
}


# nc_cb[[1]]
# 
# ggplot(filter(nc_cb[nc_cb$dt < -1,], model != "GLM"), aes(x = (datetime), y = ht, color = model)) + geom_point() + 
#   scale_color_viridis_d() + facet_wrap(~model)
# 
# nc_cb %>% select(datetime, model, ss) %>% 
#   spread(key = model, value = ss) %>% 
#   filter(!is.na(Obs)) %>% 
#   ggplot(aes(x = Obs, y = Simstrat)) + geom_point() + 
#   geom_abline(a= 0, b = 1)
# 

data.table::rbindlist(fmd) %>% 
  filter(variable == "rmse") %>% 
  ggplot(aes(x = reorder(lake, value), y = value, color = L1)) + 
  geom_point() + coord_flip() + 
  geom_hline(yintercept = 2) + 
  scale_color_viridis_d(end = 0.8) + 
  theme_bw() + 
  labs(color = "", y = "RMSE", x = "")


data.table::rbindlist(fmd) %>% 
  filter(variable == "rmse") %>%
  left_join(meta, by = c("lake" = "lake.name")) %>% 
  ggplot(aes(x = mean.depth, y = value, color = L1)) + 
  geom_point()  + 
  scale_color_viridis_d(end = 0.8) + 
  theme_bw() + 
  labs(x = "Mean depth", y = "RMSE", color = "")

nc_cb <- lapply(1:length(nc_cb), function(x){
  nc_cb[[x]]$lake <- meta$lake.name[x]
  return(nc_cb[[x]])
})

nc_cb <- data.table::rbindlist(nc_cb)



ssfit <- nc_cb %>% filter(month(datetime) %in% 6:8) %>% 
  group_by(yr = year(datetime), lake, model) %>% 
  summarize(ss = mean(ss, na.rm = TRUE)) %>% 
  filter(model == "Simstrat") %>% 
  group_by(lake) %>% nest() %>% 
  mutate(fit = purrr::map(data, ~ lm(ss ~ yr, data = .x))) %>% 
  mutate(slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  mutate(p = purrr::map_dbl(fit, function(x){summary(x)$coefficients[2,4]})) %>% 
  select(lake, slp, p) %>% arrange(desc(slp))


nc_cb %>% filter(month(datetime) %in% 6:8) %>% 
  group_by(yr = year(datetime), lake, model) %>% 
  summarize(ss = mean(cb, na.rm = TRUE)) %>% 
  filter(model == "Simstrat") %>% 
  mutate(lake = factor(lake, levels = ssfit$lake)) %>% 
  ggplot(aes(x = yr, y = ss)) + geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~lake, scales = "free_y", ncol = 6) + 
  theme_bw() + 
  labs(x = "Year", y = "Schmidt Stability")


nc_cb %>% filter(month(datetime) %in% 6:8) %>% 
  group_by(yr = year(datetime), lake, model) %>% 
  summarize(ss = mean(ss, na.rm = TRUE)) %>% 
  filter(model == "Simstrat") %>% 
  group_by(lake) %>% nest() %>% 
  mutate(fit = purrr::map(data, ~ lm(ss ~ yr, data = .x))) %>% 
  mutate(slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  mutate(p = purrr::map_dbl(fit, function(x){summary(x)$coefficients[2,4]})) %>% 
  select(lake, slp, p) %>% left_join(meta, by = c("lake" = "lake.name")) %>% 
  ggplot(aes(x = max.depth, y = slp, color = p <= 0.05)) + geom_point()


nc_cb %>% filter(month(datetime) %in% 6:8) %>% 
  group_by(yr = year(datetime), lake, model) %>% 
  summarize(ss = mean(et,na.rm = TRUE)) %>% 
  filter(model == "Simstrat") %>% 
  group_by(lake) %>% nest() %>% 
  mutate(fit = purrr::map(data, ~ lm(ss ~ yr, data = .x))) %>% 
  mutate(slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  mutate(p = purrr::map_dbl(fit, function(x){summary(x)$coefficients[2,4]})) %>% 
  select(lake, slp, p) %>% left_join(meta, by = c("lake" = "lake.name")) %>% 
  ggplot(aes(x = max.depth, y = slp, color = p <= 0.05)) + geom_point()

nc_cb %>% filter(month(datetime) %in% 6:8) %>% 
  group_by(yr = year(datetime), lake, model) %>% 
  summarize(ss = mean(cb, na.rm = TRUE)) %>% 
  filter(model == "Simstrat") %>% 
  group_by(lake) %>% nest() %>% 
  mutate(fit = purrr::map(data, ~ lm(ss ~ yr, data = .x))) %>% 
  mutate(slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  mutate(p = purrr::map_dbl(fit, function(x){summary(x)$coefficients[2,4]})) %>% 
  select(lake, slp, p) %>% left_join(meta, by = c("lake" = "lake.name")) %>% 
  ggplot(aes(x = max.depth, y = slp, color = p < 0.05)) + geom_point()



nc_cb %>% filter(month(datetime) %in% 6:8) %>% 
  group_by(yr = year(datetime), lake, model) %>% 
  summarize(ss = mean(ht, na.rm = TRUE)) %>% 
  filter(model == "Simstrat") %>% 
  group_by(lake) %>% nest() %>% 
  mutate(fit = purrr::map(data, ~ lm(ss ~ yr, data = .x))) %>% 
  mutate(slp = purrr::map_dbl(fit, function(x){coef(x)[2]})) %>% 
  mutate(p = purrr::map_dbl(fit, function(x){summary(x)$coefficients[2,4]})) %>% 
  select(lake, slp, p) %>% left_join(meta, by = c("lake" = "lake.name")) %>% 
  ggplot(aes(x = max.depth, y = slp, color = p < 0.05)) + geom_point()



era5 <- arrow::read_parquet("era5_adk_1980-2022.parquet") %>% data.table::as.data.table()

library(gganimate)

p1 <- era5 %>% 
  mutate(month = floor_date(datetime, "1 week")) %>% 
  group_by(lat, lon, month) %>% 
  summarize(temp = mean(Air_Temperature_celsius)) %>% 
  ggplot() + 
  geom_tile(aes(x = lon, y = lat, fill = temp)) + 
  geom_sf(data = adkalt2, fill = NA) + 
  scale_fill_viridis_c(option = "B") + 
  transition_states(month) +
  labs(title = "{closest_state}")

animate(p1, nframes = 3000)


