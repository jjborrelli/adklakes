library(LakeEnsemblR)
library(rLakeAnalyzer)
library(ggplot2)
library(patchwork)
t1 <- Sys.time()
ler_yaml <- "LakeEnsemblR.yaml"

meta <- meta[meta$max.depth > 5,]
for(i in 1:nrow(meta)){
  
  # i = 3
  # Location data
  input_yaml_multiple(file = ler_yaml, meta$lake.name[i], key1 = "location", key2 = "name")
  input_yaml_multiple(file = ler_yaml, meta$lat[i], key1 = "location", key2 = "latitude")
  input_yaml_multiple(file = ler_yaml, meta$long[i], key1 = "location", key2 = "longitude")
  input_yaml_multiple(file = ler_yaml, meta$elevation.m[i], key1 = "location", key2 = "elevation")
  input_yaml_multiple(file = ler_yaml, meta$max.depth[i], key1 = "location", key2 = "depth")
  
  bathy <- approx.bathy(meta$max.depth[i], meta$SA.ha[i]*1e5, meta$mean.depth[i])
  colnames(bathy) <- c("Depth_meter", "Area_meterSquared")
  write.csv(bathy, "lake_hyps.csv", row.names = FALSE)
  
  
  input_yaml_multiple(file = ler_yaml, "lake_hyps.csv", key1 = "location", key2 = "hypsograph")
  input_yaml_multiple(file = ler_yaml, meta$max.depth[i], key1 = "location", key2 = "init_depth")
  
  
  # input
  
  laketdo <- filter(tdo, lake.name == meta$lake.name[i])
  initlake <- filter(laketdo, date == min(laketdo$date), depth <= meta$max.depth[i]) %>% select(deps = depth, temp)
  write.csv(initlake, "init_profile.csv", row.names = FALSE)
  
  ## initial prof
  input_yaml_multiple(file = ler_yaml, "init_profile.csv", key1 = "input", key2 = "init_temp_profile", key3 = "file")
  
  # light ext
  
  kdval <- secchi %>% group_by(lake.name) %>% 
    summarize(kd = median(1.7/secchi, na.rm = TRUE))
  
  input_yaml_multiple(file = ler_yaml, round(kdval$kd[kdval$lake.name %in% meta$lake.name[i]], 2),
                      key1 = "input", key2 = "light", key3 = "Kw")
  
  ## start date
  input_yaml_multiple(file = ler_yaml, min(laketdo$date), key1 = "time", key2 = "start")
  
  ## end date
  input_yaml_multiple(file = ler_yaml, "1999-12-31 00:00:00", key1 = "time", key2 = "stop")
  
  
  ## Set meteorology driver file
  met <- read.csv("LakeEnsemblR_meteo_standard_narr.csv")
  met2 <- met %>% filter(year(ymd_hms(datetime)) >= 1994, year(ymd_hms(datetime)) < 2001)
  
  write.csv(met2, "LakeEnsemblr_meteo_narr_sub.csv", row.names = FALSE)
  
  input_yaml_multiple(file = ler_yaml, "LakeEnsemblr_meteo_narr_sub.csv", key1 = "meteo", key2 = "file")
  
  # output
  ## Set output file
  outfile <- paste0("output_", gsub(" ", "", meta$lake.name[i]))
  input_yaml_multiple(file = ler_yaml, outfile, key1 = "output", key2 = "file")
  
  # params
  
  ## Assume circle for now
  fetch <- sqrt((meta$SA.ha[i] * 1e5)/pi)*2 
  blen <- sqrt((meta$SA.ha[i] * 1e5)/pi)*2
  bwid <- sqrt((meta$SA.ha[i] * 1e5)/pi)*2
  
  input_yaml_multiple(file = ler_yaml, fetch, key1 = "model_parameters", key2 = "FLake", key3 = "fetch_lk")
  input_yaml_multiple(file = ler_yaml, blen, key1 = "model_parameters", key2 = "GLM", key3 = "bsn_len")
  input_yaml_multiple(file = ler_yaml, bwid, key1 = "model_parameters", key2 = "GLM", key3 = "bsn_wid")
  
  
  
  config_file <- ler_yaml
  model <- c("FLake", "GLM", "GOTM", "Simstrat")
  
  export_config(config_file = config_file, model = model)
  
  run_ensemble(config_file = config_file, model = model, parallel = TRUE)
  
  
}

t2 <- Sys.time()
t2-t1

i = 15
ncdf <- paste0("output/output_", gsub(" ", "", meta$lake.name[i]), ".nc")


plot_heatmap(ncdf) +
  theme_bw(base_size = 12) + 
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + 
  labs(x = "Date", y = "Depth (m)", color = "°C")


