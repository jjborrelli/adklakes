
hulcdf <- function(hudf, lctif){
  
  classlevels <- c("Barren Land (Rock/Sand/Clay)",
                   "Developed, Open Space", 
                   "Developed, Low Intensity", 
                   "Developed, Medium Intensity", 
                   "Developed High Intensity", 
                   "Pasture/Hay", 
                   "Cultivated Crops",
                   "Grassland/Herbaceous", 
                   "Shrub/Scrub",
                   "Deciduous Forest",
                   "Evergreen Forest", 
                   "Mixed Forest", 
                   "Woody Wetlands", 
                   "Emergent Herbaceous Wetlands", 
                   "Open Water")
  
  n2df <- list()
  for(i in 1:nrow(hudf)){
    wbd_i <- hudf[i,]
    a <- st_transform(wbd_i, st_crs(lctif))
    n1 <- raster::crop(lctif, a)
    n2 <- raster::mask(n1, a)
    
    n2df[[i]] <- raster::as.data.frame(n2, xy = TRUE) %>% 
      filter(!is.na(Class_Class)) %>% 
      mutate(Class_Class = factor(Class_Class, levels = classlevels), 
             n = n()) %>% 
      group_by(Class_Class) %>% 
      summarize(n2 = n(), n = mean(n)) %>%
      ungroup() %>% mutate(prop = n2/n) %>% 
      dplyr::select(Class_Class, prop) %>% spread(key = Class_Class, value = prop) %>% 
      janitor::clean_names()
    
    
    # print(i)
  }
  return(n2df)
}



huelevdf <- function(hudf, elevtif){
  
  n2df <- list()
  for(i in 1:nrow(hudf)){
    wbd_i <- hudf[i,]
    a <- st_transform(wbd_i, st_crs(elevtif))
    n1 <- raster::crop(elevtif, a)
    n2 <- raster::mask(n1, a)
    
    n2df[[i]] <- raster::as.data.frame(n2, xy = TRUE) %>% 
      summarize(elev_mean = mean(elev_mosaic), 
                elev_median = median(elev_mosaic),
                elev_min = min(elev_mosaic), 
                elev_max = max(elev_mosaic),
                elev_q25 = quantile(elev_mosaic, probs = 0.25), 
                elev_q75 = quantile(elev_mosaic, probs = 0.75),
                elev_sd = sd(elev_mosaic))
    
    # print(i)
  }
  return(n2df)
}


hulake <- function(hudf, lakes){
  lakes$lakearea <- st_area(lakes)
  test2 <- st_intersection(lakes, hudf)
  
  test3 <- test2 %>% as.data.frame() %>% 
    group_by(name) %>% 
    summarize(nlakes = n(),
              tlaksam = sum(aeap == 1 | als == 1 | epa_time == 1 | ny_cslap == 1 | 
                              els == 1 | emap == 1 | nylci == 1 | altm == 1 | awi == 1),
              aeap = sum(aeap), als = sum(als), 
              epa_time = sum(epa_time), nycslap = sum(ny_cslap), els = sum(els), 
              emap = sum(emap), nylci = sum(nylci), altm = sum(altm), 
              awi = sum(awi), tot_samp_prog = sum(aeap, als, epa_time, nycslap, 
                                                  els, emap, nylci, altm, awi),
              mean_lake_area = mean(lakearea), min_lake_area = min(lakearea),
              max_lake_area = max(lakearea),
              mean_lake_meandepth = mean(best_meandepth, na.rm = TRUE),
              mean_lake_maxdepth = mean(best_maxdepth, na.rm = TRUE),
              mean_lake_median_srp = mean(medianvalue_srp, na.rm = TRUE),
              mean_lake_median_tp = mean(medianvalue_tp, na.rm = TRUE),
              mean_lake_median_tn = mean(medianvalue_tn, na.rm = TRUE),
              mean_lake_median_doc = mean(medianvalue_doc, na.rm = TRUE),
              mean_lake_median_chla = mean(medianvalue_chla, na.rm = TRUE)
              )
  
  return(test3)
  
}

