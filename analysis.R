### These are the libraries I use in the following script

library(adklakedata)

library(dplyr)
library(tidyr)
library(lubridate)

library(purrr)
library(mgcv)
library(randomForest)

library(ggplot2)


# Load the chemistry and nutrient data sets
chem <- adk_data("chem")
nut <- adk_data("nutrient")

# Look at the first six rows of each data set
head(chem)
head(nut)

# The Fe column in nut is a factor, so here it is converted into a number
nut$Fe <- as.numeric(as.character(nut$Fe))

# This is a random forest model using the different variables in nut to predict chlorophyll
# Probably not a terribly useful analysis of the data
# random forest does not like missing data, so the data argument uses only complete observations nut[complete.cases(nut),]
rf <- randomForest(chl.a.ug.L ~ lake.name + year + TotalP.ug.L + MRP.ug.L + TFP.ug.L + TotalN + Fe, data = nut[complete.cases(nut),])
# view the random forest
rf
# look at the importance of different variables
varImpPlot(rf)

# combining the nut and chem data sets
allchem <- nut %>% group_by(lake.name, year, month) %>% 
  summarize_at(.vars = vars(TotalP.ug.L:Fe), mean, na.rm = TRUE) %>% 
  left_join(select(chem, -c(1,3)), by = c("lake.name", "year", "month"))

# Re running the random forest on chlorophyll using all the variables in the combined dataset
# maybe interesting, maybe not
rf <- randomForest(chl.a.ug.L ~ ., data = allchem[complete.cases(allchem),], importance = TRUE, ntree = 1000)
# view random forest
rf
# view importance of the different predictor variables
varImpPlot(rf)

# double checking year/month combinations for the chem and nut data sets
table(chem$year, chem$month)
table(nut$year, nut$month)


# load in the phytoplankton data set
phyt <- adk_data("phyto")
# look at the first few rows
head(phyt)
# check the year/month combinations
table(phyt$year, phyt$month)


# load in the crustacean and rotifer zooplankton data sets
crust <- adk_data("crustacean")
rot <- adk_data("rotifer")
rot$Taxa <- rot$Family # add a Taxa column to the rotifer dataset
rot$Family <- NULL # remove Family column

# combine crustacean and rotifer data into one zooplankton data set
zoop <- bind_rows(crust, rot) 
rm(rot);rm(crust) # remove rot and crust from memory

# check the year/month combinations
table(zoop$year, zoop$month)

# collect zooplankton data by group
zgrp <- zoop %>% 
  mutate(Group = case_when(Taxa == "Calanoid" ~ "Calanoid",  # convert Group == Copepod to Calanoid
                           Taxa == "Cyclopoid" ~ "Cyclopoid",# convert Group == Copepod to Cyclopoid 
                           TRUE ~ Group)) %>%  # leave all other group values the same
  group_by(lake.name, year, month, Group) %>% # group by lake, year month, and group
  summarize(org = sum(org.l)) %>% ungroup() %>%  # add up the number of organisms
  spread(key = Group, value = org, fill = 0) # spread the data set into wide format

# Plot the nut data from Big Moose lake
filter(nut, lake.name == "Big Moose") %>% 
  mutate(Fe = as.numeric(as.character(Fe))) %>% 
  gather(key = variable, value = value, Z.max:sample.depth, TotalP.ug.L:Fe) %>% 
  ggplot(aes(x = ymd(date), y = value)) + geom_point() + 
  facet_wrap(~variable, scales = "free_y")

# plotting the number of cells per ml over time for Big Moose lake by phytoplankton phylum
filter(phyt, lake.name == "Big Moose") %>% group_by(date, year, month, day, Phylum) %>% 
  #summarize(biovol = sum(biovol.um3.per.ml)) %>% ungroup() %>% 
  summarize(biovol = sum(cells.per.ml)) %>% ungroup() %>% 
  mutate(date = ymd(date)) %>% 
  #filter(!Phylum %in% c("unknown","Rhodophyta", "Euglenophyta")) %>% 
  filter(Phylum %in% phyla) %>% 
  ggplot(aes(x = date, y = log10(biovol))) + geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x)) + 
  facet_grid(.~Phylum) + theme_bw() + theme(panel.grid = element_blank()) 


# The 18 lakes that have all years of data
lake18 <- c("Big Moose", "Brooktrout", "Cascade", "Dart", "G", 
            "Indian", "Jockeybush", "Limekiln", "Moss", "North",
            "Rondaxe", "Sagamore", "South", "Squaw")

# The 5 phyla that are most common among phytoplankton
phyla <- c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Cyanobacteria", "Ochrophyta")


# just look at nutrients and phytoplankton from Big Moose
nut.bm <- filter(nut, lake.name == "Big Moose") %>% 
  mutate(Fe = as.numeric(as.character(Fe)), date = as.character(date))
phy.bm <- filter(phyt, lake.name == "Big Moose") %>% group_by(date, year, month, day, Phylum) %>% 
  summarize(biovol = sum(cells.per.ml)) %>% ungroup() %>% 
  mutate(date = as.character(date)) %>%
  filter(Phylum %in% phyla) %>% spread(key = Phylum, value = biovol, fill = 0)

# combine Big Moose phytoplankton and nutrients
phynut <- left_join(phy.bm, nut.bm, by = c("date", "year", "month", "day"))

# plotting nutrients against phytoplankton
phynut %>% gather(key = Phylum, value = cells, Bacillariophyta:Ochrophyta) %>% 
  ggplot(aes(x = TotalP.ug.L, y = log10(cells))) + geom_point() + facet_grid(.~Phylum)


# Here is a plot of the relationship between chlorophyll and phytoplankton 
filter(phyt, lake.name %in% lake18) %>% 
  group_by(lake.name, date, year, month, day, Phylum) %>% 
  summarize(biovol = sum(biovol.um3.per.ml)) %>% ungroup() %>%  # here and elsewhere use this for biovolume
  #summarize(biovol = sum(cells.per.ml)) %>% ungroup() %>%  # here and elsewhere use this for cells per ml
  left_join(nut, by = c("lake.name", "date", "year", "month", "day")) %>% 
  filter(Phylum %in% phyla) %>% 
  ggplot(aes(x = chl.a.ug.L, y = log10(biovol))) + geom_point() + 
  geom_smooth() + 
  facet_grid(Phylum~., scales = "free_y") +
  theme_bw() + theme(panel.grid = element_blank()) #+
  #labs(y = "Chlorophyll a (ug/L)", x = "Log10(Cells per mL)")

# Fitting linear models using total phosphorus, total nitrogen, and chlorophyll to predict biovolume (or cells)
# one model for each of the 5 phyla
phyfit <- filter(phyt, lake.name %in% lake18) %>% 
  group_by(lake.name, date, year, month, day, Phylum) %>% 
  summarize(biovol = sum(biovol.um3.per.ml)) %>% ungroup() %>% 
  #summarize(biovol = sum(cells.per.ml)) %>% ungroup() %>% 
  left_join(nut, by = c("lake.name", "date", "year", "month", "day")) %>% 
  filter(Phylum %in% phyla) %>% 
  group_by(Phylum) %>% 
  nest() %>% 
  mutate(fitchl = map(data, function(x) lm(log10(biovol)~TotalP.ug.L+TotalN+chl.a.ug.L, data = x)))

# a look at the output from each of the 5 models
summary(phyfit$fitchl[[1]])
summary(phyfit$fitchl[[2]])
summary(phyfit$fitchl[[3]])
summary(phyfit$fitchl[[4]])
summary(phyfit$fitchl[[5]])


# more model fitting, this time, predicting chlorophyta biovolume with nutrients and chlorophyll
# this time going lake by lake
phyfit <- filter(phyt, lake.name %in% lake18) %>% 
  group_by(lake.name, date, year, month, day, Phylum) %>% 
  summarize(biovol = sum(biovol.um3.per.ml)) %>% ungroup() %>% 
  #summarize(biovol = sum(cells.per.ml)) %>% ungroup() %>% 
  left_join(nut, by = c("lake.name", "date", "year", "month", "day")) %>% 
  filter(Phylum %in% phyla) %>% spread(key = Phylum, value = biovol, fill = 0) %>% 
  group_by(lake.name) %>% nest() %>% 
  #mutate(fit = map(data, function(x) lm(chl.a.ug.L ~ Bacillariophyta + Chlorophyta + Cryptophyta + 
  #                                        Cyanobacteria + Ochrophyta, data = x)))
  mutate(fit = map(data, function(x) lm(Chlorophyta ~ chl.a.ug.L + TotalP.ug.L + TotalN, data = x)))
  
# using the broom package to look at model fits
phyfit %>% mutate(mod = map(fit, broom::tidy)) %>% unnest(mod) # significance of each predictor
phyfit %>% mutate(mod = map(fit, broom::glance)) %>% unnest(mod) # fit of each model
# look at the r.squared, fair to good for some but terrible for others

# another look at chlorophyll a and cell count by phyla; Big Moose lake only
phynut %>% gather(key = Phylum, value = cells, Bacillariophyta:Ochrophyta) %>% 
  ggplot(aes(x = chl.a.ug.L, y = (cells))) + geom_point() + facet_grid(Phylum~., scales = "free_y")

# combining the phytoplankton, nutrient/chemistry datasets, and zooplankton data sets all together
allphy <- phyt %>% 
  group_by(lake.name, year, month, Phylum) %>% 
  #summarize(biovol = log10(sum(biovol.um3.per.ml))) %>% ungroup() %>% 
  summarize(biovol = log10(sum(cells.per.ml))) %>% ungroup() %>% 
  spread(key = "Phylum", value = biovol, fill = 0) %>% 
  left_join(allchem, by = c("lake.name", "year", "month")) %>% 
  left_join(zgrp, by = c("lake.name", "year", "month"))


# This section runs a series of random forest models, one for each phytoplankton phylum
# Because a random forest is stochastic (random), it is often wise to run many random forests
# This code runs 10 random forests, and for each one saves the importance for each variable
# A boxplot is plotted at the end to identify the variability in each variable's importance

incmse <- list()
rsq <- c()
for(i in 1:10){
  rf <- randomForest(Chlorophyta ~ month + TotalP.ug.L + MRP.ug.L + TFP.ug.L + TotalN + chl.a.ug.L + 
                       Fe + SO4_minus2 + NO3_minus + CL + Fluor + DIC + DOC + SiO2 + Calcium + Mg + Sodium + 
                       Potassium + NH4_plus + pH + conduct.uS.cm + Calanoid + Cladoceran + Cyclopoid + 
                       Rotifera, data = allphy[complete.cases(allphy),], ntree = 1000,
                     importance = TRUE)
  incmse[[i]] <- importance(rf)[,1]
  
  rsq[[i]] <- median(rf$rsq)
  print(i)
}
rf
boxplot(do.call(rbind, incmse))
rev(sort(apply(do.call(rbind, incmse), 2, median)))

incmse <- list()
rsq <- c()
for(i in 1:10){
  rf <- randomForest(Bacillariophyta ~ month + TotalP.ug.L + MRP.ug.L + TFP.ug.L + TotalN + chl.a.ug.L + 
                       Fe + SO4_minus2 + NO3_minus + CL + Fluor + DIC + DOC + SiO2 + Calcium + Mg + Sodium + 
                       Potassium + NH4_plus + pH + conduct.uS.cm + Calanoid + Cladoceran + Cyclopoid + 
                       Rotifera, data = allphy[complete.cases(allphy),], ntree = 1000,
                     importance = TRUE)
  incmse[[i]] <- importance(rf)[,1]
  rsq[[i]] <- median(rf$rsq)
  print(i)
}
rf
boxplot(do.call(rbind, incmse))
rev(sort(apply(do.call(rbind, incmse), 2, median)))

incmse <- list()
rsq <- c()
for(i in 1:10){
  rf <- randomForest(Cyanobacteria ~ month + TotalP.ug.L + MRP.ug.L + TFP.ug.L + TotalN + chl.a.ug.L + 
                       Fe + SO4_minus2 + NO3_minus + CL + Fluor + DIC + DOC + SiO2 + Calcium + Mg + Sodium + 
                       Potassium + NH4_plus + pH + conduct.uS.cm + Calanoid + Cladoceran + Cyclopoid + 
                       Rotifera, data = allphy[complete.cases(allphy),], ntree = 1000,
                     importance = TRUE)
  incmse[[i]] <- importance(rf)[,1]
  rsq[[i]] <- median(rf$rsq)
  print(i)
}
rf
boxplot(do.call(rbind, incmse))
rev(sort(apply(do.call(rbind, incmse), 2, median)))

incmse <- list()
rsq <- c()
for(i in 1:10){
  rf <- randomForest(Cryptophyta ~ month + TotalP.ug.L + MRP.ug.L + TFP.ug.L + TotalN + chl.a.ug.L + 
                       Fe + SO4_minus2 + NO3_minus + CL + Fluor + DIC + DOC + SiO2 + Calcium + Mg + Sodium + 
                       Potassium + NH4_plus + pH + conduct.uS.cm, data = allphy[complete.cases(allphy),], ntree = 1000,
                     importance = TRUE)
  incmse[[i]] <- importance(rf)[,1]
  rsq[[i]] <- median(rf$rsq)
  print(i)
}
rf
boxplot(do.call(rbind, incmse))
rev(sort(apply(do.call(rbind, incmse), 2, median)))

incmse <- list()
rsq <- c()
for(i in 1:10){
  rf <- randomForest(Ochrophyta ~ month + TotalP.ug.L + MRP.ug.L + TFP.ug.L + TotalN + chl.a.ug.L + 
                       Fe + SO4_minus2 + NO3_minus + CL + Fluor + DIC + DOC + SiO2 + Calcium + Mg + Sodium + 
                       Potassium + NH4_plus + pH + conduct.uS.cm, data = allphy[complete.cases(allphy),], ntree = 1000,
                     importance = TRUE)
  incmse[[i]] <- importance(rf)[,1]
  rsq[[i]] <- median(rf$rsq)
  print(i)
}
rf
boxplot(do.call(rbind, incmse))
rev(sort(apply(do.call(rbind, incmse), 2, median)))



#### USING GAMS ####

# Plot of each of the 5 phyla for 18 lakes over time
# for each phyla/lake combo a GAM is included with the geom_smooth() function
filter(phyt, lake.name %in% lake18) %>% 
  group_by(lake.name, date, year, month, day, Phylum) %>% 
  #summarize(biovol = sum(biovol.um3.per.ml)) %>% ungroup() %>% 
  summarize(biovol = sum(cells.per.ml)) %>% ungroup() %>% 
  mutate(date = ymd(date)) %>% 
  #filter(!Phylum %in% c("unknown","Rhodophyta", "Euglenophyta")) %>% 
  filter(Phylum %in% phyla) %>% 
  ggplot(aes(x = date, y = log10(biovol))) + geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x)) + 
  facet_grid(lake.name~Phylum) + theme_bw() + theme(panel.grid = element_blank()) 

# Groups the data by lake and phyla, then fits a GAM to each combination
test <- filter(phyt, lake.name %in% lake18) %>% 
  group_by(lake.name, date, year, month, day, Phylum) %>% 
  summarize(cells = sum(cells.per.ml)) %>% ungroup() %>% 
  mutate(dates = ymd(date)) %>% 
  filter(Phylum %in% phyla) %>% 
  mutate(logcells = log10(cells)) %>%
  group_by(lake.name, Phylum) %>% nest() %>% 
  mutate(fitgam = map(data, function(x){gam(logcells ~ s(year), data = x)}))

# examine the predictors for the first fitted model
broom::tidy(test$fitgam[[1]])
# examine the predictors for all the fitted models
test %>% mutate(mod = map(fitgam, function(x) broom::tidy(x))) %>% unnest(mod)
# examine the fit for all the fitted models
# this is more for comparison when we start fitting more complex models
test %>% mutate(mod = map(fitgam, function(x) broom::glance(x))) %>% unnest(mod)
