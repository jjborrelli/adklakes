# to build probabilistic allometric niche model for freshwater lakes
# use draws from residuals of regressions to generate distribution of niche model params
wdata <- data.table::fread("E:/JeffersonProject/SppData/freshwater_bodysize.csv") %>% 
  filter(`Mean mass (g) consumer` != -999, `Mean mass (g) resource` != -999)
brosedata <- data.table::fread("E:/JeffersonProject/SppData/283_2_FoodWebDataBase_2018_12_10.csv")
colnames(brosedata)
table(brosedata$ecosystem.type)

broselake <- brosedata %>% 
  filter(ecosystem.type == "lakes", 
         res.mass.mean.g. != -999, 
         con.mass.mean.g. != -999)

plot(log10(broselake$res.mass.mean.g.) ~ log10(broselake$con.mass.mean.g.))

plot(c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)), 
     c(log10(wdata$`Mean mass (g) resource`),log10(broselake$res.mass.mean.g.)))

library(quantreg)

# mean_reg = lm(Bprey~Bpred)			# For the n parameter
mean_reg <- lm(log10(broselake$res.mass.mean.g.) ~ log10(broselake$con.mass.mean.g.))
# qrsup = rq(Bprey~Bpred,tau = quartil[2])	# For the higher limit of the range
qrsup <- rq(log10(broselake$res.mass.mean.g.)~log10(broselake$con.mass.mean.g.),tau = 0.05)
# qrinf = rq(Bprey~Bpred,tau = quartil[1])	# For the lower limit of the range
qrinf <- rq(log10(broselake$res.mass.mean.g.)~log10(broselake$con.mass.mean.g.),tau = 0.95)


plot(log10(broselake$res.mass.mean.g.) ~ log10(broselake$con.mass.mean.g.))
points(predict(mean_reg) ~ c(log10(broselake$con.mass.mean.g.)), col = "blue", typ = 'l')
points(predict(qrsup) ~ c(log10(broselake$con.mass.mean.g.)), col = "blue", typ = 'l', lty = 3)
points(predict(qrinf) ~ c(log10(broselake$con.mass.mean.g.)), col = "blue", typ = 'l', lty = 3)

mean_reg2 <- lm(log10(broselake$res.mass.mean.g.) ~ log10(broselake$con.mass.mean.g.) * broselake$con.metabolic.type)

summary(mean_reg)
summary(mean_reg2)

mean_reg3 <- lm(c(log10(wdata$`Mean mass (g) resource`),log10(broselake$res.mass.mean.g.)) ~ 
                  c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)))
summary(mean_reg3)

qrs3 <- rq(c(log10(wdata$`Mean mass (g) resource`),log10(broselake$res.mass.mean.g.)) ~ 
                  c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)),tau = 0.05)

qri3 <- rq(c(log10(wdata$`Mean mass (g) resource`),log10(broselake$res.mass.mean.g.)) ~ 
                  c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)),tau = 0.95)

plot(c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)), 
     c(log10(wdata$`Mean mass (g) resource`),log10(broselake$res.mass.mean.g.)))
points(predict(mean_reg3) ~ c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)), col = "blue", typ = 'l')
points(predict(qri3) ~ c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)), col = "blue", typ = 'l', lty = 3)
points(predict(qrs3) ~ c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)), col = "blue", typ = 'l', lty = 3)

ggplot(filter(broselake, con.metabolic.type != "endotherm vertebrate"), aes(x = con.mass.mean.g., y = res.mass.mean.g.)) + geom_point() +
  facet_grid(con.metabolic.type ~ .) + 
  scale_y_log10() + scale_x_log10()


tdf <- data.frame(log10_res_mass_g = c(log10(wdata$`Mean mass (g) resource`),log10(broselake$res.mass.mean.g.)),
                  log10_con_mass_g = c(log10(wdata$`Mean mass (g) consumer`), log10(broselake$con.mass.mean.g.)),
                  con_met_type = c(wdata$`Metabolic category consumer`, broselake$con.metabolic.type)) %>% 
  filter(con_met_type != "endotherm vertebrate")

tdf

qmod1 <- rq(log10_res_mass_g ~ log10_con_mass_g * con_met_type, tau = 0.05, data = tdf)
qmod2 <- rq(log10_res_mass_g ~ log10_con_mass_g * con_met_type, tau = 0.95, data = tdf)
qmod3 <- lm(log10_res_mass_g ~ log10_con_mass_g * con_met_type, data = tdf)


ggplot(tdf, aes(x = log10_con_mass_g, y = log10_res_mass_g)) + 
  geom_point() + 
  geom_line(data = broom::augment(qmod3), aes(y = .fitted), lty = 2, color = "blue", linewidth = 2) + 
  geom_line(data = broom::augment(qmod2), aes(y = .fitted), lty = 2, color = "blue", linewidth = 2) +
  geom_line(data = broom::augment(qmod1), aes(y = .fitted), lty = 2, color = "blue", linewidth = 2) + 
  facet_wrap(~con_met_type)


ggplot(tdf, aes(x = log10_con_mass_g, y = log10_res_mass_g)) + 
  geom_point() + 
  geom_abline(data = (qmod3), lty = 2, color = "blue", linewidth = 2) + 
  geom_abline(data = (qmod2), lty = 2, color = "blue", linewidth = 2) +
  geom_abline(data = (qmod1), lty = 2, color = "blue", linewidth = 2) + 
  facet_wrap(~con_met_type)




tdf <- data.frame(log10_res_mass_g = c(log10(broselake$res.mass.mean.g.)),
                  log10_con_mass_g = c(log10(broselake$con.mass.mean.g.)),
                  con_met_type = c(broselake$con.metabolic.type)) %>% 
  filter(con_met_type != "endotherm vertebrate")


qmod1 <- rq(log10_res_mass_g ~ log10_con_mass_g, tau = 0.05, data = tdf)
qmod2 <- rq(log10_res_mass_g ~ log10_con_mass_g, tau = 0.95, data = tdf)
qmod3 <- rq(log10_res_mass_g ~ log10_con_mass_g, tau = 0.5, data = tdf)


ggplot(tdf, aes(x = log10_con_mass_g, y = log10_res_mass_g)) + 
  geom_point() + 
  geom_line(data = broom::augment(qmod3, type.predict = "response"), aes(y = .fitted), lty = 2, color = "blue", linewidth = 2) + 
  geom_line(data = broom::augment(qmod2, type.predict = "response"), aes(y = .fitted), lty = 2, color = "blue", linewidth = 2) +
  geom_line(data = broom::augment(qmod1, type.predict = "response"), aes(y = .fitted), lty = 2, color = "blue", linewidth = 2) 


predict(qmod3, data.frame(log10_con_mass_g = log10(do.call(rbind, list(zA, zB))$ug_WW_ind/1e6)))
predict(qmod2, data.frame(log10_con_mass_g = log10(do.call(rbind, list(zA, zB))$ug_WW_ind/1e6)))
predict(qmod1, data.frame(log10_con_mass_g = log10(do.call(rbind, list(zA, zB))$ug_WW_ind/1e6)))

pA$l10biomass <- log10(pA$biovol_um3_cell/1e6/1e6)

preydfs <- list()
for(i in 1:nrow(rbind(zA,zB))){
  ci <- predict(qmod3, data.frame(log10_con_mass_g = log10(do.call(rbind, list(zA, zB))$ug_WW_ind/1e6)[i]))
  ui <- predict(qmod2, data.frame(log10_con_mass_g = log10(do.call(rbind, list(zA, zB))$ug_WW_ind/1e6)[i]))
  li <- predict(qmod1, data.frame(log10_con_mass_g = log10(do.call(rbind, list(zA, zB))$ug_WW_ind/1e6)[i]))
  
  preydfs[[i]] <- filter(pA, l10biomass > li, l10biomass < ui)
}

sapply(preydfs, nrow)
