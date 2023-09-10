### PACKAGES
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(sp)
library(spatstat)
library(maptools)
library(sf)
library(spatialreg)
library(rgdal)
library(spatstat)
library(raster)
library(tigris)
library(ggplot2)
library(broom)
library(tidyverse)
library(ggmap)
library(spdep)
library(lattice)
library(gstat)
library(maptools)
library(gridExtra)
### POINT REFERENCE ANALYSIS
ActiveSpinData <- read.csv("ActiveSpinData.csv")

PitcherMovementsFastball <- read.csv("PitcherMovementFastball.csv") %>% dplyr::select(-1)
library(janitor)
ActiveSpin <- ActiveSpinData %>%
  row_to_names(row_number = 1)

#create log active spin
PitchMovementsWithActiveSpinFastball <- left_join(PitcherMovementsFastball, ActiveSpin, by = c("player_name" = "Pitcher")) %>%
  mutate(logActiveSpin = log((as.numeric(Fourseam)/100)*meanspinrate)) %>%
  dplyr::select(c(2,3,15)) %>%
  unique()

#EDA
summary(PitchMovementsWithActiveSpinFastball$meanhorizontal)
summary(PitchMovementsWithActiveSpinFastball$meanvertical)
summary(PitchMovementsWithActiveSpinFastball$logActiveSpin)

#EDA plot of average horizontal and vertical movements of fastballs
ggplot(data = PitchMovementsWithActiveSpinFastball) + 
  geom_point(aes(x=meanhorizontal, y = meanvertical, col = logActiveSpin)) + 
  scale_color_distiller(palette = "Spectral")

#turn horizontal and vertical movements into coordinates to apply variograms
coordinates(PitchMovementsWithActiveSpinFastball) <- ~meanhorizontal + meanvertical
v <- variogram(logActiveSpin ~ 1, data = PitchMovementsWithActiveSpinFastball)
set.seed(999)
print(xyplot(gamma ~ dist, v, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
               for (i in 1:200) {                
                 PitchMovementsWithActiveSpinFastball$random = sample(PitchMovementsWithActiveSpinFastball$logActiveSpin) 
                 v = variogram(random ~ 1, PitchMovementsWithActiveSpinFastball)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             ylim = c(-.05, 0.1), 
             xlab = 'Distance', ylab = 'Semivariance'
))

show.vgms()

ggplot(data = v, aes(x = dist, y = gamma)) +
  geom_point(size = 3)


#compare variogram fits
vg_param_fastball1 <- variogramLine(vgm(psill = 0.075,
                                        model = "Gau",     
                                        range = 0.9,   
                                        nugget = 0.0012),     
                                    maxdist = 3)

ggplot(data = v, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_fastball1, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")

vg_param_fastball2 <- variogramLine(vgm(psill = 0.055,
                                        model = "Wav",     
                                        range = 0.9,   
                                        nugget = 0.0012),     
                                    maxdist = 3)

ggplot(data = v, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_fastball2, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_fastball3 <- variogramLine(vgm(psill = 0.05,
                                        model = "Pow",     
                                        range = 2,   
                                        nugget = 0.0012),     
                                    maxdist = 3)

ggplot(data = v, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_fastball3, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")

vg_param_fastball1$Type <- "Gau"
vg_param_fastball2$Type <- "Wav"
vg_param_fastball3$Type <- "Pow"
compare_variograms_fastball <- bind_rows(vg_param_fastball1, vg_param_fastball2, vg_param_fastball3)


ggplot(data = v, aes(x = dist, y = gamma)) +
  geom_point(size = 3) +
  geom_line(data = compare_variograms_fastball, aes(x = dist, y = gamma, col = Type), size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(col = "Variogram Type") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))



#fastball fit
v_fit <- fit.variogram(v, vgm(psill = 0.07,
                              model = "Gau",     
                              range = 0.9,   
                              nugget = 0.0012), fit.sills = FALSE, fit.ranges = FALSE, fit.method = FALSE)




fastball_grid <- expand.grid(lon = seq(PitchMovementsWithActiveSpinFastball@bbox[1,1],
                                       PitchMovementsWithActiveSpinFastball@bbox[1,2],length.out = 200), 
                             lat = seq(PitchMovementsWithActiveSpinFastball@bbox[2,1],
                                       PitchMovementsWithActiveSpinFastball@bbox[2,2],length.out = 200))  

sp_grid <- fastball_grid
coordinates(sp_grid) <- ~lon+lat
plot(sp_grid)

mean(PitchMovementsWithActiveSpinFastball@data$logActiveSpin)
fastball_sk <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinFastball, sp_grid, v_fit, beta = 7.607202) %>% as.data.frame()
fastball_ok <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinFastball, sp_grid, v_fit) %>% as.data.frame()


m <- vgm(psill = 0.07,
         model = "Gau",     
         range = 0.9,   
         nugget = 0.0012) 

#apply ordinary and simple kriging
set.seed(999)
fastball_ok_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinFastball, m, nfold=345)
bubble(fastball_ok_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(fastball_ok_cross$residual)
mean((fastball_ok_cross$residual)^2)


fastball_sk_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinFastball, beta = 7.607202, m, nfold=345)
bubble(fastball_sk_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(fastball_sk_cross$residual)
mean((fastball_sk_cross$residual)^2)


#ordinary kriging plot and error plot
ggplot() +
  geom_point(data = fastball_ok, aes(x = lon, y = lat, col = var1.pred),  shape = 15, size = 3) +
  labs(col = "Predicted Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "Spectral") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))

ggplot() +
  geom_point(data = fastball_ok, aes(x = lon, y = lat, col = var1.var),  shape = 15, size = 3) +
  labs(col = "Variance Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "PiYG") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))




#do the same with curveballs
PitcherMovementsCurveballs <- read.csv("PitcherMovementCurveballs.csv") %>% dplyr::select(-1)
library(janitor)
ActiveSpin <- ActiveSpinData %>%
  row_to_names(row_number = 1)

PitchMovementsWithActiveSpinCurveballs <- left_join(PitcherMovementsCurveballs, ActiveSpin, by = c("player_name" = "Pitcher")) %>%
  mutate(logActiveSpin = log((as.numeric(Curve)/100)*meanspinrate)) %>%
  dplyr::select(c(2,3,15)) %>%
  unique()

summary(PitchMovementsWithActiveSpinCurveballs$meanhorizontal)
summary(PitchMovementsWithActiveSpinCurveballs$meanvertical)
summary(PitchMovementsWithActiveSpinCurveballs$logActiveSpin)



ggplot(data = PitchMovementsWithActiveSpinCurveballs) + 
  geom_point(aes(x=meanhorizontal, y = meanvertical, col = logActiveSpin)) + 
  scale_color_distiller(palette = "Spectral")




#
coordinates(PitchMovementsWithActiveSpinCurveballs) <- ~meanhorizontal + meanvertical
v_curve <- variogram(logActiveSpin ~ 1, data = PitchMovementsWithActiveSpinCurveballs)
print(xyplot(gamma ~ dist, v_curve, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
               for (i in 1:200) {                
                 PitchMovementsWithActiveSpinCurveballs$random = sample(PitchMovementsWithActiveSpinCurveballs$logActiveSpin) 
                 v = variogram(random ~ 1, PitchMovementsWithActiveSpinCurveballs)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             ylim = c(-0.1, 0.5), 
             xlab = 'distance', ylab = 'semivariance'
))

show.vgms()

ggplot(data = v_curve, aes(x = dist, y = gamma)) +
  geom_point(size = 3)



vg_param_curve1 <- variogramLine(vgm(psill = 0.1,
                                     model = "Gau",     
                                     range = 0.6,   
                                     nugget = 0.001757067),     
                                 maxdist = 3)

ggplot(data = v_curve, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_curve1, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")

vg_param_curve2 <- variogramLine(vgm(psill = 0.085,
                                     model = "Wav",     
                                     range = 0.8,   
                                     nugget = 0.001757067),     
                                 maxdist = 3)

ggplot(data = v_curve, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_curve2, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")

vg_param_curve3 <- variogramLine(vgm(psill = 0.13,
                                     model = "Pow",     
                                     range = 2,   
                                     nugget = 0.001757067),     
                                 maxdist = 3)

ggplot(data = v_curve, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_curve3, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")

vg_param_curve1$Type <- "Gau"
vg_param_curve2$Type <- "Wav"
vg_param_curve3$Type <- "Pow"
compare_variograms_curve <- bind_rows(vg_param_curve1, vg_param_curve2, vg_param_curve3)


ggplot(data = v_curve, aes(x = dist, y = gamma)) +
  geom_point(size = 3) +
  geom_line(data = compare_variograms_curve, aes(x = dist, y = gamma, col = Type), size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(col = "Variogram Type")



v_fit_curve <- fit.variogram(v_curve, model = vgm(psill = 0.1,
                                                  model = "Gau",     
                                                  range = 0.6,   
                                                  nugget = 0.001757067), fit.sills = FALSE, fit.ranges = FALSE, fit.method = FALSE)




curve_grid <- expand.grid(lon = seq(PitchMovementsWithActiveSpinCurveballs@bbox[1,1],
                                    PitchMovementsWithActiveSpinCurveballs@bbox[1,2],length.out = 200), 
                          lat = seq(PitchMovementsWithActiveSpinCurveballs@bbox[2,1],
                                    PitchMovementsWithActiveSpinCurveballs@bbox[2,2],length.out = 200))  

sp_grid_curve <- curve_grid
coordinates(sp_grid_curve) <- ~lon+lat
plot(sp_grid_curve)

mean(PitchMovementsWithActiveSpinCurveballs@data$logActiveSpin)
curveball_sk <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinCurveballs, sp_grid_curve, v_fit_curve, beta = 7.437252) %>% as.data.frame()
curveball_ok <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinCurveballs, sp_grid_curve, v_fit_curve) %>% as.data.frame()


ggplot() +
  geom_point(data = curveball_ok, aes(x = lon, y = lat, col = var1.pred),  size = 0.8) +
  labs(col = "Predicted Active Spin")+
  scale_colour_distiller(palette = "Spectral")

ggplot() +
  geom_point(data = curveball_ok, aes(x = lon, y = lat, col = var1.var),  size = 0.8) +
  labs(col = "Predicted Active Spin")+
  scale_colour_distiller(palette = "Spectral")



m_curve <- vgm(psill = 0.1,
               model = "Gau",     
               range = 0.6,   
               nugget = 0.001757067) 

set.seed(999)
curve_ok_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinCurveballs, m_curve, nfold=119)
bubble(curve_ok_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(curve_ok_cross$residual)
mean((curve_ok_cross$residual)^2)


curve_sk_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinCurveballs, beta = 7.437252, m_curve, nfold=119)
bubble(curve_sk_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(curve_sk_cross$residual)
mean((curve_sk_cross$residual)^2)


PitcherMovementsSliders <- read.csv("PitcherMovementSliders.csv") %>% dplyr::select(-1)
library(janitor)
ActiveSpin <- ActiveSpinData %>%
  row_to_names(row_number = 1)

PitchMovementsWithActiveSpinSliders <- left_join(PitcherMovementsSliders, ActiveSpin, by = c("player_name" = "Pitcher")) %>%
  mutate(logActiveSpin = log((as.numeric(Slider)/100)*meanspinrate)) %>%
  dplyr::select(c(2,3,15)) %>%
  unique()


summary(PitchMovementsWithActiveSpinSliders$meanhorizontal)
summary(PitchMovementsWithActiveSpinSliders$meanvertical)
summary(PitchMovementsWithActiveSpinSliders$logActiveSpin)


#do the same with sliders
ggplot(data = PitchMovementsWithActiveSpinSliders) + 
  geom_point(aes(x=meanhorizontal, y = meanvertical, col = logActiveSpin)) + 
  scale_color_distiller(palette = "Spectral")


coordinates(PitchMovementsWithActiveSpinSliders) <- ~meanhorizontal + meanvertical
v_slider <- variogram(logActiveSpin ~ 1, data = PitchMovementsWithActiveSpinSliders)
print(xyplot(gamma ~ dist, v_slider, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
               for (i in 1:200) {                
                 PitchMovementsWithActiveSpinSliders$random = sample(PitchMovementsWithActiveSpinSliders$logActiveSpin) 
                 v = variogram(random ~ 1, PitchMovementsWithActiveSpinSliders)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             ylim = c(-0.1, 0.5), 
             xlab = 'distance', ylab = 'semivariance'
))

show.vgms()

ggplot(data = v_slider, aes(x = dist, y = gamma)) +
  geom_point(size = 3)



vg_param_slider1 <- variogramLine(vgm(psill = 0.29,
                                      model = "Gau",     
                                      range = 0.6,   
                                      nugget = 0.033),     
                                  maxdist = 3)


ggplot(data = v_slider, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_slider1, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_slider2 <- variogramLine(vgm(psill = 0.25,
                                      model = "Wav",     
                                      range = 0.9,   
                                      nugget = 0.033),     
                                  maxdist = 3)


ggplot(data = v_slider, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_slider2, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")



vg_param_slider3 <- variogramLine(vgm(psill = 0.3,
                                      model = "Pow",     
                                      range = 2,   
                                      nugget = 0.033),     
                                  maxdist = 3)


ggplot(data = v_slider, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_slider3, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")



vg_param_slider1$Type <- "Gau"
vg_param_slider2$Type <- "Wav"
vg_param_slider3$Type <- "Pow"
compare_variograms_sliders <- bind_rows(vg_param_slider1, vg_param_slider2, vg_param_slider3)


ggplot(data = v_slider, aes(x = dist, y = gamma)) +
  geom_point(size = 3) +
  geom_line(data = compare_variograms_sliders, aes(x = dist, y = gamma, col = Type), size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(col = "Variogram Type")


v_fit_slider <- fit.variogram(v_slider, model = vgm(psill = 0.29,
                                                    model = "Gau",     
                                                    range = 0.6,   
                                                    nugget = 0.033), fit.sills = FALSE, fit.ranges = FALSE, fit.method = FALSE)




slider_grid <- expand.grid(lon = seq(PitchMovementsWithActiveSpinSliders@bbox[1,1],
                                     PitchMovementsWithActiveSpinSliders@bbox[1,2],length.out = 200), 
                           lat = seq(PitchMovementsWithActiveSpinSliders@bbox[2,1],
                                     PitchMovementsWithActiveSpinSliders@bbox[2,2],length.out = 200))  

sp_grid_slider <- slider_grid
coordinates(sp_grid_slider) <- ~lon+lat
plot(sp_grid_slider)

mean(PitchMovementsWithActiveSpinSliders@data$logActiveSpin)
slider_sk <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSliders, sp_grid_slider, v_fit_slider, beta = 6.687262) %>% as.data.frame()
slider_ok <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSliders, sp_grid_slider, v_fit_slider) %>% as.data.frame()

m_slider <- vgm(psill = 0.29,
                model = "Gau",     
                range = 0.6,   
                nugget = 0.033) 

set.seed(999)
slider_ok_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSliders, m_slider, nfold=275)
bubble(slider_ok_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(slider_ok_cross$residual)
mean((slider_ok_cross$residual)^2)


slider_sk_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSliders, beta = 6.687262, m_slider, nfold=275)
bubble(slider_sk_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(slider_sk_cross$residual)
mean((slider_sk_cross$residual)^2)

ggplot() +
  geom_point(data = slider_ok, aes(x = lon, y = lat, col = var1.pred), shape = 15, size = 3) +
  labs(col = "Predicted Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "Spectral") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))

ggplot() +
  geom_point(data = slider_ok, aes(x = lon, y = lat, col = var1.var),shape = 15, size = 3) +
  labs(col = "Variance Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "PiYG") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))





#do the same with changeups
PitcherMovementsChangeups <- read.csv("PitcherMovementChangeups.csv") %>% dplyr::select(-1)
library(janitor)
ActiveSpin <- ActiveSpinData %>%
  row_to_names(row_number = 1)

PitchMovementsWithActiveSpinChangeups <- left_join(PitcherMovementsChangeups, ActiveSpin, by = c("player_name" = "Pitcher")) %>%
  mutate(logActiveSpin = log((as.numeric(Change)/100)*meanspinrate)) %>%
  dplyr::select(c(2,3,15)) %>%
  unique()

summary(PitchMovementsWithActiveSpinChangeups$meanhorizontal)
summary(PitchMovementsWithActiveSpinChangeups$meanvertical)
summary(PitchMovementsWithActiveSpinChangeups$logActiveSpin)



ggplot(data = PitchMovementsWithActiveSpinChangeups) + 
  geom_point(aes(x=meanhorizontal, y = meanvertical, col = logActiveSpin)) + 
  scale_color_distiller(palette = "Spectral")


coordinates(PitchMovementsWithActiveSpinChangeups) <- ~meanhorizontal + meanvertical
v_change <- variogram(logActiveSpin ~ 1, data = PitchMovementsWithActiveSpinChangeups)
print(xyplot(gamma ~ dist, v_change, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
               for (i in 1:200) {                
                 PitchMovementsWithActiveSpinChangeups$random = sample(PitchMovementsWithActiveSpinChangeups$logActiveSpin) 
                 v = variogram(random ~ 1, PitchMovementsWithActiveSpinChangeups)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             ylim = c(-0.01, 0.1), 
             xlab = 'distance', ylab = 'semivariance'
))



show.vgms()

ggplot(data = v_change, aes(x = dist, y = gamma)) +
  geom_point(size = 3)



vg_param_change1 <- variogramLine(vgm(psill = 0.045,
                                      model = "Gau",     
                                      range = 0.45,   
                                      nugget = 0.0064),     
                                  maxdist = 3)

ggplot(data = v_change, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_change1, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_change2 <- variogramLine(vgm(psill = 0.037,
                                      model = "Wav",     
                                      range = 0.55,   
                                      nugget = 0.0064),     
                                  maxdist = 3)

ggplot(data = v_change, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_change2, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_change3 <- variogramLine(vgm(psill = 0.08,
                                      model = "Pow",     
                                      range = 2,   
                                      nugget = 0.0064),     
                                  maxdist = 3)

ggplot(data = v_change, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_change3, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_change1$Type <- "Gau"
vg_param_change2$Type <- "Wav"
vg_param_change3$Type <- "Pow"
compare_variograms_change <- bind_rows(vg_param_change1, vg_param_change2, vg_param_change3)


ggplot(data = v_change, aes(x = dist, y = gamma)) +
  geom_point(size = 3) +
  geom_line(data = compare_variograms_change, aes(x = dist, y = gamma, col = Type), size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(col = "Variogram Type")




v_fit_change <- fit.variogram(v_change, model = vgm(psill = 0.045,
                                                    model = "Gau",     
                                                    range = 0.45,   
                                                    nugget = 0.0064), fit.sills = FALSE, fit.ranges = FALSE, fit.method = FALSE)




change_grid <- expand.grid(lon = seq(PitchMovementsWithActiveSpinChangeups@bbox[1,1],
                                     PitchMovementsWithActiveSpinChangeups@bbox[1,2],length.out = 200), 
                           lat = seq(PitchMovementsWithActiveSpinChangeups@bbox[2,1],
                                     PitchMovementsWithActiveSpinChangeups@bbox[2,2],length.out = 200))  

sp_grid_change <- change_grid
coordinates(sp_grid_change) <- ~lon+lat
plot(sp_grid_change)
mean(PitchMovementsWithActiveSpinChangeups@data$logActiveSpin)
change_sk <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinChangeups, sp_grid_change, v_fit_change, beta = 7.363968) %>% as.data.frame()
change_ok <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinChangeups, sp_grid_change, v_fit_change) %>% as.data.frame()
m_change <- vgm(psill = 0.045,
                model = "Gau",     
                range = 0.45,   
                nugget = 0.0064) 

set.seed(999)
change_ok_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinChangeups, m_change, nfold=159)
bubble(change_ok_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(change_ok_cross$residual)
mean((change_ok_cross$residual)^2)


change_sk_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinChangeups, beta = 7.363968, m_change, nfold=159)
bubble(change_sk_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(change_sk_cross$residual)
mean((change_sk_cross$residual)^2)

ggplot() +
  geom_point(data = change_ok, aes(x = lon, y = lat, col = var1.pred), shape = 15, size = 3) +
  labs(col = "Predicted Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement") +
  scale_colour_distiller(palette = "Spectral") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))

ggplot() +
  geom_point(data = change_ok, aes(x = lon, y = lat, col = var1.var),  shape = 15, size = 3) +
  labs(col = "Variance Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "PiYG") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))







#do the same with sinkers
PitcherMovementsSinkers <- read.csv("PitcherMovementSinkers.csv") %>% dplyr::select(-1)
library(janitor)
ActiveSpin <- ActiveSpinData %>%
  row_to_names(row_number = 1)

PitchMovementsWithActiveSpinSinkers <- left_join(PitcherMovementsSinkers, ActiveSpin, by = c("player_name" = "Pitcher")) %>%
  mutate(logActiveSpin = log((as.numeric(Sinker)/100)*meanspinrate)) %>%
  dplyr::select(c(2,3,15)) %>%
  unique()


summary(PitchMovementsWithActiveSpinSinkers$meanhorizontal)
summary(PitchMovementsWithActiveSpinSinkers$meanvertical)
summary(PitchMovementsWithActiveSpinSinkers$logActiveSpin)



ggplot(data = PitchMovementsWithActiveSpinSinkers) + 
  geom_point(aes(x=meanhorizontal, y = meanvertical, col = logActiveSpin)) + 
  scale_color_distiller(palette = "Spectral")


coordinates(PitchMovementsWithActiveSpinSinkers) <- ~meanhorizontal + meanvertical
v_sinker <- variogram(logActiveSpin ~ 1, data = PitchMovementsWithActiveSpinSinkers)
print(xyplot(gamma ~ dist, v_sinker, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
               for (i in 1:200) {                
                 PitchMovementsWithActiveSpinSinkers$random = sample(PitchMovementsWithActiveSpinSinkers$logActiveSpin) 
                 v = variogram(random ~ 1, PitchMovementsWithActiveSpinSinkers)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             ylim = c(-0.01, 0.1), 
             xlab = 'distance', ylab = 'semivariance'
))

show.vgms()

ggplot(data = v_sinker, aes(x = dist, y = gamma)) +
  geom_point(size = 3)



vg_param_sinker1 <- variogramLine(vgm(psill = 0.014,
                                      model = "Gau",     
                                      range = 0.4,   
                                      nugget = 0.0032),     
                                  maxdist = 3)

ggplot(data = v_sinker, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_sinker1, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_sinker2 <- variogramLine(vgm(psill = 0.012,
                                      model = "Wav",     
                                      range = 0.6,   
                                      nugget = 0.0032),     
                                  maxdist = 3)

ggplot(data = v_sinker, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_sinker2, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_sinker3 <- variogramLine(vgm(psill = 0.02,
                                      model = "Pow",     
                                      range = 2,   
                                      nugget = 0.0032),     
                                  maxdist = 3)

ggplot(data = v_sinker, aes(x = dist, y = gamma)) +
  geom_point(size = 3) + 
  geom_line(data = vg_param_sinker3, aes(x = dist, y = gamma), 
            col = "blue", size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(title = "Fitted variogram")


vg_param_sinker1$Type <- "Gau"
vg_param_sinker2$Type <- "Wav"
vg_param_sinker3$Type <- "Pow"
compare_variograms_sinker <- bind_rows(vg_param_sinker1, vg_param_sinker2, vg_param_sinker3)


ggplot(data = v_sinker, aes(x = dist, y = gamma)) +
  geom_point(size = 3) +
  geom_line(data = compare_variograms_sinker, aes(x = dist, y = gamma, col = Type), size =2) +
  xlab("Distance") + ylab("Semivariance") + labs(col = "Variogram Type")



v_fit_sinker <- fit.variogram(v_sinker, model = vgm(psill = 0.014,
                                                    model = "Gau",     
                                                    range = 0.4,   
                                                    nugget = 0.0032), fit.sills = FALSE, fit.ranges = FALSE, fit.method = FALSE)




sinker_grid <- expand.grid(lon = seq(PitchMovementsWithActiveSpinSinkers@bbox[1,1],
                                     PitchMovementsWithActiveSpinSinkers@bbox[1,2],length.out = 200), 
                           lat = seq(PitchMovementsWithActiveSpinSinkers@bbox[2,1],
                                     PitchMovementsWithActiveSpinSinkers@bbox[2,2],length.out = 200))  

sp_grid_sinker <- sinker_grid
coordinates(sp_grid_sinker) <- ~lon+lat
mean(PitchMovementsWithActiveSpinSinkers@data$logActiveSpin)
sink_sk <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSinkers, sp_grid_sinker, v_fit_sinker, beta = 7.532223) %>% as.data.frame()
sink_ok <- krige(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSinkers, sp_grid_sinker, v_fit_sinker) %>% as.data.frame()


m_sinker <- vgm(psill = 0.014,
                model = "Gau",     
                range = 0.4,   
                nugget = 0.0032) 

set.seed(999)
sinker_ok_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSinkers, m_sinker, nfold=212)
bubble(sinker_ok_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(sinker_ok_cross$residual)
mean((sinker_ok_cross$residual)^2)


sinker_sk_cross <- krige.cv(logActiveSpin ~ 1, PitchMovementsWithActiveSpinSinkers, beta = 7.532223, m_sinker, nfold=212)
bubble(sinker_sk_cross, "residual", main = "logActiveSpin: LOO CV residuals, Ordinary")
mean(sinker_sk_cross$residual)
mean((sinker_sk_cross$residual)^2)

ggplot() +
  geom_point(data = sink_ok, aes(x = lon, y = lat, col = var1.pred),  shape = 15, size = 3) +
  labs(col = "Predicted Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "Spectral") +
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))

ggplot() +
  geom_point(data = sink_ok, aes(x = lon, y = lat, col = var1.var),  shape = 15, size = 3) +
  labs(col = "Variance Log Active Spin", x = "Horizontal Movement", y = "Vertical Movement")+
  scale_colour_distiller(palette = "PiYG")+
  theme(legend.key = element_rect(colour="#182d55", fill = c("white")),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.key.size = unit(2, "cm"),
        legend.position = "bottom",
        text = element_text(
          size = 20, face = "bold.italic"))
