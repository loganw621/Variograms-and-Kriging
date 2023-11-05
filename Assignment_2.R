# Load data
suppressMessages(library(geoR))

# Understand data (As a thumb rule, the maximum distance for the variogram is taken as half of the maximum distance between two data points in the data set)
summary(parana)
xyz = parana

# Estimating spatial trend
plot(parana)
plot(parana, trend = "1st") # adjust for first order trends 
plot(parana, trend = "2nd") # adjust for second order trend to remove linear trend in residuals 

# Creating and visualizing variogram
plot(variog(parana, option = "cloud", max.dist = 309.75)) # cloud variogram
plot(variog(parana, max.dist = 309.75)) # binned variogram
vario_manual <- variog(parana,
                       max.dist=309.75,
                       uvec=seq(0.01,309.75,30)) # binned variogram with specified bins
print(paste("minimum pairs in a single bin were", min(vario_manual$n))) # esnure greater than 30
plot(vario_manual)

plot(variog(parana, trend = "1st", max.dist = 309.75)) # Variogram with first order trend
vario_model <- variog(parana, trend = "2nd", max.dist = 309.75)
plot(vario_model) # Variogram with second order trend

# Determination of directional effects/ Anisotropy
plot(variog4(parana, trend = "2nd", max.dist = 309.75), omnidirectional = T)

# Fitting covariance models on a variogram
exp_fit_fix <- variofit(vario_model, cov.model = "exp", fix.nugget = T)
exp_fit_nofix <- variofit(vario_model, cov.model = "exp", fix.nugget = F)
sph_fit_fix <- variofit(vario_model, cov.model = "sph", fix.nugget = T)
sph_fit_nofix <- variofit(vario_model, cov.model = "sph", fix.nugget = F)

# Visualisation of model fit
plot(vario_model,pch=16)
lines(exp_fit_fix,col="green",lwd=4, lty = 1)
lines(exp_fit_nofix,col="red",lwd=4, lty = 2)
lines(sph_fit_fix,col="blue",lwd=4, lty = 3) 
lines(sph_fit_nofix,col="black",lwd=4, lty = 4) 

# Selecting best fit model (resulting model with least SSQ value is considered as best fit)
(exp_SSQ_fix <- summary(exp_fit_fix)$sum.of.squares)
(exp_SSQ_nofix <- summary(exp_fit_nofix)$sum.of.squares)
(sph_SSQ_fix <- summary(sph_fit_fix)$sum.of.squares)
(sph_SSQ_nofix <- summary(sph_fit_nofix)$sum.of.squares)
which.min(list(exp_SSQ_fix,exp_SSQ_nofix,
               sph_SSQ_fix, sph_SSQ_nofix)) # exponential model without fixed nugget should be considered as best fit model

# Creation of a prediction grid (creating a 100 by 100 grid)
prediction_grid <- expand.grid(seq(0, 800, length.out = 100), seq(0, 800, length.out = 100))
plot(prediction_grid)

# Kriging (spatial prediction for fixed covariance parameters using global neighbourhood)
krig_rain <- krige.conv(parana, loc=prediction_grid,
                        krige=krige.control(obj.model=exp_fit_nofix))

# Visualisation
image(krig_rain,col=heat.colors(8))
rgb.palette <- colorRampPalette(c("blue", "lightblue",
                                  "orange", "red"),space = "rgb")

image(krig_rain,krig_rain$krige.var, loc = prediction_grid,col=rgb.palette(20),
      xlab="Coord X",ylab="Coord Y", borders = parana$borders,
      main="Kriging variance")
##
# Define breaks and labels for the legend
breaks <- seq(min(krig_rain$krige.var), max(krig_rain$krige.var), length.out = 5)
labels <- c("Low", "", "", "", "High") # Adjust labels as needed

# Plot with legend
image(krig_rain, krig_rain$krige.var, loc = prediction_grid, col = rgb.palette(20),
      xlab = "Coord X", ylab = "Coord Y", borders = parana$borders,
      main = "Kriging variance")

# Adding Legend
legend("topright", legend = labels, fill = rgb.palette(5), title = "Variance")
##

image(krig_rain,krig_rain$predict, loc = prediction_grid,col=rgb.palette(20),
      xlab="Coord X",ylab="Coord Y", borders = parana$borders,
      main="Kriging prediction")
##
# Define breaks and labels for the legend
breaks <- seq(min(krig_rain$predict), max(krig_rain$predict), length.out = 5)
labels <- c("Low", "", "", "", "High") # Adjust labels as needed

# Plot with legend
image(krig_rain,krig_rain$predict, loc = prediction_grid,col=rgb.palette(20),
      xlab="Coord X",ylab="Coord Y", borders = parana$borders,
      main="Kriging Prediction")

# Adding Legend
legend("topright", legend = labels, fill = rgb.palette(5), title = "Values")
##

# Performing cross validation of results
xv = xvalid(parana, model = exp_fit_nofix)

# Visualisation of cross validated results
library(tidyverse)
xv1 <- data.frame(xv$data, xv$predicted)
ggplot(data = xv1, aes(x = xv.data, y = xv.predicted))+
  geom_point()+
  geom_smooth(method = "lm", color = "red")+
  xlab("Observed rainfall")+
  ylab("Predicted rainfall")+
  labs(title = "Linear regression model for predicted rainfall") # xv.predicted = α+β1(xv.data)+ϵ

# Calculating mean squared and mean absolute errors
mean(xv$error^2)
mean(abs(xv$error)) # conclude that on an average, an error of 19.2194244inches of mean rainfall can be expected at a given location



