library('tidyverse')
library('broom')
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages('raster')
library(ggplot2)

clim <- read.csv('https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv',sep = ';')
head(clim,6)

clim$altitude <- as.numeric(gsub(",", "", clim$altitude))
clim$p_mean

clim$p_mean <- as.numeric(gsub(",", "", clim$p_mean))
head(clim,6)
str(clim)
summary(clim)

G1 <- raster::getData(country = "France", level = 1)

ggplot() +
geom_polygon(data = G1,aes(x = long, y = lat, group = group),colour = "grey10", fill = "#fff7bc") +
geom_point(data = clim,aes(x = lon, y = lat),alpha = .5,size = 2) +
theme_bw() +
xlab("Longitude") +
ylab("Latitude") +
coord_map()


climfrar <- clim[1:34,]

model <- lm(t_mean~ altitude +lat + lon, data = climfrar)

summary(model)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#   altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#   lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#   lon          0.0321010  0.0395728   0.811    0.424  

Mean_Average_Temperature = 37.265 - 0.006414*altitude - 0.534*latitude + 0.0321*longitude

# The intercept shows the average mean annual temperature of 37.265 for a altitude, 
# latitude and longitude with zero values (a= 37.265, SE = 2.622, p<0.001)

# Altitude is negatively associated with mean annual temperature, for every 
# increase in altitude by one unit the average mean annual temperature 
# decreases by 0.0064 units(b= 0.0064,SE = 0.00087, p<0.001)

# Latitude is negatively associated with mean annual temperature, for every 
# increase in latitude by one unit the average mean average temperature 
# decreases by 0.534 units(c= 0.534,SE = 0.0557, p<0.001)


# Longitude is positively associated with mean annual temperature, for every 
# increase in longitude by one unit the average mean average temperature 
# increases by 0.0321 units(d= 0.0321,SE = 0.03957, p>0,001)


model_2 <- lm(t_mean~ altitude + lat, data = climfrar)
summary(model_2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
#   altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
#   lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***

Mean_Average_Temperature = 37.9147 - 0.00626*altitude - 0.547*latitude

prediction_mont <- model_2$coefficients[1] + model_2$coefficients[2]*(1212) + model_2$coefficients[3]*(44.16)
prediction_mont

prediction_pic <- 37.9147 + model_2$coefficients[2]*(2860) + model_2$coefficients[3]*(42.93)
prediction_pic

pred_t_mean <- predict(model_2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
pred_t_mean



# The predicted mean annual temperature of Mont-Ventoux is 6.17 with a 95%
# confidence that the predicted mean annual temperature falls within the range
# of 3.79 to 8.54.

# The predicted mean annual temperature of Pic-du-midi is -3.45 with a 95%
# confidence that the predicted mean annual temperature falls within the range
# of -8.35 to 1.45.



scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
              pch = 16, highlight.3d = TRUE, angle = 45))
scatter_3d$plane3d(model_2)

# The intercept shows the mean annual temperature of 37.915 for a altitude, 
# latitude and longitude with zero values (a= 37.915, SE = 2.483, p<0.001)

# Altitude is negatively associated with mean annual temperature, for every 
# increase in altitude by one unit the average mean annual temperature 
# decreases by 0.00636 units(b= 0.00364,SE = 0.000844, p<0.001)

# Latitude is negatively associated with mean annual temperature, for every 
# increase in latitude by one unit the average mean annual temperature 
# decreases by 0.547 units(c= 0.547,SE = 0.0533, p<0.001)

# The r squared value of 83% shows the proportion of variance in the mean annual
# temperature that is explained by altitude and latitude.

# The associated p value of 1.268e-12 suggests that the independent variables
# altitude and latitude are significantly related to mean annual temperature.
