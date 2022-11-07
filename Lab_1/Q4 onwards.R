x_coords <- c(55, 43, 23, 67, 43)
y_coords <- c(22, 26, 30, 45, 39)
values <- c(2.5, 6.8, 5.5, 4.5, 3.5)
demo_data <- data.frame(x_coords, y_coords, values)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = demo_data)+
  geom_point(mapping = aes(x = x_coords, y = y_coords, size = values))+
  ggtitle("Demo Plot of X-Y Data in data.frame")+
  xlab("Longitude (Degrees)")+
  ylab("Latitude (Degrees)")+
  labs(size = "Made up Values")
install.packages(c("sp", "raster", "rgdal"))
longitude <- c(70, 76, 38, 80, 78, 78, 84, 79, 57, 61)
latitude <- c(-67, -41, -43, -52, -57, -26, -56, -24, -74, -34)
lon_lat <- cbind(longitude, latitude)
class(lon_lat)
library(sp)
pts <- SpatialPoints(lon_lat)
class(pts)
str(pts)
pts@coords
pts@bbox
pts@proj4string
long_lat_WGS84 <- CRS('+proj=longlat +datum=WGS84')
class(long_lat_WGS84)
pts <- SpatialPoints(lon_lat, proj4string = long_lat_WGS84)
pts
library(raster)
SpatialPointsDataFrame(pts, age)
install.packages("tidyverse")
library(tidyverse)
mpg
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = cty))
age <- c(30, 34, 40, 30, 26, 65, 50, 46, 67, 49)
SpatialPointsDataFrame(pts, as.data.frame(age), coords.nrs = numeric(0), proj4string = CRS(as.character(NA)))
