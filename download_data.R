# Testing the npn.getObs function with morton arb
# For more info: https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit?usp=sharing
rm(list=ls())

library(raster); library(rgdal); library(rgeos)
library(lubridate)

# Read in the Morton Arb boundaries
morton <- rgdal::readOGR("/Volumes/GIS/Collections/boundaries/Morton_Arboretum.shp")
morton <- sp::spTransform(morton, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # convert it to lon/lat
plot(morton)


species <- c("Quercus macrocarpa", "Acer rubrum")

region <- morton

request_src="Rollinson_R_test"


dat.test <- npn.getObs(region=morton, species=species, request_src=request_src, start_date="2017-05-01", end_date="2017-10-31")
summary(dat.test)



