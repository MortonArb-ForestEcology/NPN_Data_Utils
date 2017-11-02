# Testing the npn.getObs function with morton arb
# For more info: https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit?usp=sharing

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



# OLD!
library(RCurl); library(jsonlite)
# test2 <- getURL("http://www.usanpn.org/npn_portal/stations/getAllStations.json?state_code=MD")
# test2b <- jsonlite::fromJSON(test2)
# summary(test2b)

# To jump straight to data download
start.date <- "2017-10-01"
end.date <- "2017-10-31"
# 41.807717, -88.095667
# 41.831906, -88.031316
lon.min <- -88.10966491699219
lon.max <-  -88.01628112792969
lat.min = 41.792816561051815
lat.max = 41.827107036777804

# --------------
# Option 1: Getting EVERYTHING for a geographic area
# --------------
# When trying to get observations, the coordinates didn't seem to work right.
# http://www.usanpn.org/npn_portal/observations/getObservations.json?start_date=2012-01-01&end_date=2012-01-03&state[0]=AZ&state[1]=IL&request_src=rest_test
dat.all <- getURL(paste0("http://www.usanpn.org/npn_portal/observations/getObservations.json?",
                         "start_date=", start.date,
                         "&end_date=", end.date,
                         "&bottom_left_x1=", lat.min,
                         "&bottom_left_y1=", lon.min,
                         "&upper_right_x2=", lat.max,
                         "&upper_right_y2=", lon.max,
                         "&request_src=", "crollinson_test"))
dat.all2 <- jsonlite::fromJSON(dat.all)
summary(dat.all2)

vars.factor <- c("state", "genus", "species", "common_name", "kingdom", "phenophase_description", "intensity_value", "individual_id", "phenophase_id", "intensity_category_id")
for(v in vars.factor){
  dat.all2[,v] <- as.factor(dat.all2[,v])
}
summary(dat.all2)

# vars.date <- c("update_datetime", "observation_date")
dat.all2$update_datetime <- strptime(dat.all2$update_datetime, format=c("%F %T"))
dat.all2$observation_date <- as.Date(dat.all2$observation_date)
summary(dat.all2)

write.csv(dat.all2, "~/Google Drive/NPN Local Leaders/Calendar/data_export1.csv", row.names=F)
# --------------


# --------------
# Option 2: Going by stations
#  -- this will probably work better if you're interested in a non-square area
#  -- to do this, you'll want to have a polygon and use the "over" function in sp 
#     to identify sites inside your polygon
# --------------
# Get a list of all stations
stat.all <- getURL("http://www.usanpn.org/npn_portal/stations/getAllStations.json?")
stat.all <- jsonlite::fromJSON(stat.all)
summary(stat.all)
dim(stat.all)

# Subset stations to region of interest
# -- here's where you can modify things to work with spatial filess
stat.area <- stat.all[stat.all$latitude<=lat.max &
                        stat.all$latitude>=lat.min &
                        stat.all$longitude>=lon.min &
                        stat.all$longitude<=lon.max,]
summary(stat.area)

# Get all data for those stations
# build a query for all stations in list
npn.obs.base <- "http://www.usanpn.org/npn_portal/observations/getObservations.json?"

query.url <- npn.obs.base
for(i in 1:nrow(stat.area)){
  query.url <- paste0(query.url, "station_id[", i-1, "]=", stat.area$station_id[i], "&") 
}
query.url <- paste0(query.url, "request_src=", "crollinson_test")

dat1 <- getURL(query.url)
dat1 <- jsonlite::fromJSON(dat1)
dat1[dat1=="-9999"] <- NA
summary(dat1)
head(dat1)

# Doing some data formatting
vars.factor <- c("state", "genus", "species", "common_name", "kingdom", "phenophase_description", "intensity_value", "individual_id", "phenophase_id", "intensity_category_id")
for(v in vars.factor){
  dat1[,v] <- as.factor(dat1[,v])
}
summary(dat1)

# vars.date <- c("update_datetime", "observation_date")
dat1$update_datetime <- strptime(dat1$update_datetime, format=c("%F %T"))
dat1$observation_date <- as.Date(dat1$observation_date)
summary(dat1)

write.csv(dat1, "~/Google Drive/NPN Local Leaders/Calendar/data_export2.csv", row.names=F)
# --------------




