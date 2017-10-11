# Trying to figure out how to dynamically query the USA NPN Database form R
# The RNPN tools seemed a bit too constrictive for me because they assume you know your state or
# species.  I'd rather work by bounding box.  
#
# Note: I think we can write a script that will then transform and push our data back to the server via the API
 
library(RCurl); library(jsonlite)
test2 <- getURL("http://www.usanpn.org/npn_portal/stations/getAllStations.json?state_code=MD")
test2b <- jsonlite::fromJSON(test2)
summary(test2b)

# To jump straight to data download
start.date <- "2017-10-01"
end.date <- "2017-10-31"
# 41.807717, -88.095667
# 41.831906, -88.031316
xmin <- -88.10966491699219
xmax <-  -88.01628112792969
ymin = 41.792816561051815
ymax = 41.827107036777804

# When trying to get observations, the coordinates didn't seem to work right.
# http://www.usanpn.org/npn_portal/observations/getObservations.json?start_date=2012-01-01&end_date=2012-01-03&state[0]=AZ&state[1]=IL&request_src=rest_test
# dat.all <- getURL(paste0("http://www.usanpn.org/npn_portal/observations/getObservations.json?",
#                          "start_date=", start.date,
#                          "&end_date=", end.date,
#                          "&bottom_left_x1=", xmin, 
#                          "&bottom_left_y1=", ymin, 
#                          "&upper_right_x2=", xmax, 
#                          "&upper_right_y2=", ymax, 
#                          "&request_src=", "crollinson_test"))
# dat.all <- getURL(paste0("http://www.usanpn.org/npn_portal/observations/getObservations.json?",
#                          "start_date=",start.date, 
#                          "&end_date=",end.date,
#                          # "&bottom_left_x1[0]=", xmin,
#                          # "&bottom_left_y1[0]=", ymin,
#                          # "&upper_right_x2[0]=", xmax,
#                          # "&upper_right_y2[0]=", ymax,
#                          
#                          "&request_src=rest_test"))
# dat.all2 <- jsonlite::fromJSON(dat.all) 
# summary(dat.all2)

# Get a list of all stations
stat.all <- getURL("http://www.usanpn.org/npn_portal/stations/getAllStations.json?")
stat.all <- jsonlite::fromJSON(stat.all)
summary(stat.all)
dim(stat.all)

# Subset stations to region of interest
stat.area <- stat.all[stat.all$latitude<=ymax &
                        stat.all$latitude>=ymin &
                        stat.all$longitude>=xmin &
                        stat.all$longitude<=xmax,]


# Get all data for those stations
dat1 <- getURL(paste0("http://www.usanpn.org/npn_portal/observations/getObservations.json?",
                      "station_id[0]=",stat.area$station_id[2],
                      "&station_id[1]=",stat.area$station_id[3],
                      "&request_src=", "crollinson_test"))
dat1 <- jsonlite::fromJSON(dat1)
dat1$update_datetime <- strptime(dat1$update_datetime, format=c("%F %T"))

vars.factor <- c("state", "genus", "species", "common_name", "kingdom", "phenophase_description", "intensity_value")
for(v in vars.factor){
  dat1[,v] <- as.factor(dat1[,v])
}
summary(dat1)




