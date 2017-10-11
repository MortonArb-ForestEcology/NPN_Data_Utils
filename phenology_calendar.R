# Trying to figure out how to dynamically query the USA NPN Database form R
# Making a phenology calendar with data downloaded directly from NPN

library(RCurl); library(jsonlite)
library(ggplot2)

# Get a list of all stations
stat.all <- getURL("http://www.usanpn.org/npn_portal/stations/getAllStations.json?")
stat.all <- jsonlite::fromJSON(stat.all)
summary(stat.all)
dim(stat.all)

# Subset stations to region of interest
# This is the rough bounding box of the Morton Arboretum 
xmin <- -88.10966491699219
xmax <-  -88.01628112792969
ymin = 41.792816561051815
ymax = 41.827107036777804
stat.area <- stat.all[stat.all$latitude<=ymax &
                        stat.all$latitude>=ymin &
                        stat.all$longitude>=xmin &
                        stat.all$longitude<=xmax,]
summary(stat.area)

# Get all data for those stations
# build a query for all stations in list
npn.obs.base <- "http://www.usanpn.org/npn_portal/observations/getObservations.json?"

query.url <- npn.obs.base
for(i in 1:nrow(stat.area)){
  query.url <- paste0(query.url, "station_id[", i-1, "]=", stat.area$station_id[i], "&") 
}
query.url <- paste0(query.url, "request_src=", "crollinson_test")


# Query all observations from our stations of interest
dat1 <- getURL(query.url)
dat1 <- jsonlite::fromJSON(dat1)
dat1[dat1=="-9999"] <- NA
summary(dat1)
head(dat1)

# Doing some data formatting
vars.factor <- c("state", "genus", "species", "common_name", "kingdom", "phenophase_description", "intensity_value", "individual_id", "phenophase_id", "intensity_category_id", "site_id")
for(v in vars.factor){
  dat1[,v] <- as.factor(dat1[,v])
}
summary(dat1)

# vars.date <- c("update_datetime", "observation_date")
dat1$update_datetime <- strptime(dat1$update_datetime, format=c("%F %T"))
dat1$observation_date <- as.Date(dat1$observation_date)
summary(dat1)


# For my example, lets just work with Quercus macrocarpa from the living collections
dat.quma <- dat1[dat1$genus=="Quercus" & dat1$species=="macrocarpa" & dat1$site_id=="26202",]
summary(dat.quma)

# Aggregating the data by week
# This is a really ugly hack-job way of doing it & you don't want to do it for VERY large datasets, but it'll be okay
week.vec <- seq(min(dat.quma$observation_date), max(dat.quma$observation_date), by=7)
for(i in unique(dat.quma$observation_date)){
  rows.now <- which(dat.quma$observation_date==i)
  
  week.now <- week.vec[which((i-3)<=week.vec & (i+3)>=week.vec)]
  
  dat.quma[rows.now, "observation_week"] <- paste(week.now)
  
}
dat.quma$observation_week <- as.Date(dat.quma$observation_week)
summary(dat.quma)

# Ordering phenophases to make a bit more sense
unique(dat.quma$phenophase_description)
dat.quma$phenophase_description <- factor(dat.quma$phenophase_description, levels=c("Breaking leaf buds", "Increasing leaf size", "Leaves", "Colored leaves", "Falling leaves", "Flowers or flower buds", "Open flowers", "Pollen release (flowers)", "Fruits", "Ripe fruits", "Recent fruit or seed drop"))

# Plotting thigs a bit
ggplot(data=dat.quma[dat.quma$phenophase_status==1,]) +
  facet_grid(phenophase_description~.) +
  geom_bar(aes(x=observation_week, fill=individual_id))


# Doing some summaries to show the fraction of individuals in a phenophase
dat.quma[dat.quma$phenophase_status==-1, "phenophase_status"] <- NA
dat.quma2 <- aggregate(dat.quma[,"phenophase_status"], 
                       by=dat.quma[,c("observation_week", "phenophase_id", "phenophase_description")], 
                       FUN=sum)

summary(dat.quma2)

leaves <- c("Breaking leaf buds", "Increasing leaf size", "Leaves", "Colored leaves", "Falling leaves")
fruits <- c("Flowers or flower buds", "Open flowers", "Pollen release (flowers)", "Fruits", "Ripe fruits", "Recent fruit or seed drop")
dat.quma2$phenophase_group <- ifelse(dat.quma2$phenophase_description %in% leaves, "leaves", "fruit")

png("~/Google Drive/NPN Local Leaders/Calendar/Calendar_QUMA_Leaves.png", height=8, width=12, unit="in", res=320)
ggplot(data=dat.quma2[dat.quma2$phenophase_group=="leaves",]) +
  # facet_grid(phenophase_group~.) +
  geom_line(aes(x=observation_week, y=x/max(dat.quma2$x, na.rm=T), color=phenophase_description), size=2) +
  scale_color_manual(name="Phenophase", values=c("darkseagreen2", "darkolivegreen2", "forestgreen", "chocolate3", "cornsilk4")) +
  labs(x="Observation Week", y="% Individuals", title="Quercus macrocarpa 2017 leaf phenology") +
  theme_bw() 
dev.off()
