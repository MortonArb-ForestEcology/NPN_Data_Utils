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
# dat.quma <- dat1[dat1$genus=="Quercus" & dat1$species=="macrocarpa" & dat1$site_id=="26202",]
dat.quercus <- dat1[dat1$genus=="Quercus" & dat1$site_id=="26202",]
summary(dat.quercus)

# Aggregating the data by week
# This is a really ugly hack-job way of doing it & you don't want to do it for VERY large datasets, but it'll be okay
week.vec <- seq(min(dat.quercus$observation_date), max(dat.quercus$observation_date)+7, by=7)
for(i in as.Date(unique(dat.quercus$observation_date))){
  rows.now <- which(dat.quercus$observation_date==i)
  
  week.now <- week.vec[which((i-3)<=week.vec & (i+3)>=week.vec)]
  
  dat.quercus[rows.now, "observation_week"] <- paste(week.now)
  
}
dat.quercus$observation_week <- as.Date(dat.quercus$observation_week)
summary(dat.quercus)

# Ordering phenophases to make a bit more sense
unique(dat.quercus$phenophase_description)
dat.quercus$phenophase_description <- factor(dat.quercus$phenophase_description, levels=c("Breaking leaf buds", "Increasing leaf size", "Leaves", "Colored leaves", "Falling leaves", "Flowers or flower buds", "Open flowers", "Pollen release (flowers)", "Fruits", "Ripe fruits", "Recent fruit or seed drop"))

# Plotting thigs a bit
ggplot(data=dat.quercus[dat.quercus$phenophase_status==1,]) +
  facet_grid(phenophase_description~.) +
  geom_bar(aes(x=observation_week, fill=species))


# Doing some summaries to show the fraction of individuals in a phenophase
dat.quercus[dat.quercus$phenophase_status==-1, "phenophase_status"] <- NA
dat.quercus2 <- aggregate(dat.quercus[,"phenophase_status"], 
                       by=dat.quercus[,c("species", "observation_week", "phenophase_id", "phenophase_description")], 
                       FUN=sum)
summary(dat.quercus2)

# Figuring out how many individuals we have for each species
spp.summary <- data.frame(species=unique(dat.quercus$species))
for(i in 1:nrow(spp.summary)){
  spp.summary[i,"n.ind"] <- length(unique(dat.quercus[dat.quercus$species==spp.summary$species[i], "individual_id"]))
}
spp.summary

# merge in the number of individuals
dat.quercus2 <- merge(dat.quercus2, spp.summary)
summary(dat.quercus2)

leaves <- c("Breaking leaf buds", "Increasing leaf size", "Leaves", "Colored leaves", "Falling leaves")
fruits <- c("Flowers or flower buds", "Open flowers", "Pollen release (flowers)", "Fruits", "Ripe fruits", "Recent fruit or seed drop")
dat.quercus2$phenophase_group <- ifelse(dat.quercus2$phenophase_description %in% leaves, "leaves", "fruit")

png("~/Google Drive/NPN Local Leaders/Calendar/Calendar_QUMA_Leaves.png", height=8, width=12, unit="in", res=320)
ggplot(data=dat.quercus2[dat.quercus2$phenophase_group=="leaves" & dat.quercus2$species=="macrocarpa",]) +
  facet_grid(species~.) +
  geom_line(aes(x=observation_week, y=x/n.ind, color=phenophase_description), size=2) +
  scale_color_manual(name="Phenophase", values=c("darkseagreen2", "darkolivegreen2", "forestgreen", "chocolate3", "cornsilk4")) +
  labs(x="Observation Week", y="% Individuals", title="Quercus macrocarpa 2017 leaf phenology") +
  theme_bw() 
dev.off()


png("~/Google Drive/NPN Local Leaders/Calendar/Calendar_OakCollection_Leaves.png", height=8, width=12, unit="in", res=320)
ggplot(data=dat.quercus2[dat.quercus2$phenophase_group=="leaves",]) +
  facet_grid(species~.) +
  geom_line(aes(x=observation_week, y=x/n.ind, color=phenophase_description), size=2) +
  scale_color_manual(name="Phenophase", values=c("darkseagreen2", "darkolivegreen2", "forestgreen", "chocolate3", "cornsilk4")) +
  labs(x="Observation Week", y="% Individuals", title="Oak Collection 2017 leaf phenology") +
  theme_bw() 
dev.off()

png("~/Google Drive/NPN Local Leaders/Calendar/Calendar_OakCollection_Leaves2.png", height=8, width=12, unit="in", res=320)
ggplot(data=dat.quercus2[dat.quercus2$phenophase_group=="leaves" & 
                           dat.quercus2$species %in% c("alba", "rubra", "macrocarpa", "velutina"),]) +
  facet_grid(species~.) +
  geom_line(aes(x=observation_week, y=x/n.ind, color=phenophase_description), size=2) +
  scale_color_manual(name="Phenophase", values=c("darkseagreen2", "darkolivegreen2", "forestgreen", "chocolate3", "cornsilk4")) +
  labs(x="Observation Week", y="% Individuals", title="Oak Collection 2017 leaf phenology") +
  theme_bw() 
dev.off()
