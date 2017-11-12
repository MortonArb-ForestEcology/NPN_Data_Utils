# -----------------------------------
# Description
# -----------------------------------
##' @title Download raw NPN individual observation data
##' @family download NPN data
##' @author Christy Rollinson
##' @description Download raw observation data from USA-NPN data web service API
# -----------------------------------
# Notes
# -----------------------------------
##' @details For more information on options, see: https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit
# -----------------------------------
# Parameters
# -----------------------------------
##' @param start_date - (optional) YYYY-MM-DD; first day of interest
##' @param end_date - (optional) YYYY-MM-DD; first day of interest
##' @param region - (optional) an object containing an extent (xmin, xmax, ymin, ymax); e.g. a bounding box (extent) or spatial polygon (i.e. shapefile) that can be used to subset stations
##' @param species - (optional) species ID number OR scientific name (will then query species ID)
##' @param station_id - (optional) unique station ID to return observations from specific location
##' @param species_type - (optional) value from getAnimalTypes or getPlantTypes
##' @param network - (optional) groups or network of interest; value from getPartnerNetworks Function
##' @param state - (optional) 2-letter abbreviation of states of interest
##' @param phenophase_category - (optional) pheonophase category of interest; value from getPhenophase function
##' @param phenophase_id - (optional) phenophase identifier number if only specific phenophases are of interest
##' @param functional_type - (optional) plant or animal functional type; from getSpeciesFunctionalTypes function
##' @param additional_field - (optional) additional data fields of interest including climate and GDD records; can also include species info, observer ID and more.  see google drive sheet for full list of options
##' @param climate_data - (optional, logical; default=F) should all climate fields be returned?
##' @param IP_address - (optional) provide usage statistics by providing the requesting client’s IP address
##' @param user_email - (optional) provide usage statistics by providing the requesting user’s IP address.
##' @param request_src - (REQUIRED) brief description of use; default is "via_R"
##' @export
# -----------------------------------
# Outputs
# -----------------------------------
##' @return 
# -----------------------------------
# Workflow
# -----------------------------------
# -----------------------------------

#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
npn.getObs <- function(start_date=NULL, end_date=NULL, region=NULL, species=NULL, station_id=NULL, species_type=NULL, network=NULL, state=NULL, phenophase_category=NULL, phenophase_id=NULL, functional_type=NULL, additional_field=NULL, climate_data=F, IP_address=NULL, user_email=NULL, request_src="via_R"){
  
  # -----------------------------------
  # Working with a particular region
  # -----------------------------------
  latmin=NULL
  latmax=NULL
  lonmin=NULL
  lonmax=NULL
  if(!is.null(region)){
    # pull out our bounding box
    lonmin <- raster::extent(region)[1]
    lonmax <- raster::extent(region)[2]
    latmin <- raster::extent(region)[3]
    latmax <- raster::extent(region)[4]
    
    # Note: in the current NPN API, they have x & y backwards
    ## bottom_left_x1 - (optional) minimum LATITUDE
    ## bottom_left_y1 - (optional) minimum LONGITUDE
    ## upper_right_x1 - (optional) maximum LATITUDE
    ## upper_right_y1 - (optional) maximum LONGITUDE
    
    # If we're working with a shapefile etc, we only want stations in our area of interest
    if(!class(region)=="Extent"){
      # stat.all <- getURL("http://www.usanpn.org/npn_portal/stations/getAllStations.json?")
      # stat.all <- jsonlite::fromJSON(stat.all)
      
      # download data using httr
      stat.all = httr::GET("http://www.usanpn.org/npn_portal/stations/getAllStations.json?",
                           httr::progress()
                           )
      
      # convert data to a clean data frame
      stat.all <- as.data.frame(jsonlite::fromJSON(httr::content(stat.all, as = "text")))
      
      # # Note: some coordinates are clearly wrong
      # stat.all[stat.all$longitude>0,"longitude"] <- stat.all[stat.all$longitude>0,"longitude"]*-1
      # stat.all[stat.all$latitude<0,"latitude"] <- stat.all[stat.all$latitude<0,"latitude"]*-1
      
      # Convert to a spatial file
      coordinates(stat.all) <- stat.all[,c("longitude", "latitude")]
      projection(stat.all) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      # plot(stat.all)
      
      # Subset our station list to just those inside our polygon
      station_id <- data.frame(stat.all[!is.na(over(stat.all, region)[,1]),])$station_id
    } # End station query
    
  } # End spatial info
  # -----------------------------------
  
  # -----------------------------------
  # Working with Species: If we need help getting species ID numbers, do so here
  # -----------------------------------
  if(!is.null(species) & class(species)!="integer"){
    # Looks like we can't query multiple species at once, so we'll need to do an old fashioned loop
    species.names <- species
    
    spp.base <- "http://www.usanpn.org/npn_portal/species/getSpeciesByScientificName.json?"
    
    for(i in seq_along(species.names)){
      spp.query = list(genus=strsplit(species.names[i]," ")[[1]][1],
                       species=strsplit(species.names[i]," ")[[1]][2])
      
      # download data using httr
      spp.info = httr::GET(spp.base,
                           query = spp.query)
      
      # convert data to a clean data frame
      species[i] <- as.data.frame(jsonlite::fromJSON(httr::content(spp.info, as = "text")))$species_id
    } # End looping through species
  } # End convering species names to ID numbers
  # -----------------------------------
  
  # -----------------------------------
  # building the query for the NPN database
  # -----------------------------------
  npn.obs.base <- "http://www.usanpn.org/npn_portal/observations/getObservations.json?"
  
  obs.query <- list(request_src=request_src)
  
  ## start_date - (optional) YYYY-MM-DD; first day of interest
  ## end_date - (optional) YYYY-MM-DD; last day of interest to subset stations
  if(!is.null(start_date)){
    obs.query[["start_date"]] <- start_date
    obs.query[["end_date"]] <- end_date
  } 

  # region - (optional) an object containing an extent (xmin, xmax, ymin, ymax); e.g. a bounding box (extent) or spatial polygon (i.e. shapefile) that can be used  
  if(!is.null(latmin)){
    # REMEMBER! This has x & y BACKWARDS
    ## bottom_left_x1 - (optional) minimum LATITUDE
    ## bottom_left_y1 - (optional) minimum LONGITUDE
    ## upper_right_x1 - (optional) maximum LATITUDE
    ## upper_right_y1 - (optional) maximum LONGITUDE
    obs.query[["bottom_left_x1"]] <- latmin
    obs.query[["bottom_left_y1"]] <- lonmin
    obs.query[["upper_right_x1"]] <- latmax
    obs.query[["upper_right_y1"]] <- lonmax
  }
  
  ## station_id - (optional) unique station ID to return observations from specific location
  if(!is.null(station_id)){
    for(i in seq_along(station_id)){
      obs.query[[paste0("station_id[", i-1, "]")]] <- station_id[i]
    }
  }
  
  ## species - (optional) species ID number OR scientific name (will then query species ID)
  if(!is.null(species)){
    for(i in seq_along(species)){
      obs.query[[paste0("species_id[", i-1, "]")]] <- species[i]
    }
  }

  ## species_type - (optional) value from getAnimalTypes or getPlantTypes
  if(!is.null(species)){
    for(i in seq_along(species)){
      obs.query[[paste0("species_id[", i-1, "]")]] <- species[i]
    }
  }
  ## network - (optional) groups or network of interest; value from getPartnerNetworks Function
  if(!is.null(network)){
    for(i in seq_along(network)){
      obs.query[[paste0("network[", i-1, "]")]] <- network[i]
    }
  }
  
  ## state - (optional) 2-letter abbreviation of states of interest
  if(!is.null(state)){
    for(i in seq_along(state)){
      obs.query[[paste0("state[", i-1, "]")]] <- state[i]
    }
  }
  
  ## phenophase_category - (optional) pheonophase category of interest; value from getPhenophase function
  if(!is.null(phenophase_category)){
    for(i in seq_along(phenophase_category)){
      obs.query[[paste0("phenophase_category[", i-1, "]")]] <- phenophase_category[i]
    }
  }
  
  ## phenophase_id - (optional) phenophase identifier number if only specific phenophases are of interest
  if(!is.null(phenophase_id)){
    for(i in seq_along(phenophase_id)){
      obs.query[[paste0("phenophase_id[", i-1, "]")]] <- phenophase_id[i]
    }
  }
  
  ## functional_type - (optional) plant or animal functional type; from getSpeciesFunctionalTypes function
  if(!is.null(functional_type)){
    for(i in seq_along(functional_type)){
      obs.query[[paste0("functional_type[", i-1, "]")]] <- functional_type[i]
    }
  }
  
  ## additional_field - (optional) additional data fields of interest including climate and GDD records; can also include species info, observer ID and more.  see google drive sheet for full list of options
  if(!is.null(additional_field)){
    for(i in seq_along(additional_field)){
      obs.query[[paste0("additional_field[", i-1, "]")]] <- additional_field[i]
    }
  }
  
  
  ## climate_data - (optional, logical) should all climate fields be returned?
  if(climate_data) obs.query[["climate_data"]] <- 1
  
  ## IP_address - (optional) 
  if(!is.null(IP_address)) obs.query[["IP_address"]] <- IP_address
  
  ## user_email - (optional)
  if(!is.null(user_email)) obs.query[["user_email"]] <- user_email
  
  dat.obs = httr::GET(npn.obs.base,
                      query=obs.query,
                      httr::progress()  
                      )
  dat.obs <- as.data.frame(jsonlite::fromJSON(httr::content(dat.obs, as = "text")))
  # summary(dat.obs)
  # -----------------------------------
  
  # -----------------------------------
  # Do some formatting of the output and return
  # -----------------------------------
  # Making factors factors rather than strings
  vars.factor <- c("state", "genus", "species", "common_name", "kingdom", "phenophase_description", "intensity_value", "individual_id", "phenophase_id", "intensity_category_id", "site_id")
  for(v in vars.factor){
    dat.obs[,v] <- as.factor(dat.obs[,v])
  }
  
  # Set up the date/time fields properly
  dat.obs$update_datetime <- strptime(dat.obs$update_datetime, format=c("%F %T"))
  dat.obs$observation_date <- as.Date(dat.obs$observation_date)
  
  # summary(dat.obs)
  return(dat.obs)
  # -----------------------------------
  
  
} # End function