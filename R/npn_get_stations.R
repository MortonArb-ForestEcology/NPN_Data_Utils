##' Get NPN Observing Stations (Sites), primarily by person id
# -----------------------------------
# Description
# -----------------------------------
##'
##' @title getAllStations
##' @family download NPN data
##' @author Christy Rollinson
##' @description Get NPN stations, options to filter by state, person, and/or network ID
# -----------------------------------
# Notes
# -----------------------------------
##' @details For more information on options, see: https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit
# -----------------------------------
# Parameters
# -----------------------------------
##' @param state_code - optional; standard US Postal abbreviation -- e.g. IL; the state to filter the stations returned; 
##' @param person_id - unique observer identifier to filter results based on the person that created the station
##' @param network_ids - unique identifier for a network which will filter results based on sites' participation in a network
##' @export
# -----------------------------------
# Outputs
# -----------------------------------
##' @return station
##' @return latitude
##' @return longitude
##' @return station_name
##' @return station_id
##' @return network_id
##' @return file_url
##' @return is_owner
# -----------------------------------
# Workflow
# -----------------------------------
# Example REST Call:
#   http://www.usanpn.org/npn_portal/stations/getAllStations.xml?state_code=MD
#   
# -----------------------------------


#----------------------------------------------------------
# Begin function
#----------------------------------------------------------
npn.getStations <- function(state_code=NULL, person_id=NULL, network_ids=NULL){
  
  if(all(is.null(state_code), is.null(person_id), is.null(network_ids))) warning("Warning: No filter criteria specified; returning ALL Nature's Notebook sites")
  if(length(state_code)>1) stop("Cannot provide more than 1 value to state_code at a time")
  if(length(person_id)>1) stop("Cannot provide more than 1 value to person_id at a time")
  
  npn.base <- "http://www.usanpn.org/npn_portal/stations/getAllStations.xml?"
  
  # Note: We can only do 1 state and person at a time, so if we have more than one, we need to loop
  stat.query=list()
  if(!is.null(state_code)) stat.query$state_code=state_code
  if(!is.null(person_id)) stat.query$person_id=person_id
  if(!is.null(network_ids)){
    for(i in seq_along(network_ids)){
      stat.query[[paste0("network_ids[", i-1, "]")]]=network_ids[i]
    }
  } 
  
  # download data using httr
  stat.info = httr::GET(npn.base,
                        query = stat.query)
  
  # get station list data; stored as xml
  stat.xml <- httr::content(stat.info, as="parsed")
  
  # Extract the children to clean things up
  xml.chil <- xml2::xml_children(stat.xml)
  
  if(length(xml.chil)==0){
    warning("No Stations for this observer.")
    
    stat.df <- data.frame(person_id=person_id, 
                          station_id=NA,
                          station_name=NA,
                          latitude=NA,
                          longitude=NA,
                          network_id=NA)
    
    return(stat.df)
  }
  
  # Convert to a data frame
  stat.df <- data.frame(person_id=person_id, 
                        station_id=rep(NA, length(xml.chil)),
                        station_name=rep(NA, length(xml.chil)),
                        latitude=rep(NA, length(xml.chil)),
                        longitude=rep(NA, length(xml.chil)),
                        network_id=rep(NA, length(xml.chil)))
  
  for(i in 1:length(xml.chil)){
    stat.df[i,"station_id"] <- xml2::xml_attrs(xml.chil[[i]])[["station_id"]]
    stat.df[i,"station_name"] <- xml2::xml_attrs(xml.chil[[i]])[["station_name"]]
    stat.df[i,"latitude"] <- xml2::xml_attrs(xml.chil[[i]])[["latitude"]]
    stat.df[i,"longitude"] <- xml2::xml_attrs(xml.chil[[i]])[["longitude"]]
    stat.df[i,"network_id"] <- xml2::xml_attrs(xml.chil[[i]])[["network_id"]]
  }

  return(stat.df)
  # -----------------------------------
}
#----------------------------------------------------------
  