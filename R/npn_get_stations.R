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
npn.getStations <- function(state_code=NULL, person_id=NULL, network_ids=NULL, request_src="via_R"){
  
  if(all(is.null(state_code, person_id, network_ids))) warning("Warning: No filter criteria specified; returning ALL Nature's Notebook sites")
  
  npn.base <- "http://www.usanpn.org/npn_portal/stations/getAllStations.xml?"
  
  stat.query=list()
  if(!is.null(state_code)) stat.query$state_code=state_code
  if(!is.null(person_id)) stat.query$person_id=person_id
  if(!is.null(network_ids)) stat.query$network_ids=network_ids

  # download data using httr
  stat.info = httr::GET(npn.base,
                        query = stat.query)
  
  # get station list data; stored as xml
  stat.xml <- httr::content(stat.info, as = "parsed")
  # spp.info = httr::GET(spp.base,
  #                      query = spp.query)
  
  
  # -----------------------------------
  # 
  # -----------------------------------
  # -----------------------------------
}
#----------------------------------------------------------
  