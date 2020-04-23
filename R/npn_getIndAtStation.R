##' Get NPN Observing Stations (Sites), primarily by person id
# -----------------------------------
# Description
# -----------------------------------
##'
##' @title getIndividualsAtStation
##' @family download NPN data
##' @author Christy Rollinson
##' @description Get individuals associated with an individual station
# -----------------------------------
# Notes
# -----------------------------------
##' @details For more information on options, see: https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit
# -----------------------------------
# Parameters
# -----------------------------------
##' @param station_ids - required station for which you want registered individuals that are observed
##' @export
# -----------------------------------
# Outputs
# -----------------------------------
##' @return indiviudal
##' @return individual_id
##' @return individual_names -- plant "nickname"
##' @return kingdom - Animalia or Plantae
##' @return species_id
##' @return active
##' @return seq_num
##' @return file_url
# -----------------------------------
# Workflow
# -----------------------------------
# Example REST Call:
#   http://www.usanpn.org/npn_portal/stations/getAllStations.xml?state_code=MD
#   
# ----------------------

npn.getIndAtStation <- function(station_ids){
  # http://www.usanpn.org/npn_portal/individuals/getIndividualsAtStations.xml?station_ids=507&station_ids=523
  
  npn.base <- "http://www.usanpn.org/npn_portal/individuals/getIndividualsAtStations.xml?"
  
  stat.query <- list()
  for(i in seq_along(station_ids)){
    stat.query[[paste0("station_ids")]] <- station_ids[i]
  }
  
  stat.info = httr::GET(npn.base,
                        query = stat.query)
  
  # get station list data; stored as xml
  stat.xml <- httr::content(stat.info, as="parsed")
  
  # Extract the children to clean things up
  xml.chil <- xml2::xml_children(stat.xml)
  
  if(length(xml.chil)==0){
    warning("No Stations for this observer.")
    
    ind.df <- data.frame(#station_id=station_ids,
                         individual_id=NA, 
                         individual_name=NA,
                         species_id=NA,
                         kingdom=NA,
                         active=NA,
                         file_url=NA)
    
    return(ind.df)
  }
  
  # Convert to a data frame
  ind.df <- data.frame(#station_id=station_ids,
                       individual_id=rep(NA, length(xml.chil)), 
                       individual_name=rep(NA, length(xml.chil)),
                       species_id=rep(NA, length(xml.chil)),
                       kingdom=rep(NA, length(xml.chil)),
                       active=rep(NA, length(xml.chil)),
                       file_url=rep(NA, length(xml.chil)))
  
  for(i in 1:length(xml.chil)){
    ind.df[i,"individual_id"] <- xml2::xml_attrs(xml.chil[[i]])[["individual_id"]]
    ind.df[i,"individual_name"] <- xml2::xml_attrs(xml.chil[[i]])[["individual_name"]]
    ind.df[i,"species_id"] <- xml2::xml_attrs(xml.chil[[i]])[["species_id"]]
    ind.df[i,"kingdom"] <- xml2::xml_attrs(xml.chil[[i]])[["kingdom"]]
    ind.df[i,"active"] <- xml2::xml_attrs(xml.chil[[i]])[["active"]]
    ind.df[i,"file_url"] <- xml2::xml_attrs(xml.chil[[i]])[["file_url"]]
  }
  
  return(ind.df)
}