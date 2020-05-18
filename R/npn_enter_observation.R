# -----------------------------------
# Description
# -----------------------------------
##' @title Enter new NPN individual observation data
##' @family Enter NPN data
##' @author Christy Rollinson
##' @description Enter raw observation data TO USA-NPN data web service API. Note this requires data to be in 'long' format with the NPN parameters listed as column headers
# -----------------------------------
# Notes
# -----------------------------------
##' @details For more information on options, see: https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit
# -----------------------------------
# Parameters
# -----------------------------------
##' @param newdata - data to be intered into NPN
##' @param user_id
##' @param user_pw
##' @param access_token -(alternate)
##' @param consumer_key - (alternate)
##' @param phenophase_id - (required in newdata)
##' @param individual_id - (required in newdata)
##' @param observation_date - (required in newdata) format YYYY-MM-DD
##' @param observation_extent - (required in newdata) whether the phenophase was observed; 1 = yes; 0 = no; -1 = unsure
##' @param observation_comment - string to add to observation comment field
##' @param observation_value_id = (optional in newdata) categorical abundance value
##' @param raw_abundance_value = (optional in newdata)
##' @export
# -----------------------------------
# Outputs
# -----------------------------------
##' @return 
# -----------------------------------
# Workflow
# -----------------------------------
# -----------------------------------

npn.putObs <- function(newdata, user_id=NULL, user_pw=NULL, access_token, consumer_key, observation_comment="Uploaded via R", npn_server="dev"){
  
  if(! npn_server %in% c("dev", "production")) {
    stop(paste0("Invalid server specificaiton.  Only 'dev' and 'production' allowed.  You specified: ", npn_server))
  }
    
  if(is.null(user_id))  user_id <- rstudioapi::askForPassword("Enter NPN user ID Number")

  if(is.null(user_pw)) user_pw <- rstudioapi::askForPassword("Enter NPN password")
  
  dat.put <- list()
  dat.put$user_id=user_id
  dat.put$user_pw = user_pw
  
  dat.put$individual_id <- unique(newdata$individual_id)[1]
  
  # Formatting the observation comment
  obs.comment <- newdata$observation_comment[1]
  if(is.na(obs.comment)){
    dat.put$observation_comment <- gsub(" ", "%20", observation_comment)
  } else {
    obs.comment <- gsub(" ", "%20", obs.comment)
    obs.comment <- gsub("&", "X", obs.comment)
    obs.comment <- gsub("=", "X", obs.comment)
    obs.comment <- gsub("?", "X", obs.comment)
    
    dat.put$observation_comment <- obs.comment
  }
  
  dat.put$observation_comment <- unique(gsub(" ", "%20", newdata$observation_comment))[1]
  
  # start new observation loop here 
  dat.put$observation_date <- mean(newdata$observation_date)
  for(i in seq_along(newdata$phenophase_id)){
    dat.put[[paste0("phenophase_id[",i-1,"]")]] <- newdata$phenophase_id[i]
    dat.put[[paste0("observation_extent[",i-1,"]")]] <- newdata$observation_extent[i]
    
    if(!is.na(newdata$observation_value_id[i]) & newdata$observation_value_id[i]!=-9999) dat.put[[paste0("observation_value_id[",i-1,"]")]] <- newdata$observation_value_id[i]
    
  }
  
  server.url <- ifelse(npn_server=="dev", "https://www-dev.usanpn.org/npn_portal/enter_observation/", "https://www.usanpn.org/npn_portal/enter_observation/")
  
  if(nrow(newdata)>1){
    
    # https://www.usanpn.org/npn_portal/enter_observation/enterObservationSet.xml?user_id=2983&user_pw=XXXX&phenophase_id[0]=183&phenophase_id[1]=184&individual_id=9605&observation_date=2011-02-03&observation_extent[0]=0&observation_extent[1]=1&observation_comment=This_is_a_test
    
    npn.base <- paste0(server.url, "enterObservationSet.xml?")

  } else {
    
    npn.base <- paste0(server.url, "enterObservation.xml?")
    
  }
  
  query.string <- c("")
  for(i in 1:length(dat.put)){
    query.string <- paste(query.string, paste(names(dat.put)[i], dat.put[[i]], sep="="), sep="&")
  }
  
  httr::PUT(url=paste0(npn.base,query.string))
  
}