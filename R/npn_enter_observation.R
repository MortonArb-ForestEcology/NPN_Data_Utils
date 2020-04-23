# -----------------------------------
# Description
# -----------------------------------
##' @title Enter new NPN individual observation data
##' @family Enter NPN data
##' @author Christy Rollinson
##' @description Enter raw observation data TO USA-NPN data web service API
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
##' @param access_token -
##' @param consumer_key - (optional?)
##' @param phenophase_id - (required)
##' @param individual_id - (required)
##' @param observation_date - (required) format YYYY-MM-DD
##' @param observation_extent - (required) whether the phenophase was observed; 1 = yes; 0 = no; -1 = unsure
##' @param observation_comment
##' @param observation_value_id = (optional) categorical abundance value
##' @param raw_abundance_value
##' @param 
##' @export
# -----------------------------------
# Outputs
# -----------------------------------
##' @return 
# -----------------------------------
# Workflow
# -----------------------------------
# -----------------------------------
# Christy's user id=2342

npn.putObs <- function(newdata, user_id=NULL, user_pw=NULL, access_token, consumer_key, phenophase_id, individual_id, observation_date, observation_extent, observation_comment="Uploaded via R", observation_value_id, raw_abundance_value=NA){
  if(is.null(user_id))  user_id <- rstudioapi::askForPassword("Enter NPN user ID Number")

  if(is.null(user_pw)) user_pw <- rstudioapi::askForPassword("Enter NPN password")
  
  dat.put <- list()
  dat.put$user_id=user_id
  dat.put$user_pw = user_pw
  
  dat.put$individual_id <- unique(newdata$individual_id)[1]
  dat.put$observation_comment <- unique(gsub(" ", "%20", newdata$observation_comment))[1]
  
  # start new observation loop here 
  dat.put$observation_date <- mean(newdata$observation_date)
  for(i in seq_along(newdata$phenophase_id)){
    dat.put[[paste0("phenophase_id[",i-1,"]")]] <- newdata$phenophase_id[i]
    dat.put[[paste0("observation_extent[",i-1,"]")]] <- newdata$observation_extent[i]
    
    if(!is.na(newdata$observation_value_id[i])) dat.put[[paste0("observation_value_id[",i-1,"]")]] <- newdata$observation_value_id[i]
    
  }
  
  
  if(nrow(newdata)>1){
    
    # https://www.usanpn.org/npn_portal/enter_observation/enterObservationSet.xml?user_id=2983&user_pw=XXXX&phenophase_id[0]=183&phenophase_id[1]=184&individual_id=9605&observation_date=2011-02-03&observation_extent[0]=0&observation_extent[1]=1&observation_comment=This_is_a_test
    
    npn.base <- "https://www.usanpn.org/npn_portal/enter_observation/enterObservationSet.xml?"
    

  } else {
    npn.base <- "https://www.usanpn.org/npn_portal/enterObservation/enterObservation.xml?"
    
  }
  
  
  httr::PUT(url=npn.base,
            query=dat.put)
  
}