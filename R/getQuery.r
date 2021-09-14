#'Read in information from database using 
#'
#'getQuery
#'@param uid User ID
#'@param pwd Default password
#'@param myQry A string representing a syntactically correct sql query
#'@return a data fraem with results from the query
#'@export
getQuery <- function(uid = uid,
                    pwd = pwd,
                    myQry = NA){
  
  #Create the channel first
  channel <-   RODBC::odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  tab <- tryCatch(RODBC::sqlQuery(channel, myQry, FALSE), #FALSE is for RODBC error handling
                  error = function(e) e) #Dummy error
  
  RODBC::odbcClose(channel)
  
  # Output
  if(is.null(dim(tab))){ # -1 if RODBC throws an error
    cat("Error: make sure the syntax is correct")
  }else{
    return(tab)    
  }
}
