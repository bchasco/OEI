#'Read in information from database using 
#'
#'getQuery
#'@param uid User ID
#'@param pwd Default password
#'@param db Database name
#'@param qryName The name of a pre-built query in the database
#'@return query A query from the list of queries that have already been built.
#'@export
getQuery <- function(uid = uid,
                    pwd = pwd,
                    db = db,
                    qryName = NA){
  
  #Create the channel first
  channel <- odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  #Available queries 
  availableQueries <- getQueryList()
  
  #Read the available query names in the database
  if(qryName%in%availableQueries){
    query <- readQuery(channel, qryName)
    return(query)
  }else{
    print("The are the available queries for your user account. \n Please choose one.")
    print(query)
  }
}
