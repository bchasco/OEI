#'Read in information from database using 
#'
#'getTable
#'@param uid User ID
#'@param pwd Default passwoed
#'@param db Database name
#'@return table A table from the list of queries that have already been built.
#'@export
getTable <- function(uid = uid,
                    pwd = pwd,
                    db = db,
                    tableName = NA){
  
  #Create the channel first
  channel <- odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  #Available queries 
  availableTables <- getTableList()
  
  #Read the available query names in the database
  if(qryName%in%availableTables){
    table <- readTable(channel, tableName)
    return(table)
  }else{
    cat("The are the available queries for your user account. \n Please choose one.")
    cat(table)
  }
}
