#'Read in information from database using 
#'
#'getTable
#'@param uid User ID
#'@param pwd Default password
#'@param db Database name
#'@return table A table from the list of tables that have already been built.
#'@export
getTable <- function(uid = uid,
                    pwd = pwd,
                    db = "NWFSC_OCEAN",
                    tableName = NA){
  
  #Create the channel first. Notice we do not use library. The library is already "install"
  #in the description file. And you use :: to reference the function in RODBC
  channel <-   RODBC::odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  #Available queries 
  availableTables <- RODBC::sqlTables(channel = channel)
  
  
  #Read the available query names in the database
  if(tableName%in%availableTables){
    table <- RODBC::sqlFetch(channel, tableName)
    return(table)
  }else{
    cat("These are the available table for your user account. \n Please choose one.")
    cat(availableTables)
  }
}
