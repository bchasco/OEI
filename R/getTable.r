#'Read in information from database using 
#'
#'getTable
#'@param uid User ID
#'@param pwd Default password
#'@param db Database name
#'@param tableName The name of the schema and table of interest - "schema.table" 
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
  # Limit to just schemas we would use
  availableTables <- subset(availableTables, TABLE_SCHEM %in% c("crepo","globec","ncc","predator","prerecruit","dbo"))
  # The set of tables, in the format we need
  availableTables <- paste0(availableTables$TABLE_SCHEM, ".", availableTables$TABLE_NAME)
  
  #Read the available query names in the database
  if(tableName%in%availableTables){
    table <- RODBC::sqlFetch(channel, tableName)
    RODBC::odbcClose()
    return(table)
  }else{
    cat("These are the available tables for your user account. Please choose one. \n")
    cat(availableTables, sep = '\n')
    RODBC::odbcClose()
  }
}
