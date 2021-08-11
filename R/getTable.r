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
                     schemaName = c("crepo","globec","ncc","predator","prerecruit","dbo"),
                     tableName = NA){

  #Create the channel first. Notice we do not use library. The library is already "install"
  #in the description file. And you use :: to reference the function in RODBC
  channel <-   RODBC::odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  #Available queries 
  availableTables <- RODBC::sqlTables(channel = channel) %>% 
    filter(TABLE_SCHEM == schemaName) %>%
    unite(availableTables, c("TABLE_SCHEM","TABLE_NAME"), sep=".") %>%
    select(availableTables)
  
  #This is a little jinky because of RODBC specifics to error handling
  tab <- tryCatch(RODBC::sqlQuery(channel, 
                                  paste("select * from",paste0("[",schemaName, "].[", tableName,"]")), 
                                  FALSE), #FALSE is for RODBC error handling
                  error = function(e) e) #Dummy error

  RODBC::odbcClose(channel = channel)
  if(is.null(dim(tab))){ # -1 if RODBC throws an error
    print(availableTables)
    cat("Error: Choose from one of the listed tables.")
  }else{
    return(tab)           
  } 
}
