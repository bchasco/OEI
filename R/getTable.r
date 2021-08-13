#'Read in information from database using 
#'
#'getTable
#'@param uid User ID
#'@param pwd Default password
#'@param db Database name
#'@param tableName The name of the schema and table of interest - "schema.table" 
#'@return table A table from the list of tables that have already been built.
#'@export
getTable <- function(uid = "brandon.chasco",
                    pwd = "bchasco",
                    db = "NWFSC_OCEAN",
                    schemaName = c("crepo","dbo"),
                    tableName = NA){
  
  #Create the channel first. Notice we do not use library. The library is already "install"
  #in the description file. And you use :: to reference the function in RODBC
  channel <-   RODBC::odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  #Available queries 
  # availableTables <- RODBC::sqlTables(channel = channel) %>% 
  #   filter(TABLE_SCHEM == schemaName) %>%
  #   unite(availableTables, c("TABLE_SCHEM","TABLE_NAME"), sep=".") %>%
  #   select(availableTables)
  
  #Available queries 
  availableTables <- RODBC::sqlTables(channel = channel)
  # Limit to just schemas we would use
  availableTables <- subset(availableTables, TABLE_SCHEM %in% schemaName)
  # The set of tables, in the format we need
  availableTables <- paste0(availableTables$TABLE_SCHEM, ".", availableTables$TABLE_NAME)
  #This is a little jinky because of RODBC specifics to error handling
  
  tab <- NULL  
  accessDenied <- FALSE
  while(!is.data.frame(tab)){ # -1 if RODBC throws an error
    tab <- tryCatch(RODBC::sqlQuery(channel, 
                             paste("select * from",paste0("[",schemaName, "].[", tableName,"]")), 
                             FALSE), #FALSE is for RODBC error handling
             error = function(e) e, 
             warning = function(w) w) #Dummy error
    if(!is.data.frame(tab)){
      if(accessDenied){
        cat("You don't have access to that table or query.")
        break()
      }
      print(availableTables)
      my.row <- as.integer(readline(prompt = "Choose the row number for one of tables listed above: "))
      schemaName <- strsplit(availableTables[my.row],split="\\.")[[1]][1]
      tableName <- strsplit(availableTables[my.row],split="\\.")[[1]][2]
      accessDenied <- TRUE
    }
  } 
  RODBC::odbcClose(channel = channel)
  return(tab)           
}
