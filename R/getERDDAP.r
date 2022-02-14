#'Get an ERDDAP file
#'getERDAPP
#'@param var=NULL Dataset ID for an erddap file (see 'https://oceanview.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000')  
#'@return ERDDAP data.frame
#'@examples x <- getERDDAP(var='cciea')
#' @export getERDDAP
getERDDAP <- function(var = NULL){
  #Read in the table contents
  content <- rvest::read_html("https://oceanview.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000")
  tableDAP <- content %>% rvest::html_table(fill = TRUE)
  dataID <- as.data.frame(tableDAP[[2]]$'Dataset ID')
  if(is.null(var)){
    print(dataID)
    var <- readline(prompt = "Enter any number or grep a name: ");
  }  
  if(!is.na(as.numeric(var))){
    varVal <- as.integer(var)
  }
  if(is.na(as.numeric(var))){
    varVal <- grep(var,tableDAP[[2]]$'Dataset ID')
    if(length(varVal)>1){
      print(tableDAP[[2]]$'Title'[varVal])
      subVal <- readline(prompt = "Narrow your search to a single table.\nEnter an integer from the list above: ");
      varVal <- varVal[as.integer(subVal)]
    }
  }
  dataSet <- tableDAP[[2]]$'Dataset ID'[varVal]
  title <- tableDAP[[2]]$'Title'[varVal]
  
  cat("You are collecting data for: \n",title)
  cat("\nThe 'Dataset ID' for this data is :\n",dataSet,"\n")
  
  dataSet <- tableDAP[[2]]$'Dataset ID'[varVal]
  dat <- read.csv(paste0("https://oceanview.pfeg.noaa.gov/erddap/tabledap/",dataSet,".csv?"), 
                  header = TRUE, 
                  stringsAsFactors = FALSE)
  
  return(dat)
}