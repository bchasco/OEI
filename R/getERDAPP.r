getERDAPP <- function(var = NULL){
  #Read in the table contents
  content <- rvest::read_html("https://oceanview.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000")
  tableDAP <- content %>% rvest::html_table(fill = TRUE)
  dataID <- as.data.frame(tableDAP[[2]]$'Dataset ID')
  if(is.null(var)){
    print(dataID)
    var <- readline(prompt = "Enter any number or grep a name: ");
  }  
  if(!is.null(var)){
    if(!is.na(as.numeric(var))){
      varVal <- as.integer(var)
    }
    if(is.na(as.numeric(var))){
      varVal <- grep(var,tableDAP[[2]]$'Dataset ID')
      print("varVal")
      print(varVal)
      if(length(varVal)>1){
        print(tableDAP[[2]]$'Title'[varVal])
        subVal <- readline(prompt = "Narrow your search to a single table. Enter an integer from the list above: ");
        varVal <- varVal[as.integer(subVal)]
      }
    }
    cat("You are collecting data for: \n", 
                tableDAP[[2]]$'Title'[varVal])
    cat("\nThe 'Dataset ID' for this data is :\n", 
                tableDAP[[2]]$'Dataset ID'[varVal],"\n")
  }
  dataSet <- tableDAP[[2]]$'Dataset ID'[varVal]
  dat <- read.csv(paste0("https://oceanview.pfeg.noaa.gov/erddap/tabledap/",dataSet,"?"), 
                  header = TRUE, 
                  stringsAsFactors = FALSE)
}