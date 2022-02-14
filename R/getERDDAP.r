#'Get an ERDDAP file
#'@param var=NULL Dataset ID for an erddap file (see 'https://oceanview.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000')  
#'@return ERDDAP data.frame
#'@examples #if you don't know the name of an erddap fileset just type
#'@examples #and you'll be prompted to choose a fileset
#'@examples getERDDAP()
#'@examples #you can either enter the row number that you're interested in 
#'@examples #or you can type something like 'PDO', and it will search the list for the PDO filesets
#'@examples #Alternatively,
#'@examples #If you don't want to look through all of the file sets, you just type
#'@examples x <- getERDDAP(var='cciea')
#'@examples #and you'll be asked to choose a row from the listed cciea filesets
#'@export getERDDAP
getERDDAP <- function(var = NULL){
  #Read in the table contents
  content <- rvest::read_html("https://oceanview.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000")
  tableDAP <- content %>% rvest::html_table(fill = TRUE)
  dataID <- as.data.frame(tableDAP[[2]]$'Dataset ID')
  print("test")
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
  
  cat("You are collecting test data for: \n",title)
  cat("\nThe 'Dataset ID' for this data is :\n",dataSet,"\n")
  
  dataSet <- tableDAP[[2]]$'Dataset ID'[varVal]

  varDAP <- rvest::read_html(paste0("https://oceanview.pfeg.noaa.gov/erddap/info/",dataSet,"/index.html"))
  varTable <- varDAP %>% rvest::html_table(fill = TRUE)
  varNames <- unique(varTable[[3]]$`Variable Name`)
  
  
  print(t(c("All",varNames[varNames!="NC_GLOBAL"])))
  #varVal <- readline(prompt = "Choose your variables (not functional yet):");
  print("Choose your variables (not functional yet):")
  
  dat <- read.csv(paste0("https://oceanview.pfeg.noaa.gov/erddap/tabledap/",dataSet,".csv?"), 
                  header = TRUE, 
                  stringsAsFactors = FALSE)
  
  return(dat)
}