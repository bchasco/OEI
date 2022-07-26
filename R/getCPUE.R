#'Read in information from database using 
#'
#'getCPUE
#'@param uid User ID
#'@param pwd Default password
#'@param db Database name
#'@param species a list of species (commonName and Age_ClassByLength)
#'@param studyType most likely 'Regular', but could also include 'Maximize Catch'
#'@param goodHaul use data from good hauls only?
#'@param day use hauls from daytime only?
#'@param includeRepeats include cpue from repeat trawls?
#'@return table A data.frame with the CPUE data from the selected species
#'@export getCPUE
getCPUE <- function(uid = uid,
                    pwd = pwd,
                    db = "NWFSC_OCEAN",
                    species = data.frame(commonName="Chinook salmon", Age_ClassByLength="yearling"),
                    studyType = c('Regular','Maximize Catch'),
                    goodHaul = TRUE,
                    day = TRUE,
                    includeRepeats=TRUE) {

  # Make sure the species list is the correct format
  if(!is.data.frame(species)){ 
    cat("Error: Species should be a data.frame with commonName and ageClass")
    return(NULL)
  }
  
  #Create the channel first. Notice we do not use library. The library is already "install"
  #in the description file. And you use :: to reference the function in RODBC
  channel <-   RODBC::odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  
  # Target stations
  JSOES_stations<-OEI::getTable(uid = uid, pwd = pwd, schemaName = 'crepo',
                           tableName = "JSOES_Target_Stations")
  # Get unique station codes
  #myStations<-getQuery(uid = uid, pwd = pwd, myQry = 'select distinct [Station Code] from crepo.[Trawl Contents]')
  myStations1<-OEI::getTable(uid = uid, pwd = pwd, schemaName = 'crepo',
                        tableName = "Trawl Info")
  myStations2<-OEI::getTable(uid = uid, pwd = pwd, schemaName = 'crepo',
                        tableName = "Station Info")
  myStations3<-OEI::getTable(uid = uid, pwd = pwd, schemaName = 'crepo',
                        tableName = "Cruise_List")
  myStations<-merge(myStations1, myStations2, by=c("Station Code", "Cruise #"))
  myStations<-merge(myStations, myStations3, by = "Cruise #")
  # Refine rows
  myStations<-myStations[myStations$GoodHaul==goodHaul &
                           myStations$Day==day &
                           myStations$`Study Type` %in% studyType,]
  if (!includeRepeats) myStations<-myStations[myStations$Repeat==0 &
                                                myStations$`Nonconsecutive Repeat`==0,]
  # Refine cols
  myStations<-myStations[,c("Station Code","Year","Month","Trawling distance (km)",
                            "GoodHaul","Day","Repeat","Nonconsecutive Repeat","Study Type")]

  colnames(myStations)<-c("Station Code","Year","Month","distTowed",
                          "GoodHaul","Day","Repeat","NonconsecRepeat","StudyType")
 
  # Add Station
  myStations$Station<-substr(myStations$`Station Code`, 7, 10)
  
  # get total number of individuals for all species
  speciesCounts<-OEI::getTable(uid = uid, pwd = pwd,
                          schemaName = 'dbo',
                          tableName = 'V_total_count_by_species_age')
  # Refine to just the data/species we want
  speciesCounts<-speciesCounts[speciesCounts$commonName %in% species$commonName,]

  # First, create the data.frame (using the speciesCount structure)
  mySpeciesCounts<-speciesCounts[0,]
  # Then fill it
  for (ss in 1:nrow(species))
    if (substr(species$commonName[ss], start = nchar(species$commonName[ss])-5,
               stop = nchar(species$commonName[ss])) == "salmon" |
        species$commonName[ss]=="Steelhead") {
      # Check both name and age class
      mySpeciesCounts<-rbind(mySpeciesCounts, speciesCounts[speciesCounts$commonName==species$commonName[ss] &
                                                              speciesCounts$Age_ClassByLength==species$Age_ClassByLength[ss],])
    } else {
      # Just check commonName 
      mySpeciesCounts<-rbind(mySpeciesCounts, speciesCounts[speciesCounts$commonName==species$commonName[ss],])
      
    }

  # Convert the data to wide format
  msc_wide<-tidyr::pivot_wider(data = mySpeciesCounts, id_cols = "Station Code",
                        names_from = c("commonName","Age_ClassByLength"),
                        values_from = "count",
                        names_sep = " ")
  
  # merge counts with Trawl Info to get km towed
  myData<-merge(myStations, msc_wide, all.x = TRUE)
  
  # Divide counts by distTowed
  for (cc in 11:ncol(myData)) myData[,cc]<-myData[,cc]/myData$distTowed
  
  # Convert all NAs to 0s
  myData[is.na(myData)]<-0
  
  #Return the data.frame object
  return(myData)

}
