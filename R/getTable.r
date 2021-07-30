getQuery <- function(uid = uid,
                    pwd = pwd,
                    db = db,
                    qryName = "SST"){
  
  #Create the channel first
  channel <- odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)
  #  
}
