## ---- include = FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup----------------------------------------------------------
library(OEI)

## ---- include = TRUE------------------------------------------------

#pretend this is a data.frame from the db

#myDataFrame <- getTable()


myDataFrame <- data.frame(y = rnorm(100) 
                          ,x = sample(1998:2021,100,replace=TRUE)
                          #,Index = sample(c("SST","NPI","PTSD"), 100, replace=TRUE)
                          )
OEI::plot(myDataFrame)


## ---- include = TRUE------------------------------------------------

#pretend this is a data.frame from the db

#myDataFrame <- getTable()


myDataFrame <- data.frame(y = rnorm(100) 
                          ,x = sample(1998:2021,100,replace=TRUE)
                          ,Index = sample(c("SST","NPI","PTSD"), 100, replace=TRUE)
                          )
OEI::plot(myDataFrame)


## ---- include = TRUE------------------------------------------------

#pretend this is a data.frame from the db

#myDataFrame <- getTable()


myDataFrame <- data.frame(y = rnorm(100) 
                          ,x = sample(1998:2021,100,replace=TRUE)
                          ,Index = sample(c("SST","NPI","PTSD"), 100, replace=TRUE)
                          )
OEI::plot(myDataFrame, multipane = TRUE)


