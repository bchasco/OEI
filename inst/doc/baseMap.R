## ---- include = FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-----------------------------------------------------------------------------------------
library(OEI)

## --------------------------------------------------------------------------------------------------
#Build a simple base map
#You can create a map with capturing the ggplot object
OEI::baseMap()

## --------------------------------------------------------------------------------------------------
#Or, you can create a ggplot object and capture it
p <- OEI::baseMap()
#Then print it
print(p)

## --------------------------------------------------------------------------------------------------
#And this way you can easily manipulate the ggplot object
p <- OEI::baseMap() +
    ggplot2::ggtitle('Coastline of Washington and Oregon')+
  # # Grid lines
  ggplot2::theme_bw()

#Then print it
print(p)

## --------------------------------------------------------------------------------------------------
#And this way you can easily manipulate the ggplot object
p <- OEI::baseMap(stateBorders = TRUE,
                  bath = NA) +
    # Grid lines
    ggplot2::theme_bw()

#Then print it
print(p)

## --------------------------------------------------------------------------------------------------
#And this way you can easily manipulate the ggplot object
p <- OEI::baseMap(style = 2, stateBorders = TRUE) +
  # # Grid lines
  ggplot2::theme_bw()

#Then print it
print(p)

