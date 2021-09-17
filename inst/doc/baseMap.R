## ---- include = FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup------------------------------------------------------------------------------
library(OEI)

## ----fig.width=4, fig.height=5, results="hide"------------------------------------------
# Build a simple base map
# You can create a map without capturing the ggplot object
OEI::baseMap()

## ----fig.width=4, fig.height=5----------------------------------------------------------
# Or, you can create a ggplot object and capture it
p <- OEI::baseMap()
# Then print it
print(p)

## ----fig.width=8, fig.height=9----------------------------------------------------------
# Styles
p1 <- OEI::baseMap(style = 1) + ggplot2::ggtitle('Style 1')
p2 <- OEI::baseMap(style = 2) + ggplot2::ggtitle('Style 2')
p3 <- OEI::baseMap(style = 3) + ggplot2::ggtitle('Style 3')
p4 <- OEI::baseMap(style = 4) + ggplot2::ggtitle('Style 4')

# And print them
figure <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
figure


## ----fig.width=4, fig.height=5----------------------------------------------------------

# And a shapefile
myPath<-file.path(paste0(normalizePath(".."),
                         "\\data\\shapefiles\\ne_10m_rivers_lake_centerlines.shp"))
rivers <- rgdal::readOGR(myPath)
f_rivers1<-ggplot2::fortify(rivers)

myPath<-file.path(paste0(normalizePath(".."),
                         "\\data\\shapefiles\\ne_10m_rivers_north_america.shp"))
rivers <- rgdal::readOGR(myPath)
f_rivers2<-ggplot2::fortify(rivers)

p1.shape<- p1 +
  ggplot2::geom_path(data = f_rivers1,
                   mapping=ggplot2::aes(x=long, y=lat, group=group), color="blue") +
  ggplot2::geom_path(data = f_rivers2,
                   mapping=ggplot2::aes(x=long, y=lat, group=group), color="blue")

p1.shape
  

