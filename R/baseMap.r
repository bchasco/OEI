#'Create the base ggplot map 
#'
#'baseMap
#'@param coord_lim a list of lat and long coordinates for the limits of the plot
#'@param bath a list of bathymetric coordinates for "lat" and "long", resolution, colour, and size
#'@param xlab x-axis label
#'@param ylab y-axis label
#'@param style (1 = vanilla)
#'@param stateBorders a vector of state borders c('washington','oregon') *no caps
#'
#'@return ggplot map object
#'
#'@examples 
#' 
#' @export baseMap
#' 
baseMap <- function(coord_lim = list(lat=c(44, 49.1), long=c(-126.15, -122.12)), 
                    bath = list(lat=c(44, 49.1), long=c(-126.15, -122.12), res=3, breaks=c(-100), colour="grey", size=0.5),
                    style = 1,
                    xlab = 'Longitude',
                    ylab = 'Latitude',
                    stateBorders = FALSE){
  
  #Grab the spatial data you need
  world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  coast <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf")

  gmap <- ggplot2::ggplot(data = world)  
  
  #I'd lay down the bathymetry first.
  if(is.list(bath)){
    b <- marmap::getNOAA.bathy(lon1 = bath$lon[1],
                               lon2 = bath$lon[2],
                               lat1 = bath$lat[1],
                               lat2 = bath$lat[2],
                               resolution = bath$res)
    # Contour lines
    gmap <- gmap + ggplot2::geom_contour(data=b, aes(x=x, y=y, z=z),
                                         breaks = bath$breaks,
                                         colour = bath$colour,
                                         size = bath$size)
  }
  
  #This is for the vanilla map
  if(style==1){
    gmap <- gmap + geom_sf(fill= 'antiquewhite') +
      ggplot2::coord_sf(xlim = coord_lim$long, ylim = coord_lim$lat, expand = FALSE) +
      ggplot2::xlab('Longitude') +
      ggplot2::ylab('Latitude')
  }
    
  if(stateBorders){
    # State boundaries
    # map <- ggplot2::map_data("state")
    # mapdata <- map[map$region%in%c("oregon", "washington"),]
    
    gmap <- gmap + 
      # ggplot2::coord_sf(xlim = coord_lim$long, ylim = coord_lim$lat, expand = FALSE) +
      # ggplot2::coord_map()+
      ggplot2::borders("state")

  }


  #Return the ggplot object
  return(gmap)
}