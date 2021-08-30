#'Create the base ggplot map 
#'
#'baseMap
#'@param coord_lim a list of lat and long coordinates for the limits of the plot
#'@param bath a list of bathymetric coordinates for "lat" and "long", resolution, colour, and size
#'@param xlab x-axis label
#'@param ylab y-axis label
#'@param style (1 = vanilla, 2 = bathymetry)
#'@param stateBorders a vector of state borders c('washington','oregon') *no caps
#'
#'@return ggplot map object
#'
#'@examples 
#' 
#' @export baseMap
#' 
baseMap <- function(coord_lim = list(lat=c(44, 49.1), long=c(-126.15, -122.12)), 
                    bath = list(res=1, breaks=c(-100, -30), colour="grey", size=0.5),
                    style = 1,
                    xlab = 'Longitude',
                    ylab = 'Latitude',
                    stateBorders = FALSE){
  
  #Grab the spatial data you need
  world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
  usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')
  coast <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf")
  
  gmap <- ggplot2::ggplot(data = world) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  # 
  # 
  # #This is for the vanilla map
  if(style==1){
    gmap <- gmap + ggplot2::geom_sf(fill= 'antiquewhite') +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                           panel.background = ggplot2::element_rect(fill = 'aliceblue'))
  } 

  #I'd lay down the bathymetry first.
  if(is.list(bath)){
    # Contour lines
    b <- marmap::getNOAA.bathy(lon1 = coord_lim$lon[1],
                               lon2 = coord_lim$lon[2],
                               lat1 = coord_lim$lat[1],
                               lat2 = coord_lim$lat[2],
                               resolution = bath$res)
    
    if(style==2) {  # Uses bathymetry to color the land
      gmap <- gmap + ggplot2::geom_raster(data = b, 
                                          ggplot2::aes(x=x, y=y, fill=z)) +
              marmap::scale_fill_etopo()
    }
    
    if(style==3) {  # Uses getData to plot altitude
      # usa_topo <- getData('alt', country='USA', level=1)
      # can_topo <- getData('alt', country='CAN', level=1)
      topo1 <- raster::getData('SRTM', lat=44, lon=-121)
      topo2 <- raster::getData('SRTM', lat=46, lon=-121)
      topo3 <- raster::getData('SRTM', lat=50, lon=-126)
      topo <- raster::mosaic(topo1, topo2, topo3, fun=mean)
      #create polygon to crop the elevation data file   
      Ps1 = as(raster::extent(c(-128,-120,40,50)), 'SpatialPolygons')
      raster::crs(Ps1) = "+proj=longlat +datum=WGS84 +no_defs"
      #crop the elevation data using the polygon
      topo = raster::crop(topo, Ps1, snap= 'out')
      #lower the reolution to enable faster plotting
      topo_lower_res <- raster::aggregate(topo, fact=5)
      topo_pts <- data.frame(raster::rasterToPoints(topo_lower_res))
      colnames(topo_pts) = c("x", "y", "z")
      
      gmap <- gmap + ggplot2::geom_raster(data = b, 
                                          ggplot2::aes(x=x, y=y, fill=z)) +
        marmap::scale_fill_etopo() +
        ggplot2::geom_tile(data = topo_pts, 
                                          ggplot2::aes(x=x, y=y, fill=z)) #+
        #ggplot2::scale_fill_gradientn(colours = terrain.colors(10)) +
        # ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
        #                panel.background = ggplot2::element_rect(fill = 'aliceblue'))
    }

    gmap <- gmap + ggplot2::geom_contour(data=b, ggplot2::aes(x=x, y=y, z=z),
                                         breaks = bath$breaks,
                                         colour = bath$colour,
                                         size = bath$size)
  }
  
  if(stateBorders){
    # State boundaries
    gmap <- gmap +
      ggplot2::geom_sf(data=usa_states,
                       colour = "grey40",
                       fill=NA)
  }

  if(is.list(coord_lim)){
    gmap <- gmap +
      ggplot2::coord_sf(xlim = coord_lim$long, ylim = coord_lim$lat, expand = FALSE)

  }


  #Return the ggplot object
  return(gmap)
}