#'Create the base ggplot map 
#'
#'baseMap
#'@param coord_lim a list of lat and long coordinates for the limits of the plot
#'@param bath a list of bathymetric coordinates for "lat" and "long", resolution, colour, and size
#'@param xlab x-axis label
#'@param ylab y-axis label
#'@param main main title for the plot
#'@param style (1 = vanilla, 2 = bathymetry)
#'@param stateBorders a vector of state borders c('washington','oregon') *no caps
#'@param wc_zm zoom level for style 4 watercolor map
#'@param legend for raster maps, should we show the legend? logical (T/F)
#'
#'@return ggplot map object
#'
#'@examples 
#' 
#' @export baseMap
#' 
baseMap <- function(coord_lim = list(lat=c(44, 49.1), long=c(-126.15, -122.12)), 
                    bath = list(include=TRUE, res=1, breaks=c(-100, -30), colour="grey", size=0.5),
                    style = 1,
                    xlab = 'Longitude',
                    ylab = 'Latitude',
                    main = '',
                    stateBorders = FALSE,
                    wc_zm=8,
                    legend=FALSE){
  
  #Grab the spatial data you need
  world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
  usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')
  #coast <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf")

  # If needed, download the bathymetry
  if(bath[[1]] | style %in% c(2,3)){
    # Contour lines
    b <- marmap::getNOAA.bathy(lon1 = coord_lim$long[1],
                               lon2 = coord_lim$long[2],
                               lat1 = coord_lim$lat[1],
                               lat2 = coord_lim$lat[2],
                               resolution = bath$res)
  }
    
  # Create the base map  
  gmap <- ggplot2::ggplot(data = world) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(main)

  # This is for the vanilla map
  if(style==1){
    gmap <- gmap + ggplot2::geom_sf(fill= 'antiquewhite') +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                           panel.background = ggplot2::element_rect(fill = 'aliceblue'))
  } 

  if(style==2) {  # Uses bathymetry to color the land
    gmap <- gmap + ggplot2::geom_raster(data = b, show.legend = legend,
                                        ggplot2::aes(x=x, y=y, fill=z)) +
            marmap::scale_fill_etopo()
  }
    
  if(style==3) {  # Uses getData to plot altitude - takes a long time to plot
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
    
    gmap <- gmap + ggplot2::geom_raster(data = b, show.legend = legend,
                                        ggplot2::aes(x=x, y=y, fill=z)) +
      marmap::scale_fill_etopo() +
      ggplot2::geom_tile(data = topo_pts, show.legend = legend, 
                                        ggplot2::aes(x=x, y=y, fill=z)) #+
      #ggplot2::scale_fill_gradientn(colours = terrain.colors(10)) +
      # ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
      #                panel.background = ggplot2::element_rect(fill = 'aliceblue'))
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

  if(bath[[1]]) gmap <- gmap + ggplot2::geom_contour(data=b, ggplot2::aes(x=x, y=y, z=z),
                                                     breaks = bath$breaks,
                                                     colour = bath$colour,
                                                     size = bath$size,
                                                     show.legend = legend)
  
  # Style 4 has to come after the state boundary and coord_sf
  #  because it does not play well with them.
  if(style==4) {  # Creates a ggmap object and plots watercolor
    bbox <- c(left = coord_lim$long[1], bottom = coord_lim$lat[1], right = coord_lim$long[2], top = coord_lim$lat[2])
    wc_map<-ggmap::get_stamenmap(bbox, zoom=wc_zm, maptype = "watercolor")
    gmap <- ggmap::ggmap(wc_map) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::ggtitle(main)
  }
  
  #Return the ggplot object
  return(gmap)
}