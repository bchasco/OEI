#' Title
#'
#' @param df data.frame with the following column headings: x, y, Index
#' @param multipane Whether you want multicolors or multiple panes
#' @return ggplot
#' @export
#'
#' @examples plot
plot <- function(df=data.frame(x=1:4,y=1:4),
                 ylab="",
                 xlab="",
                 multipane=FALSE){
  
  #ggplot object
  p <- ggplot2::ggplot(df,ggplot2::aes(x=x,y=y)) +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab)
  
  #Categorical data
  if(!is.null(df$Index)){
    if(multipane==TRUE){
      p <- p + 
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~Index, ncol=1)
    }
    if(multipane==FALSE){
      p <- p + 
        ggplot2::geom_point(ggplot2::aes(colour=Index))
    }
  }else{
    p <- p +
      ggplot2::geom_point()
  }
  
  #Return the plot
  print(p)
}
