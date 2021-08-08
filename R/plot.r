#' Title
#'
#' @param df data.frame
#'
#' @return ggplot
#' @export
#'
#' @examples plot
plot <- function(df=data.frame(x=1:4,y=1:4, c= NULL),
                 ylab="",
                 xlab=""){
  
  #ggplot object
  p <- ggplot2::ggplot(df,ggplot2::aes(x=x,y=y)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab)
  
  #Categorical data
  if(!is.null(df$c)){
    p <- p + 
      ggplot2::facet_wrap(~c, ncol=1)
  }
  
  #Return the plot
  print(p)
}
