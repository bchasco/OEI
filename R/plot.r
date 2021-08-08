#' Title
#'
#' @param df data.frame
#'
#' @return ggplot
#' @export
#'
#' @examples plot
<<<<<<< HEAD
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
=======
plot <- function(df=data.frame(x=1:4,y=1:4, c= NULL)){
  p <- ggplot2::ggplot(data=as.data.frame(df),ggplot2::aes(x=x,y=y)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()
>>>>>>> 5e10fd3f3156bee6c2b54cece77636d6d35f872a
  if(!is.null(df$c)){
    p <- p + 
      ggplot2::facet_wrap(~c, ncol=1)
  }
  
  #Return the plot
  print(p)
}
