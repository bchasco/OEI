#' Title
#'
#' @param df data.frame
#'
#' @return ggplot
#' @export
#'
#' @examples plot
plot <- function(df=data.frame(x=1:4,y=1:4, c= NULL)){
  p <- ggplot2::ggplot(data=as.data.frame(df),ggplot2::aes(x=x,y=y)) +
    ggplot2::geom_line()
  if(!is.null(df$c)){
    p <- p + 
      ggplot2::facet_wrap(~c)
  }
  print(p)
  print("test branch")
  print("Change to chasco branch")
}
