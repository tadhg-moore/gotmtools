#' Create line plot from observed temperature in long format
#'
#' Plots a line plot of the temperatures with the lines coloured for each depth.
#'
#' @param data dataframe; observations loaded with load.obs
#' @param main character; Title of the graph. Defaults to 'Lineplot'
#' @param ylab character; Label of the y-axis. Defaults to 'Temperature'
#' @param xlim vector; Limits for the x-axis. Defaults to range of values in the data
#' @return Line plot of temperatures at different depths
#' @import ggplot2
#' @export
#'
long_lineplot <- function(data, rev.depths = FALSE, ylab = 'Temperature', main = 'Lineplot'){
  data[,2] <- factor(data[,2])
  if(rev.depths == TRUE){
    data[,2] <- factor(data[,2], rev(levels(data[,2])))
  }
  p1 <- ggplot(data, aes_string(names(data)[1], names(data)[3], colour = names(data)[2]))+
    geom_line()+
    ggtitle(main)+
    ylab(ylab)+
    theme_bw()
  return(p1)
}

