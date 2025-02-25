#Define plotting functions used in script.
library(ggplot2)

#IBM colours - A vector of 5 good colour-blind friendly colours. Magenta, orange, purple, yellow, blue.
ibmcols <- c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
             rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
darkgrey <- rgb(64/255, 64/255, 64/255)

#Plot lines - just input matrix or data frame with first column as x and all other columns as values to plot.
plotlines <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, shade = FALSE, linetype = "solid", background = "transparent"){
  cat("\nCheck if your axes need units!\n ")
  
  #Check for shade.
  if(shade && ncol(data) == 3){
    return(plotlineswithshade(data, labels, xlimits, ylimits, colours, textsize, titletextoffset, removetitles, linewidth, xpand, ypand, xpandadd, ypandadd, xangle, xhjust, xvjust, borderthickness, xpandmultiplier))
  }
  
  #Convert to proper expands.
  if(is.null(xpand)){
    xpand <- c(max(floor(log10(abs(min(data[,1], na.rm = TRUE)))) + 1, 0) * xpandmultiplier, (floor(log10(abs(max(data[,1], na.rm = TRUE)))) + 1) * xpandmultiplier)
    if(!is.null(ylimits)){
      if(ylimits[1] == 0){
        xpand[1] <- 0
      }
    }
  }
  xpand <- ggplot2::expansion(mult = xpand, add = xpandadd)
  ypand <- ggplot2::expansion(mult = ypand, add = ypandadd)
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) != ncol(data) - 1){
      stop("Number of colours must be equal to number of lines.")
    }
  } else if(ncol(data) <= 6){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
                rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
    colours <- colours[1:(ncol(data) - 1)]
  }
  #Reshape data.
  data <- as.data.frame(data)
  colnames(data)[1] <- "x"
  if(nrow(data) < 3){
    return(NULL)
  }
  points <- tidyr::gather(data, key = "line", value = "y_value", -x)
  if(is.null(colours)){
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset), axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    }
  }
  #Change limits.
  if(!is.null(xlimits)){
    p <- p + ggplot2::scale_x_continuous(limits = xlimits, expand = xpand)
  } else {
    p <- p + ggplot2::scale_x_continuous(expand = xpand)
  }
  if(!is.null(ylimits)){
    p <- p + ggplot2::scale_y_continuous(limits = ylimits, expand = ypand)
  } else {
    p <- p + ggplot2::scale_y_continuous(expand = ypand)
  }
  return(suppressWarnings(print(p)))
}