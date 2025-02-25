#IBM colours - A vector of 5 good colour-blind friendly colours. Magenta, orange, purple, yellow, blue.
ibmcols <- c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
             rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))

darkgrey <- rgb(64/255, 64/255, 64/255)

#Plot point cloud - just input points matrix or data frame with two columns for x and y.
plotpointcloud <- function(points, labels = c("x", "y"), textsize = 20, titletextoffset = 4, removetitles = FALSE, pointsize = 1.25, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", xlimits = NULL, ylimits = NULL){
  #Get labels.
  xlab <- labels[1]
  ylab <- labels[2]
  #Set third column.
  if(ncol(points) == 2){
    points <- cbind(points, 0)
  }
  #Get limits.
  if(is.null(xlimits)){
    xlimits <- c(min(points[,1], na.rm = TRUE), max(points[,1], na.rm = TRUE))
  }
  if(is.null(ylimits)){
    ylimits <- c(min(points[,2], na.rm = TRUE), max(points[,2], na.rm = TRUE))
  }
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < length(unique(points[,3])) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(length(unique(points[,3])) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rgb(64/255, 64/255, 64/255), rep(colours, length.out = length(unique(points[,3])) - 1))
  }
  colnames(points) <- c("x", "y", "t")
  points <- as.data.frame(points)
  if(is.null(colours)){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#Plot heat map - input a matrix of numbers or data frame of values in x and y. Third value of labels will be used as legend title if given.
plotheatmap <- function(data, labels = c("x", "y"), limits = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, background = "transparent", includelegend = FALSE, breaks = NULL, xbreaks = NULL, ybreaks = NULL){
  heatmapexpand <- 0.01
  #Check for data format.
  if(ncol(data) != 3){
    points <- matrix(0, nrow = ncol(data) * nrow(data), ncol = 3)
    xlims <- 1:ncol(data)
    ylims <- 1:nrow(data)
    vals <- combineparameters(list(1:ncol(data), 1:nrow(data)))
    for(i in 1:nrow(vals)){
      points[i,] <- c(vals[i,1], nrow(data) - vals[i,2] + 1, data[vals[i,2], vals[i,1]])
    }
    points <- as.data.frame(points)
  } else {
    points <- as.data.frame(data)
    xlims <- c(min(points[,1]), max(points[,1]))
    ylims <- c(min(points[,2]), max(points[,2]))
  }
  #Get limits.
  if(is.null(limits)){
    limits <- c(min(points[,3]), max(points[,3]))
  }
  #Get breaks.
  if(is.null(breaks)){
    breaks <- limits
  }
  if(is.null(xbreaks)){
    xbreaks <- xlims
  }
  if(is.null(ybreaks)){
    ybreaks <- ylims
  }
  colnames(points) <- c("x", "y", "t")
  points <- as.data.frame(points)
  if(length(labels) == 3){
    legendtitle <- labels[3]
  } else {
    legendtitle = " "
  }
  if(includelegend){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_raster(ggplot2::aes(fill = t)) +
        ggplot2::labs(x = "x", y = "y") +
        ggplot2::scale_x_continuous(expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", limits = limits, na.value = "black", breaks = breaks, labels = c(sprintf("%.2f", min(limits)), sprintf("%.2f", max(limits)))) + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       text = ggplot2::element_text(family = "serif"),
                       legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = textsize),
                       legend.key.height = ggplot2::unit(1, "cm"), legend.key.width = ggplot2::unit(1, "cm"), legend.background = ggplot2::element_rect(fill = background))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_raster(ggplot2::aes(fill = t)) +
        ggplot2::labs(x = labels[1], y = labels[2], fill = legendtitle) +
        ggplot2::scale_x_continuous(breaks = xbreaks, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(breaks = ybreaks, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", limits = limits, na.value = "black", breaks = breaks, labels = c(sprintf("%.2f", min(limits)), sprintf("%.2f", max(limits)))) + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       text = ggplot2::element_text(family = "serif"),
                       legend.title = ggplot2::element_text(size = textsize), legend.text = ggplot2::element_text(size = textsize),
                       legend.key.height = ggplot2::unit(1, "cm"), legend.key.width = ggplot2::unit(1, "cm"), legend.background = ggplot2::element_rect(fill = background))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_raster(ggplot2::aes(fill = t)) +
        ggplot2::labs(x = "x", y = "y") +
        ggplot2::scale_x_continuous(expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", limits = limits, breaks = breaks, na.value = "black") + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_raster(ggplot2::aes(fill = t)) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::scale_x_continuous(breaks = xbreaks, expand = c(0.01, 0.01)) +
        ggplot2::scale_y_continuous(breaks = ybreaks, expand = c(0.01, 0.01)) +
        ggplot2::scale_fill_viridis_c(option = "magma", limits = limits, breaks = breaks, na.value = "black") + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } 
  }
}

#Plot lines - just input matrix or data frame with first column as x and all other columns as values to plot.
plotlines <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, shade = FALSE, linetype = "solid", background = "transparent"){
  
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

#Plot bar chart - input matrix or data frame with first column as type and second column as value. If multiple values of the same type are given, all will be averaged.
plotbarchart <- function(data, labels = c("x", "y"), ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, background = "transparent"){
  
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) != nrow(data)){
      stop("Number of colours must be equal to number of lines.")
    }
  } else if(nrow(data) <= 6){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
                rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
    colours <- colours[1:(nrow(data))]
  }
  
  #Reshape data.
  data <- as.data.frame(data)
  colnames(data) <- c("type", "value")
  
  #Check if multiple values of same type are given.
  if(length(unique(data[,1])) != nrow(data)){
    #Take mean of each unique type.
    uniques <- unique(data[,1])
    newdata <- c()
    #Iterate over each type.
    for(typ in uniques){
      newdata <- c(newdata, mean(data[data[,1] == typ, 2], na.rm = TRUE))
    }
    data <- data.frame(type = uniques, value = newdata)
  }
  
  if(removetitles){
    p <- 
      ggplot2::ggplot(data, ggplot2::aes(x = type, y = value, fill = type)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                     text = ggplot2::element_text(family = "serif"))
  } else {
    p <- 
      ggplot2::ggplot(data, ggplot2::aes(x = type, y = value, fill = as.factor(type))) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::labs(x = labels[1], y = labels[2]) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                     axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                     text = ggplot2::element_text(family = "serif"))
  }
  
  #Change limits.
  if(!is.null(ylimits)){
    p <- p + ggplot2::coord_cartesian(ylim = ylimits)
  }
  return(suppressWarnings(print(p)))
}

#Plot histogram - input a vector of values, or a data frame with two columns: the first is type, the second is data points.
plothistogram <- function(data, labels = c("x", "y"), binwidth = 1, alpha = 1, xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, linetype = "solid", background = "transparent", addline = TRUE){
  
  #Reformat data.
  points <- as.data.frame(data)
  if(ncol(points) == 1){
    points <- cbind(1, points)
  }
  colnames(points) <- c("line", "x")
  
  #Convert to proper expands.
  if(is.null(xpand)){
    xpand <- c(max(floor(log10(abs(min(points[,2], na.rm = TRUE)))) + 1, 0) * xpandmultiplier, (floor(log10(abs(max(points[,2], na.rm = TRUE)))) + 1) * xpandmultiplier)
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
    if(length(colours) != length(unique(points[,1]))){
      stop("Number of colours must be equal to number of lines.")
    }
  } else if(length(unique(points[,1])) <= 5){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
                rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
    colours <- colours[1:length(unique(points[,1]))]
  }
  
  #Plot.
  if(addline){
    p <- rep(darkgrey, length(colours))
  } else {
    p <- colours
  }
  if(removetitles){
    p <- 
      ggplot2::ggplot(points, ggplot2::aes(x = x, fill = as.factor(line), colour = as.factor(line))) +
      ggplot2::geom_histogram(binwidth = binwidth, position = "identity", alpha = alpha) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::scale_colour_manual(values = p) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                     text = ggplot2::element_text(family = "serif"))
  } else {
    p <- 
      ggplot2::ggplot(points, ggplot2::aes(x = x, fill = as.factor(line), colour = as.factor(line))) +
      ggplot2::geom_histogram(binwidth = binwidth, position = "identity", alpha = alpha) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::scale_colour_manual(values = p) +
      ggplot2::labs(x = labels[1], y = labels[2]) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                     axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                     text = ggplot2::element_text(family = "serif"))
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

#Plot histogram - input a vector of values, or a data frame with two columns: the first is type, the second is data points.
plotfrequencyhistogram <- function(data, labels = c("x", "y"), binwidth = 1, alpha = 1, xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, linetype = "solid", background = "transparent", addline = TRUE){
  
  #Reformat data.
  points <- as.data.frame(data)
  if(ncol(points) == 1){
    points <- cbind(1, points)
  }
  colnames(points) <- c("line", "x")
  
  #Convert to proper expands.
  if(is.null(xpand)){
    xpand <- c(max(floor(log10(abs(min(points[,2], na.rm = TRUE)))) + 1, 0) * xpandmultiplier, (floor(log10(abs(max(points[,2], na.rm = TRUE)))) + 1) * xpandmultiplier)
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
    if(length(colours) != length(unique(points[,1]))){
      stop("Number of colours must be equal to number of lines.")
    }
  } else if(length(unique(points[,1])) <= 5){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
                rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
    colours <- colours[1:length(unique(points[,1]))]
  }
  
  #Plot.
  if(addline){
    p <- rep(darkgrey, length(colours))
  } else {
    p <- colours
  }
  if(removetitles){
    p <- 
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = ..count../sum(..count..), fill = as.factor(line), colour = as.factor(line))) +
      ggplot2::geom_histogram(binwidth = binwidth, position = "identity", alpha = alpha) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::scale_colour_manual(values = p) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                     text = ggplot2::element_text(family = "serif"))
  } else {
    p <- 
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = ..count../sum(..count..), fill = as.factor(line), colour = as.factor(line))) +
      ggplot2::geom_histogram(binwidth = binwidth, position = "identity", alpha = alpha) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::scale_colour_manual(values = p) +
      ggplot2::labs(x = labels[1], y = labels[2]) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                     axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                     text = ggplot2::element_text(family = "serif"))
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

#Plot 2D Histogram.
plot2Dhistogram <- function(points, labels = c("x", "y"), textsize = 20, titletextoffset = 4, removetitles = FALSE, background = "transparent", includelegend = FALSE, xbreaks = NULL, ybreaks = NULL, numberofbins = 20, binspacing = 2, xlims = NULL, ylims = NULL){
  heatmapexpand <- 0.05
  #Check for data format.
  points <- as.data.frame(points)
  if(is.null(xlims)){
    xlims <- c(floor(min(points[,1])), ceiling(max(points[,1])))
  }
  if(is.null(ylims)){
    ylims <- c(floor(min(points[,2])), ceiling(max(points[,2])))
  }
  #Get breaks.
  if(is.null(xbreaks)){
    xbreaks <- seq(xlims[1], xlims[2], ceiling(numberofbins/binspacing))
  }
  if(is.null(ybreaks)){
    ybreaks <- seq(ylims[1], ylims[2], ceiling(numberofbins/binspacing))
  }
  #Get labels.
  xlab <- labels[1]
  ylab <- labels[2]
  colnames(points) <- c("x", "y")
  if(includelegend){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bin2d(bins = numberofbins) +
        ggplot2::labs(x = "x", y = "y") +
        ggplot2::scale_x_continuous(limits = xlims, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(limits = ylims, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", na.value = "black") + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       text = ggplot2::element_text(family = "serif"),
                       legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = textsize),
                       legend.key.height = ggplot2::unit(1.35, "cm"), legend.key.width = ggplot2::unit(1, "cm"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bin2d(bins = numberofbins) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", na.value = "black") + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       text = ggplot2::element_text(family = "serif"),
                       legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = textsize),
                       legend.key.height = ggplot2::unit(1.35, "cm"), legend.key.width = ggplot2::unit(1, "cm"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bin2d(bins = numberofbins) +
        ggplot2::labs(x = "x", y = "y") +
        ggplot2::scale_x_continuous(limits = xlims, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(limits = ylims, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", na.value = "black") + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bin2d(bins = numberofbins) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(heatmapexpand, heatmapexpand)) +
        ggplot2::scale_fill_viridis_c(option = "magma", na.value = "black") + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } 
  }
}

#Plot box plot - input data where each column is a separate variable.
plotboxplot <- function(points, labels = c("x", "y"), axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", linethickness = 1, sortdata = FALSE){
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < ncol(points) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(ncol(points) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rep(colours, length.out = ncol(points)))
  }
  #Sort points.
  if(sortdata){
    data <- data.frame(type = 0, value = 0)
    data <- data[-1,]
    for(i in 1:ncol(points)){
      sub <- cbind(labels[i], points[,i])
      colnames(sub) <- c("type", "value")
      data <- rbind(data, sub)
    }
    points <- data
  }
  points <- as.data.frame(points)
  colnames(points) <- c("type", "value")
  uniques <- unique(points$type)
  if(prod(is.numeric(uniques)) == 1){
    uniques <- sort(uniques)
  }
  points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  points[,2] <- as.numeric(points[,2])
  #Create plots.
  if(is.null(colours)){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#Plot box plot with significance - does same as boxplot but adds a significance bar on the figure. Only works with two variables though.
plotboxplotwithsignificance <- function(points, labels = c("x", "y"), axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", linethickness = 1, sortdata = FALSE){
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < ncol(points) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(ncol(points) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rep(colours, length.out = ncol(points)))
  }
  #Sort points.
  if(sortdata){
    data <- data.frame(type = 0, value = 0)
    data <- data[-1,]
    for(i in 1:ncol(points)){
      sub <- cbind(labels[i], points[,i])
      colnames(sub) <- c("type", "value")
      data <- rbind(data, sub)
    }
    points <- data
  }
  points <- as.data.frame(points)
  colnames(points) <- c("type", "value")
  uniques <- unique(points$type)
  if(prod(is.numeric(uniques)) == 1){
    uniques <- sort(uniques)
  }
  points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  points[,2] <- as.numeric(points[,2])
  #Create plots.
  if(is.null(colours)){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = type, y = value, fill = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = type, y = value, fill = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = type, y = value, fill = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = type, y = value, fill = as.factor(type))) +
        ggplot2::geom_boxplot(lwd = linethickness) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#Plot violin plot - input data where each column is a separate variable.
plotviolinplot <- function(points, labels = c("x", "y"), axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", linethickness = 1, sortdata = FALSE){
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < ncol(points) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(ncol(points) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rep(colours, length.out = ncol(points)))
  }
  #Sort points.
  if(sortdata){
    data <- data.frame(type = 0, value = 0)
    data <- data[-1,]
    for(i in 1:ncol(points)){
      sub <- cbind(labels[i], points[,i])
      colnames(sub) <- c("type", "value")
      data <- rbind(data, sub)
    }
    points <- data
  }
  points <- as.data.frame(points)
  colnames(points) <- c("type", "value")
  uniques <- unique(points$type)
  if(prod(is.numeric(uniques)) == 1){
    uniques <- sort(uniques)
  }
  points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  points[,2] <- as.numeric(points[,2])
  #Create plots.
  if(is.null(colours)){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#Plot violin plot with significance - does same as boxplot but adds a significance bar on the figure. Only works with two variables though.
plotviolinplotwithsignificance <- function(points, labels = c("x", "y"), axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", linethickness = 1, sortdata = FALSE){
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < ncol(points) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(ncol(points) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rep(colours, length.out = ncol(points)))
  }
  #Sort points.
  if(sortdata){
    data <- data.frame(type = 0, value = 0)
    data <- data[-1,]
    for(i in 1:ncol(points)){
      sub <- cbind(labels[i], points[,i])
      colnames(sub) <- c("type", "value")
      data <- rbind(data, sub)
    }
    points <- data
  }
  points <- as.data.frame(points)
  colnames(points) <- c("type", "value")
  uniques <- unique(points$type)
  if(prod(is.numeric(uniques)) == 1){
    uniques <- sort(uniques)
  }
  points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  points[,2] <- as.numeric(points[,2])
  #Create plots.
  if(is.null(colours)){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, fill = as.factor(type), group = as.factor(type))) +
        ggplot2::geom_violin(lwd = linethickness) +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar", color = "black", width = 0.5) +
        ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#Plot bee swarm - input data where each column is a separate variable.
plotbeeswarm <- function(points, labels = c("x", "y"), axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", cex = 1, pointsize = 2, sortdata = FALSE){
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < ncol(points) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(ncol(points) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rep(colours, length.out = ncol(points)))
  }
  #Sort points.
  if(sortdata){
    data <- data.frame(type = 0, value = 0)
    data <- data[-1,]
    for(i in 1:ncol(points)){
      sub <- cbind(labels[i], points[,i])
      colnames(sub) <- c("type", "value")
      data <- rbind(data, sub)
    }
    points <- data
  }
  points <- as.data.frame(points)
  colnames(points) <- c("type", "value")
  uniques <- unique(points$type)
  if(prod(is.numeric(uniques)) == 1){
    uniques <- sort(uniques)
  }
  points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  points[,2] <- as.numeric(points[,2])
  #Create plots.
  if(removetitles){
    ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, colour = as.factor(type), group = as.factor(type))) +
      ggbeeswarm::geom_beeswarm(cex = cex, size = pointsize) +
      ggplot2::labs(x = axes[1], y = axes[2]) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                     legend.position = "none", text = ggplot2::element_text(family = "serif"))
  } else {
    ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, colour = as.factor(type), group = as.factor(type))) +
      ggbeeswarm::geom_beeswarm(cex = cex, size = pointsize) +
      ggplot2::labs(x = axes[1], y = axes[2]) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                     axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                     legend.position = "none", text = ggplot2::element_text(family = "serif"))
  }
}

#Plot bee swarm with significance - does same as beeswarm but adds a significance bar on the figure. Only works with two variables though.
plotbeeswarmwithsignificance <- function(points, labels = c("x", "y"), axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", cex = 1, pointsize = 2, sortdata = FALSE, ylimits = NULL){
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < ncol(points) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(ncol(points) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rep(colours, length.out = ncol(points)))
  }
  #Sort points.
  if(sortdata){
    data <- data.frame(type = 0, value = 0)
    data <- data[-1,]
    for(i in 1:ncol(points)){
      sub <- cbind(labels[i], points[,i])
      colnames(sub) <- c("type", "value")
      data <- rbind(data, sub)
    }
    points <- data
  }
  points <- as.data.frame(points)
  colnames(points) <- c("type", "value")
  uniques <- unique(points$type)
  if(prod(is.numeric(uniques)) == 1){
    uniques <- sort(uniques)
  }
  points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  points[,2] <- as.numeric(points[,2])
  #Create plots.
  if(removetitles){
    p <- ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, group = as.factor(type))) +
      ggbeeswarm::geom_beeswarm(ggplot2::aes(colour = as.factor(type)), cex = cex, size = pointsize) +
      ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE, textsize = textsize / 2) +
      ggplot2::labs(x = axes[1], y = axes[2]) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::scale_y_continuous(expand = c(0,0), limits = ylimits) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                     legend.position = "none", text = ggplot2::element_text(family = "serif"))
  } else {
    p <- ggplot2::ggplot(points, ggplot2::aes(x = as.factor(type), y = value, group = as.factor(type))) +
      ggbeeswarm::geom_beeswarm(ggplot2::aes(colour = as.factor(type)), cex = cex, size = pointsize) +
      ggsignif::geom_signif(comparisons = list(c(labels[1], labels[2])), map_signif_level = TRUE, textsize = textsize / 2) +
      ggplot2::labs(x = axes[1], y = axes[2]) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                     axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                     legend.position = "none", text = ggplot2::element_text(family = "serif"))
  }
  #Change limits.
  if(!is.null(ylimits)){
    p <- p + ggplot2::scale_y_continuous(limits = ylimits, expand = c(0,0))
  }
  return(suppressWarnings(print(p)))
}

#Plot lines with shade - input matrix with first column as x, and second and third columns as the y values. Adds shade between the two. Outputs as default square panel.
plotlineswithshade <- function(data, labels = c("x", "y", "z"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, linetype = "solid", background = "transparent"){
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
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  #Reshape data.
  points <- as.data.frame(data)
  colnames(points) <- c("x", "y1", "y2")
  if(is.null(colours)){
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = linetype, color = rgb(245/255, 93/255, 0/255)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = y1, ymax = y2), fill = rgb(101/255, 143/255, 255/255), alpha = 0.25) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = linetype, color = rgb(245/255, 93/255, 0/255)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = y1, ymax = y2), fill = rgb(101/255, 143/255, 255/255), alpha = 0.25) +
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
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = linetype, color = colours[2]) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = y1, ymax = y2), fill = colours[3], alpha = 0.25) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = linetype, color = colours[2]) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = y1, ymax = y2), fill = colours[3], alpha = 0.25) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
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

#Plot lines with best fit - input matrix with first column as x and second column as y. Set fit variable to a function you'd like to fit to. Leave as NULL for linear. Outputs as default square panel.
plotlineswithfit <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, linetype = "solid", background = "transparent", fit = NULL){
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
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  #Get fit.
  if(is.null(fit)){
    fit <- function(x){
      return(x)
    }
  }
  #Reshape data.
  points <- as.data.frame(cbind(data, 0))
  #Remove undefined entries.
  points <- points[!is.na(fit(points[,1])) & !is.infinite(fit(points[,1])),]
  #If empty, return NULL.
  if(nrow(points) == 0){
    return(NULL)
  }
  colnames(points) <- c("x", "y1", "y2")
  model <- lm(y1 ~ fit(x), data = points)
  coeff = model[[1]]
  points[,3] <- coeff[1] + coeff[2] * fit(points[,1])
  if(is.null(colours)){
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = rgb(245/255, 93/255, 0/255)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = rgb(245/255, 93/255, 0/255)) +
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
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = colours[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = colours[2]) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
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
  suppressWarnings(print(p))
  return(model)
}

#Plot lines with best fit in square format.
plotfitsquare <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 16, titletextoffset = 2, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0.01), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 0.75, xpandmultiplier = 0.02, shade = FALSE, linetype = "solid", background = "transparent", fit = NULL){
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
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  #Get fit.
  if(is.null(fit)){
    fit <- function(x){
      return(x)
    }
  }
  #Reshape data.
  points <- as.data.frame(cbind(data, 0))
  colnames(points) <- c("x", "y1", "y2")
  #Find undefined entries.
  points <- points[!is.na(fit(points[,1])) & !is.infinite(fit(points[,1])),]
  #If empty, return NULL.
  if(nrow(points) == 0){
    return(NULL)
  }
  model <- lm(y1 ~ fit(x), data = points)
  coeff = model[[1]]
  points[,3] <- coeff[1] + coeff[2] * fit(points[,1])
  if(is.null(colours)){
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = rgb(245/255, 93/255, 0/255)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = rgb(245/255, 93/255, 0/255)) +
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
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = colours[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = colours[2]) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
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
  suppressWarnings(print(p))
  return(mean(model[[2]] ** 2), na.rm = TRUE)
}

#Plot lines with best fit in rectangle format.
plotfitrectangle <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 16, titletextoffset = 2, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0.01), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 0.75, xpandmultiplier = 0.01, shade = FALSE, linetype = "solid", background = "transparent", fit = NULL){
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
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  #Get fit.
  if(is.null(fit)){
    fit <- function(x){
      return(x)
    }
  }
  #Reshape data.
  points <- as.data.frame(cbind(data, 0))
  colnames(points) <- c("x", "y1", "y2")
  #Remove undefined entries.
  points <- points[!is.na(fit(points[,1])) & !is.infinite(fit(points[,1])),]
  #If empty, return NULL.
  if(nrow(points) == 0){
    return(NULL)
  }
  model <- lm(y1 ~ fit(x), data = points)
  coeff = model[[1]]
  points[,3] <- coeff[1] + coeff[2] * fit(points[,1])
  if(is.null(colours)){
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = rgb(245/255, 93/255, 0/255)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = rgb(212/255, 30/255, 125/255)) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = rgb(245/255, 93/255, 0/255)) +
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
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = colours[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = y1), linewidth = linewidth, linetype = linetype, color = colours[1]) +
        ggplot2::geom_line(ggplot2::aes(y = y2), linewidth = linewidth, linetype = "dashed", color = colours[2]) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
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
  suppressWarnings(print(p))
  return(mean(model[[2]] ** 2), na.rm = TRUE)
}

#Consistent line plots - input a list of matrices or data frames and it will plot all of them separately but with consistent style.
plotlinesconsistent <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, shade = FALSE, linetype = "solid", background = "transparent", mode = "default"){
  #If limits are not specified, get maximum x and y limits from data.
  minx <- 0
  maxx <- 0
  miny <- 0
  maxy <- 0
  for(dataset in data){
    #Check each of the minimums and maximums.
    if(minx > min(dataset[,1], na.rm = TRUE)){
      minx <- min(dataset[,1], na.rm = TRUE)
    }
    if(maxx < max(dataset[,1], na.rm = TRUE)){
      maxx <- max(dataset[,1], na.rm = TRUE)
    }
    if(miny > min(dataset[,2:ncol(dataset)], na.rm = TRUE)){
      miny <- min(dataset[,2:ncol(dataset)], na.rm = TRUE)
    }
    if(maxy < max(dataset[,2:ncol(dataset)], na.rm = TRUE)){
      maxy <- max(dataset[,2:ncol(dataset)], na.rm = TRUE)
    }
  }
  if(is.null(xlimits)){
    xlimits <- c(minx, maxx)
  }
  if(is.null(ylimits)){
    ylimits <- c(miny, maxy)
  }
  
  #Convert to proper expands.
  if(is.null(xpand)){
    xpand <- c(max(floor(log10(abs(minx))) + 1, 0) * 0.01, (floor(log10(abs(maxx))) + 1) * 0.01)
    if(!is.null(ylimits)){
      if(ylimits[1] == 0){
        xpand[1] <- 0
      }
    }
  }
  
  #Check type.
  if(tolower(mode) == "default"){
    #Iterate over each entry.
    return(lapply(data, function(data){
      #Return plot.
      return(plotlines(data, labels, xlimits, ylimits, colours, textsize, titletextoffset, removetitles, linewidth, xpand, ypand, xpandadd, ypandadd, xangle, xhjust, xvjust, borderthickness, xpandmultiplier, shade, linetype, background))
    }))
  } else if(tolower(mode) == "rectangle"){
    #Iterate over each entry.
    return(lapply(data, function(data){
      #Return plot.
      return(plotlinesrectangle(data, labels, xlimits, ylimits, shade = shade, background = background))
    }))
  } else if(tolower(mode) == "square"){
    #Iterate over each entry.
    return(lapply(data, function(data){
      #Return plot.
      return(plotlinessquare(data, labels, xlimits, ylimits, shade = shade, background = background))
    }))
  }
  
}

#Plot lines for a square panel - input data and labels. Other parameters are optional. Produces figure quality plots.
plotlinessquare <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 16, titletextoffset = 2, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0.01), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 0.75, xpandmultiplier = 0.02, shade = FALSE, linetype = "solid", background = "transparent"){
  plotlines(data, labels, xlimits, ylimits, colours, textsize, titletextoffset, removetitles, linewidth, xpand, ypand, xpandadd, ypandadd, xangle, xhjust, xvjust, borderthickness, xpandmultiplier, shade, linetype, background)
}

#Plot lines for a rectangle panel (two square panels) - input data and labels. Other parameters are optional. Produces figure quality plots.
plotlinesrectangle <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 16, titletextoffset = 2, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0.01), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 0.75, xpandmultiplier = 0.01, shade = FALSE, linetype = "solid", background = "transparent"){
  plotlines(data, labels, xlimits, ylimits, colours, textsize, titletextoffset, removetitles, linewidth, xpand, ypand, xpandadd, ypandadd, xangle, xhjust, xvjust, borderthickness, xpandmultiplier, shade, linetype, background)
}

#Plots point cloud for a square panel - just input points matrix or data frame with two columns for x and y.
plotpointcloudsquare <- function(points, labels = c("x", "y"), textsize = 16, titletextoffset = 0, removetitles = TRUE, pointsize = 1.25, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", xlimits = NULL, ylimits = NULL){
  plotpointcloud(points, labels, textsize, titletextoffset, removetitles, pointsize, borderthickness, colours, repeatcolours, background, xlimits, ylimits)
}

#Plots heat map for a square panel.
plotheatmapsquare <- function(data, labels = c("x", "y"), limits = NULL, textsize = 16, titletextoffset = 0, removetitles = FALSE, background = "transparent", includelegend = FALSE, breaks = NULL, xbreaks = NULL, ybreaks = NULL){
  plotheatmap(data, labels, limits, textsize, titletextoffset, removetitles, background, includelegend, breaks, xbreaks, ybreaks)
}

#Plots heat map for a rectangle panel.
plotheatmaprectangle <- function(data, labels = c("x", "y"), limits = NULL, textsize = 16, titletextoffset = 0, removetitles = FALSE, background = "transparent", includelegend = FALSE, breaks = NULL, xbreaks = NULL, ybreaks = NULL){
  plotheatmap(data, labels, limits, textsize, titletextoffset, removetitles, background, includelegend, breaks, xbreaks, ybreaks)
}

#Plots heat map legend for a square panel.
plotheatmaplegend <- function(data, labels = c("x", "y"), limits = NULL, textsize = 16, titletextoffset = 0, removetitles = FALSE, background = "transparent", includelegend = TRUE, breaks = NULL, xbreaks = NULL, ybreaks = NULL){
  for(j in 1:ncol(data)){
    data <- data[!is.na(data[,j]),]
  }
  p <- plotheatmap(data, labels, limits, textsize, titletextoffset, removetitles, background, includelegend, breaks, xbreaks, ybreaks)
  legend <- ggpubr::get_legend(p)
  ggpubr::as_ggplot(legend)
}

#Plots 2D histogram for a square panel.
plot2Dhistogramsquare <- function(points, labels = c("x", "y"), textsize = 16, titletextoffset = 0, removetitles = FALSE, background = "transparent", includelegend = FALSE, xbreaks = NULL, ybreaks = NULL, numberofbins = 20, binspacing = 2, xlims = NULL, ylims = NULL){
  plot2Dhistogram(points, labels, textsize, titletextoffset, removetitles, background, includelegend, xbreaks, ybreaks, numberofbins, binspacing, xlims, ylims)
}

#Plots 2D histogram for a rectangle panel.
plot2Dhistogramrectangle <- function(points, labels = c("x", "y"), textsize = 16, titletextoffset = 0, removetitles = FALSE, background = "transparent", includelegend = FALSE, xbreaks = NULL, ybreaks = NULL, numberofbins = 20, binspacing = 2, xlims = NULL, ylims = NULL){
  plot2Dhistogram(points, labels, textsize, titletextoffset, removetitles, background, includelegend, xbreaks, ybreaks, numberofbins, binspacing, xlims, ylims)
}

#Plots 2D histogram legend for a square panel.
plot2Dhistogramlegend <- function(points, labels = c("x", "y"), textsize = 16, titletextoffset = 0, removetitles = FALSE, background = "transparent", includelegend = TRUE, xbreaks = NULL, ybreaks = NULL, numberofbins = 20, binspacing = 2, xlims = NULL, ylims = NULL){
  for(j in 1:ncol(points)){
    points <- points[!is.na(points[,j]),]
  }
  p <- plot2Dhistogram(points, labels, textsize, titletextoffset, removetitles, background, includelegend, xbreaks, ybreaks, numberofbins, binspacing, xlims, ylims)
  legend <- ggpubr::get_legend(p)
  ggpubr::as_ggplot(legend)
}

#Save plot.
saveplot <- function(where, type = "svg", width = 17.342, height = 17.342){
  ggplot2::ggsave(conc(where, ".", type), device = type, width = width, height = height, units = "cm")
}

#Save plot square - saves the plot in the right size for a square panel.
saveplotsquare <- function(where, type = "svg"){
  ggplot2::ggsave(conc(where, ".", type), device = type, width = 8.54, height = 8.54, units = "cm")
}

#Save plot rectangle - saves the plot in the right size for a rectangle panel (two square panels).
saveplotrectangle <- function(where, type = "svg"){
  ggplot2::ggsave(conc(where, ".", type), device = type, width = 17.08, height = 8.54, units = "cm")
}

#Save heat map rectangle - saves a heatmap as a rectangle panel leaving space for the legend.
saveheatmaprectangle <- function(where, type = "svg"){
  savetwoplots(where, type, width = 14.08, height = 8.54, mode = "default")
}

#Combine parameters - combines a list of vectors representing parameters values into one data frame so that each combination of parameters can be derived from one for loop.
combineparameters <- function(parameterlist, names = NULL){
  #Iterate over each vector in the list and get lengths.
  lengths <- c()
  for(i in 1:length(parameterlist)){
    lengths <- append(lengths, length(parameterlist[[i]]))
  }
  rows <- prod(lengths)
  #Define matrix starting with final column.
  parametermatrix <- matrix(parameterlist[[length(parameterlist)]], ncol = 1, nrow = rows)
  #For remaining vectors, attach.
  indices <- 1:length(parameterlist)
  for(i in (length(parameterlist) - 1):1){
    #Create column vector.
    counts <- prod(lengths[indices > i])
    column <- c()
    for(j in parameterlist[[i]]){
      column <- append(column, rep(j, counts))
    }
    #Add to matrix.
    parametermatrix <- cbind(column, parametermatrix)
  }
  if(!is.null(names)){
    colnames(parametermatrix) <- names
  }
  return(parametermatrix)
}

#Create parameters - creates a data frame of all combinations of parameter values given a minimum, maximum and number of values.
createparameters <- function(mins, maxs, number, names = NULL){
  #Check number of increments.
  if(length(number) == 1){
    number <- rep(number, length(mins))
  }
  
  #Initialise parameter list.
  parameterlist <- vector("list", length(mins))
  
  #Iterate over each parameter.
  for(i in 1:length(parameterlist)){
    #Create parameter vector.
    parameterlist[[i]] <- mins[i] + (maxs[i] - mins[i]) * (1:number[i] - 1) / (number[i] - 1)
  }
  
  #Combine and return.
  return(combineparameters(parameterlist, names))
}

#ToMATo clustering.
tomato <- function(points, search_radius, persistence){
  #Define useful properties.
  number_of_points <- nrow(points)
  distance_matrix <- as.matrix(Rfast::Dist(points[,1:2]))
  distance_radius <- distance_matrix <= search_radius
  #Get densities.
  densities <- as.numeric(colSums(distance_radius))
  #Get filtration.
  filtration <- 1:number_of_points
  point_ids <- Rfast::sort_cor_vectors(filtration, densities, descending = TRUE)
  filtration_ids <- Rfast::sort_cor_vectors(filtration, point_ids)
  #Perform persistent homology.
  r <- rep(0, number_of_points)
  for(i in filtration){
    #Get original point id.
    point_id <- point_ids[i]
    #Extract filtrations of neighbours of i.
    neighbour_filtrations <- filtration_ids[distance_radius[, point_id]]
    #Extract neighbours with lower filtration values than i.
    N <- neighbour_filtrations[neighbour_filtrations < i]
    #Check length of N.
    if(length(N) == 0){
      #If N is empty, then i is a peak. Create a new root. Update r and e.
      r[point_id] <- point_id
    }else{
      #If N is non-empty, then there must be at least one neighbour of higher density. #Cycle through neighbours and check persistence holds.
      potentials <- N[densities[point_ids[N]] <= densities[point_id] + persistence]
      #Check length of potentials.
      if(length(potentials) == 0){
        #Node cannot connect to any neighbours (persistence too low). Create new set for node.
        r[point_id] <- point_id
      } else {
        #Node can connect to at least one neighbour and bridges gap between other neighbours.
        potentials <- point_ids[Rfast::Sort(potentials)]
        j <- potentials[1]
        r[point_id] <- j
        if(length(potentials) > 1){
          #Merge the sets of the potentials.
          for(k in potentials[-1]){
            r_k <- r == k
            r[r_k] <- j
          }
        }
      }
    }
  }
  #Determine cluster indices.
  found <- 0
  #Filter out clusters with too few points.
  cluster_index <- rep(0, number_of_points)
  for(i in unique(r)){
    #Get points corresponding to that root.
    cluster_points <- filtration[r == i]
    #Filter out clusters below persistence and singlets.
    if(densities[i] <= persistence || length(cluster_points) == 1){
      #Set to outliers.
      cluster_index[cluster_points] <- 0
    } else {
      #Initialise new cluster.
      found <- found + 1
      cluster_index[cluster_points] <- found
    }
  }
  #Return new point cloud with cluster index.
  return(cbind(points, cluster_index))
}

#Write CSV but with row.names always false because that is literally never useful.
writecsv <- function(what, where){
  write.csv(what, where, row.names = FALSE)
}

#Animate line - currently only works for one line because ggplot refuses to be intuitive.
animateline <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, time_unit = "s"){
  data <- as.data.frame(data)
  points <- as.data.frame(do.call(rbind, lapply(2:ncol(data), function(j){
    new <- do.call(rbind, lapply(1:nrow(data), function(i){
      return(cbind(data[1:i,c(1,j)], as.character(j), data[i,1]))
    }))
    colnames(new) <- c("x", "y_value", "line", "t")
    return(new)
  })))
  colnames(points) <- c("x", "y_value", "line", "t")
  # return(points)
  # points <- points[order(points$t),]
  #Create plot.
  p <- 
    ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line, group = t)) +
    ggplot2::geom_line(linewidth = linewidth) +
    ggplot2::labs(x = labels[1], y = labels[2]) +
    ggplot2::theme(panel.background = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                   axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                   axis.line = ggplot2::element_line(colour = "black"), legend.position = "none")
  # return(p)
  p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time}", time_unit, sep = ""))
}

#Create circular cluster simulation.
simulatecircularclusters <- function(ROI = 1000, number_of_clusters = 5, cluster_radius = 50, points_per_cluster = 30, background_to_cluster_ratio = 0.1, output_clusters = FALSE){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  
  #Define cluster centres.
  centres <- cbind(runif(number_of_clusters, cluster_radius, ROI[1] - cluster_radius), runif(number_of_clusters, cluster_radius, ROI[2] - cluster_radius))
  
  #Create circular clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    r <- cluster_radius * sqrt(runif(points_per_cluster, 0, 1))
    theta <- runif(points_per_cluster, 0, 2 * pi)
    if(output_clusters){
      return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2], i))
    } else {
      return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2]))
    }
  }))
  
  #Overlay outliers.
  if(background_to_cluster_ratio > 0){
    outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
    if(output_clusters){
      points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2]), 0))
    } else {
      points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
    }
  }
  return(points)
}

#Create CSR points.
simulatecsr <- function(ROI = 1000, number_of_points = 100){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  return(cbind(runif(number_of_points, 0, max = ROI[1]), runif(number_of_points, 0, max = ROI[2])))
}

#Create Gaussian clusters simulation.
simulategaussianclusters <- function(ROI = 1000, number_of_clusters = 5, cluster_sd = 50, points_per_cluster = 30, background_to_cluster_ratio = 0.1, output_clusters = FALSE){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  #Calculate outliers.
  outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
  
  #Define cluster centres.
  centres <- cbind(runif(number_of_clusters, cluster_sd, ROI[1] - cluster_sd), runif(number_of_clusters, cluster_sd, ROI[2] - cluster_sd))
  
  #Create Gaussian clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    if(output_clusters){
      return(cbind(rnorm(points_per_cluster, centres[i,1], cluster_sd), rnorm(points_per_cluster, centres[i,2], cluster_sd), i))
    } else {
      return(cbind(rnorm(points_per_cluster, centres[i,1], cluster_sd), rnorm(points_per_cluster, centres[i,2], cluster_sd)))
    }
  }))
  
  #Overlay outliers.
  if(output_clusters){
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2]), 0))
  } else {
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
  }
  return(points)
}

#Create uniform fibrous clusters simulation.
simulateuniformfibres <- function(ROI = 1000, number_of_clusters = 5, fibre_length = 50, fibre_width = 7, points_per_cluster = 30, background_to_cluster_ratio = 0.1, output_clusters = FALSE){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  #Calculate outliers.
  outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
  
  #Define cluster centres and angles.
  starts <- cbind(runif(number_of_clusters, fibre_length, ROI[1] - fibre_length), runif(number_of_clusters, fibre_length, ROI[2] - fibre_length))
  angles <- runif(number_of_clusters, 0, 2 * pi)
  ends <- starts + cbind(fibre_length * cos(angles), fibre_length * sin(angles))
  centres <- cbind(runif(number_of_clusters, fibre_length / 2, ROI[1] - (fibre_length / 2)), runif(number_of_clusters, fibre_length / 2, ROI[2] - (fibre_length / 2)))
  
  #Create fibrous clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    #Simulate rectangular cluster.
    pts <- cbind(runif(points_per_cluster, 0, fibre_width), runif(points_per_cluster, 0, fibre_length))
    #Apply rotation matrix
    pts <- pts %*% matrix(c(cos(angles[i]), -sin(angles[i]), sin(angles[i]), cos(angles[i])), nrow = 2, byrow = TRUE)
    #Move to start point.
    pts[,1] <- pts[,1] + starts[i,1]
    pts[,2] <- pts[,2] + starts[i,2]
    #Return clusters
    if(output_clusters){
      return(cbind(pts, i))
    } else {
      return(pts)
    }
  }))
  
  #Overlay outliers.
  if(output_clusters){
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2]), 0))
  } else {
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
  }
  return(points)
}

#Create Gaussian fibrous clusters simulation.
simulategaussianfibres <- function(ROI = 1000, number_of_clusters = 5, fibre_length = 50, fibre_width = 7, points_per_cluster = 30, background_to_cluster_ratio = 0.1, output_clusters = FALSE){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  #Calculate outliers.
  outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
  
  #Define cluster centres and angles.
  starts <- cbind(runif(number_of_clusters, fibre_length, ROI[1] - fibre_length), runif(number_of_clusters, fibre_length, ROI[2] - fibre_length))
  angles <- runif(number_of_clusters, 0, 2 * pi)
  ends <- starts + cbind(fibre_length * cos(angles), fibre_length * sin(angles))
  centres <- cbind(runif(number_of_clusters, fibre_length / 2, ROI[1] - (fibre_length / 2)), runif(number_of_clusters, fibre_length / 2, ROI[2] - (fibre_length / 2)))
  
  #Create fibrous clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    #Simulate rectangular cluster.
    pts <- cbind(rnorm(points_per_cluster, fibre_width / 2, fibre_width), runif(points_per_cluster, 0, fibre_length))
    #Apply rotation matrix
    pts <- pts %*% matrix(c(cos(angles[i]), -sin(angles[i]), sin(angles[i]), cos(angles[i])), nrow = 2, byrow = TRUE)
    #Move to start point.
    pts[,1] <- pts[,1] + starts[i,1]
    pts[,2] <- pts[,2] + starts[i,2]
    #Return clusters
    if(output_clusters){
      return(cbind(pts, i))
    } else {
      return(pts)
    }
  }))
  
  #Overlay outliers.
  if(output_clusters){
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2]), 0))
  } else {
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
  }
  return(points)
}

#Create uniform annular clusters simulation.
simulateuniformannularclusters <- function(ROI = 1000, number_of_clusters = 5, cluster_radius = 50, inner_radius = 25, points_per_cluster = 30, background_to_cluster_ratio = 0.1, output_clusters = FALSE){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  #Calculate outliers.
  outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
  
  #Define cluster centres.
  centres <- cbind(runif(number_of_clusters, cluster_radius, ROI[1] - cluster_radius), runif(number_of_clusters, cluster_radius, ROI[2] - cluster_radius))
  
  #Create circular clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    r <- cluster_radius * sqrt(runif(points_per_cluster, inner_radius / cluster_radius, 1))
    theta <- runif(points_per_cluster, 0, 2 * pi)
    if(output_clusters){
      return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2], i))
    } else {
      return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2]))
    }
  }))
  
  #Overlay outliers.
  if(output_clusters){
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2]), 0))
  } else {
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
  }
  return(points)
}

#Create Gaussian annular clusters simulation.
simulategaussianannularclusters <- function(ROI = 1000, number_of_clusters = 5, cluster_radius = 50, inner_radius = 25, points_per_cluster = 30, background_to_cluster_ratio = 0.1, output_clusters = FALSE){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  #Calculate outliers.
  outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
  
  #Define cluster centres.
  centres <- cbind(runif(number_of_clusters, cluster_radius, ROI[1] - cluster_radius), runif(number_of_clusters, cluster_radius, ROI[2] - cluster_radius))
  
  #Create circular clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    r <- rnorm(points_per_cluster, (inner_radius + cluster_radius) / 2, cluster_radius - inner_radius)
    theta <- runif(points_per_cluster, 0, 2 * pi)
    if(output_clusters){
      return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2], i))
    } else {
      return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2]))
    }
  }))
  
  #Overlay outliers.
  if(output_clusters){
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2]), 0))
  } else {
    points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
  }
  return(points)
}

#Per Parameter Analysis Single Value - averages results per parameter, ignoring NAs. Works for results with one dependent variable.
perparameteranalysissinglevalue <- function(parameters, results, parametercolumntotake = 1, resultscolumntotake = 1){
  #Iterate over each parameter.
  #Extract unique values.
  uniques <- sort(unique(parameters[,parametercolumntotake]))
  #Iterate over each unique value.
  outputs <- do.call(c, lapply(uniques, function(value){
    #Get average result corresponding to that value.
    return(mean(results[which(parameters[,parametercolumntotake] == value), resultscolumntotake], na.rm = TRUE))
  }))
  #Return outputs.
  return(outputs)
}

#Per Parameter Analysis - averages results per parameter, ignoring NAs.
perparameteranalysis <- function(parameters, location, name = "Simulation", parametercolumnstoignore = NULL, simulationcolumntotake = 1){
  #Read in data.
  datasets <- lapply(1:nrow(parameters), function(i){
    return(read.csv(paste(main, name, " ", i, ".csv", sep = "")))
  })
  
  #Remove columns to ignore.
  if(!is.null(parametercolumnstoignore)){
    parameters <- parameters[, -parametercolumnstoignore]
  }
  
  #Get parameter names.
  names <- colnames(parameters)
  
  #Iterate over each parameter.
  lapply(names, function(name){
    #Extract unique values.
    uniques <- sort(unique(parameters$name))
    #Create vector for storing results.
    simulations <- vector("list", nrow(simulations))
    #Iterate over each unique value.
    for(value in uniques){
      #Get simulations which correspond to that value.
      numbers <- which(parameters$name == unique)
      
    }
  })
}

#Save two plots - saves two plots, one a png for easy viewing, and the other whichever type you specify.
savetwoplots <- function(where, type = "png", width = 17.342, height = 17.342, mode = "default"){
  if(!is.null(where)){
    if(tolower(mode) == "default"){
      ggplot2::ggsave(paste(where, ".", type, sep = ""), device = type, width = width, height = height, units = "cm")
      ggplot2::ggsave(paste(where, ".png", sep = ""), device = "png", width = width, height = height, units = "cm")
    } else if(tolower(mode) == "rectangle"){
      ggplot2::ggsave(paste(where, ".", type, sep = ""), device = type, width = 17.08, height = 8.54, units = "cm")
      ggplot2::ggsave(paste(where, ".png", sep = ""), device = "png", width = 17.08, height = 8.54, units = "cm")
    } else if(tolower(mode) == "square"){
      ggplot2::ggsave(paste(where, ".", type, sep = ""), device = type, width = 8.54, height = 8.54, units = "cm")
      ggplot2::ggsave(paste(where, ".png", sep = ""), device = "png", width = 8.54, height = 8.54, units = "cm")
    }
  }
}

#K to H - converts Ripley's K function to a H function.
KtoH <- function(K){
  K[,2] <- sqrt(K[,2] / pi) - K[,1]
  colnames(K) <- c("r", "H")
  return(K)
}

#H to K - converts Ripley's H function to a K function.
HtoK <- function(H){
  H[,2] <- pi * (H[,2] + H[,1]) ** 2
  colnames(H) <- c("r", "K")
  return(H)
}

#Replace entries - replaces all entries A with B in column C. All can be vectors.
replaceentries <- function(data, A, B, C){
  for(i in 1:length(A)){
    a <- A[i]
    b <- B[i]
    c <- C[i]
    data[data[,c] == a, c] <- b
  }
  return(data)
}

#Concatenate - like paste but with no separator as that is also literally never useful.
conc <- function(..., sep = ""){
  return(paste(..., sep = sep))
}