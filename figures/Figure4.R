#Creates all panels for Figure 4.
#Data input directory goes here.
input_dir <- "data/Figure4/"

#Plot output directory goes here.
output_dir <- "figures/Figure4Panels/"

#Get plotting functions.
source(paste(getwd(), "figures/PlotFunctions.R", sep = "/"))

#Update input and output directories.
input_dir <- paste(getwd(), input_dir, sep = "/")
output_dir <- paste(getwd(), output_dir, sep = "/")

#Perform ANOVA and TukeyHSD on configurations inflection points and scales.
#Read in data.
inflections <- read.csv(conc(input_dir, "All Configurations vs Point of Inflection.csv"))
inflections[,1] <- letters[1:15]
colnames(inflections) <- c("configuration", conc("trial_", 1:10))
inflections <- inflections[,1:10]
writecsv(inflections, conc(output_dir, "Inflections.csv"))
scales <- read.csv(conc(input_dir, "All Configurations vs Scale.csv"))
scales[,1] <- letters[1:15]
colnames(scales) <- c("configuration", conc("trial_", 1:10))
scales <- scales[,1:10]
writecsv(scales, conc(output_dir, "Scales.csv"))

#Sort data.
infdata <- do.call(rbind, lapply(1:nrow(inflections), function(i){
  return(cbind(as.data.frame(inflections[i,1]), as.data.frame(as.numeric(inflections[i, 2:ncol(inflections)]))))
}))
scadata <- do.call(rbind, lapply(1:nrow(scales), function(i){
  return(cbind(as.data.frame(scales[i,1]), as.data.frame(as.numeric(scales[i, 2:ncol(scales)]))))
}))
colnames(infdata) <- c("configuration", "value")
colnames(scadata) <- c("configuration", "value")

#Define box plot function.
#Plot box plot - input data where each column is a separate variable.
plotboxplot <- function(points, labels = c("x", "y"), w = 1, axes = c("type", "value"), textsize = 16, titletextoffset = 0, removetitles = FALSE, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", linethickness = 1, sortdata = FALSE){
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
  
  #points <- replaceentries(points, A = uniques, B = labels, C = rep(1, length(uniques)))
  colours <- c(rep(ibmcols, length.out = 15))
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
        ggplot2::geom_boxplot(lwd = linethickness, width = w) +
        ggplot2::labs(x = axes[1], y = axes[2]) +
        ggplot2::scale_fill_manual(values = ggplot2::alpha(colours, 0.5)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, face = "bold"),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#Create box plots.
plotboxplot(infdata, axes = c("configuration", "point of inflection"), w = 0.9, textsize = 32, linethickness = 0.5)
savetwoplots(conc(output_dir, "Configuration vs Point of Inflection"), height = 20, width = 30)
plotboxplot(scadata, axes = c("configuration", "scale"), w = 0.9, textsize = 32, linethickness = 0.5)
savetwoplots(conc(output_dir, "Configuration vs Scale"), height = 20, width = 30)

#Perform ANOVA and generate heat maps - note that this shows Configuration 1 (row) - Configuration 2 (column).
#Inflections.
#Perform ANOVA.
anova_model <- aov(value ~ configuration, data = infdata)
#Summary of ANOVA.
summary(anova_model)
#Perform Tukey's HSD test.
tukey_test <- TukeyHSD(anova_model)
print(tukey_test)
writecsv(tukey_test[[1]], conc(output_dir, "TukeyHSD Results Inflection.csv"))
#Extract the matrix of pairwise differences.
tukey_matrix <- tukey_test$configuration[, c("diff")]
#Convert names of comparisons to separate group columns.
comparison_names <- strsplit(rownames(tukey_test$configuration), "-")
group1 <- sapply(comparison_names, "[[", 1)
group2 <- sapply(comparison_names, "[[", 2)
#Create a data frame with group comparisons and differences.
heatmap_data <- data.frame(group1 = group1, group2 = group2, diff = tukey_matrix)
#Convert the data into wide format.
heatmap_matrix <- cbind(rbind(NA, reshape2::dcast(heatmap_data, group1 ~ group2, value.var = "diff")), NA)
heatmap_matrix[1,1] <- "a"
colnames(heatmap_matrix)[ncol(heatmap_matrix)] <- "CSR CSR CSR"
names <- heatmap_matrix[,1]
names < gsub(" ", "-", names)
heatmap_matrix <- heatmap_matrix[,2:ncol(heatmap_matrix)]
#Fill in the lower triangle of the heatmap.
heatmap_matrix[upper.tri(heatmap_matrix)] <- -t(heatmap_matrix)[upper.tri(heatmap_matrix)]
heatmap_matrix[is.na(heatmap_matrix)] <- 0
colnames(heatmap_matrix) <- names
rownames(heatmap_matrix) <- names
plotheatmap(heatmap_matrix) #This shows row - column.
#Convert rownames to a separate column (optional for ggplot).
heatmap_matrix$group1 <- rownames(heatmap_matrix)
#Melt the data for ggplot.
heatmap_melt <- reshape2::melt(heatmap_matrix, id.vars = "group1", variable.name = "group2", value.name = "difference")
#Generate the heatmap using ggplot
ggplot(heatmap_melt, aes(x = group1, y = group2, fill = difference)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = ibmcols[3], high = ibmcols[2], mid = "white", 
                       midpoint = 0, limit = c(min(heatmap_melt$difference, na.rm=TRUE), 
                                               max(heatmap_melt$difference, na.rm=TRUE))) +
  labs(fill = "diff. infl.", x = "configuration 2", y = "configuration 1") +
  theme_minimal() +
  theme(text = ggplot2::element_text(family = "serif", size = 36), axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"), legend.key.width = unit(1.5, "cm"), legend.key.height = unit(2.5, "cm"),
        axis.title.x = element_blank(), axis.title.y = element_blank())
savetwoplots(conc(output_dir, "Inflection Heatmap"), width = 30)

#Perform ANOVA
anova_model <- aov(value ~ configuration, data = scadata)
#Summary of ANOVA
summary(anova_model)
#Perform Tukey's HSD test
tukey_test <- TukeyHSD(anova_model)
print(tukey_test)
writecsv(tukey_test[[1]], conc(output_dir, "TukeyHSD Results Scale.csv"))
#Extract the matrix of pairwise differences.
tukey_matrix <- tukey_test$configuration[, c("diff")]
#Convert names of comparisons to separate group columns.
comparison_names <- strsplit(rownames(tukey_test$configuration), "-")
group1 <- sapply(comparison_names, "[[", 1)
group2 <- sapply(comparison_names, "[[", 2)
#Create a data frame with group comparisons and differences.
heatmap_data <- data.frame(group1 = group1, group2 = group2, diff = tukey_matrix)
#Convert the data into wide format.
heatmap_matrix <- cbind(rbind(NA, reshape2::dcast(heatmap_data, group1 ~ group2, value.var = "diff")), NA)
heatmap_matrix[1,1] <- "a"
colnames(heatmap_matrix)[ncol(heatmap_matrix)] <- "CSR CSR CSR"
names <- heatmap_matrix[,1]
names < gsub(" ", "-", names)
heatmap_matrix <- heatmap_matrix[,2:ncol(heatmap_matrix)]
#Fill in the lower triangle of the heatmap.
heatmap_matrix[upper.tri(heatmap_matrix)] <- -t(heatmap_matrix)[upper.tri(heatmap_matrix)]
heatmap_matrix[is.na(heatmap_matrix)] <- 0
colnames(heatmap_matrix) <- names
rownames(heatmap_matrix) <- names
plotheatmap(heatmap_matrix) #This shows row - column.
#Convert rownames to a separate column (optional for ggplot).
heatmap_matrix$group1 <- rownames(heatmap_matrix)
#Melt the data for ggplot.
heatmap_melt <- reshape2::melt(heatmap_matrix, id.vars = "group1", variable.name = "group2", value.name = "difference")
#Generate the heatmap using ggplot
ggplot(heatmap_melt, aes(x = group1, y = group2, fill = difference)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = ibmcols[3], high = ibmcols[2], mid = "white", 
                       midpoint = 0, limit = c(min(heatmap_melt$difference, na.rm=TRUE), 
                                               max(heatmap_melt$difference, na.rm=TRUE))) +
  labs(fill = "diff. scale", x = "Configuration 2", y = "Configuration 1") +
  theme_minimal() +
  theme(text = ggplot2::element_text(family = "serif", size = 36), axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"), legend.key.width = unit(1.5, "cm"), legend.key.height = unit(2.5, "cm"),
        axis.title.x = element_blank(), axis.title.y = element_blank())
savetwoplots(conc(output_dir, "Scale Heatmap"), width = 30)