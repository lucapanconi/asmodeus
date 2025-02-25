#Creates all panels for Figure 5.
#Data input directory goes here.
input_dir <- "data/Figure5/"

#Plot output directory goes here.
output_dir <- "figures/Figure5Panels/"

#Get plotting functions.
source(paste(getwd(), "figures/PlotFunctions.R", sep = "/"))

#Update input and output directories.
input_dir <- paste(getwd(), input_dir, sep = "/")
output_dir <- paste(getwd(), output_dir, sep = "/")

#Panel a - cluster radius
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Cluster Radius vs Probability of Activation", files)]
files <- c(files[length(files)], files[1:(length(files) - 1)])
#Set panel names.
panel_names <- c("ai", "aii", "aiii", "aiv", "av")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("cluster radius (nm)", "prob. of activation"), ylimits = c(0, 1.05))
  savetwoplots(conc(output_dir, panel_name))
}

#Panel b - number of clusters
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Number of Clusters vs Probability of Activation", files)]
files <- c(files[length(files)], files[1:(length(files) - 1)])
#Set panel names.
panel_names <- c("bi", "bii", "biii", "biv", "bv")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("number of clusters", "prob. of activation"), ylimits = c(0, 1.05))
  savetwoplots(conc(output_dir, panel_name))
}

#Panel c - points per cluster
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Points per Cluster vs Probability of Activation", files)]
files <- c(files[length(files)], files[1:(length(files) - 1)])
#Set panel names.
panel_names <- c("ci", "cii", "ciii", "civ", "cv")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("%points per cluster", "prob. of activation"), ylimits = c(0, 1.05))
  savetwoplots(conc(output_dir, panel_name))
}