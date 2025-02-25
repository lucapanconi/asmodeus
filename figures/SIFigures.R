#Creates all panels for SI Figures.
#Data input directory goes here.
input_dir <- "data/SIFigures/"

#Plot output directory goes here.
output_dir <- "figures/SIFigurePanels/"

#Get plotting functions.
source(paste(getwd(), "figures/PlotFunctions.R", sep = "/"))

#Update input and output directories.
input_dir <- paste(getwd(), input_dir, sep = "/")
output_dir <- paste(getwd(), output_dir, sep = "/")

#SI Figure 1 - cluster radius
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Cluster Radius vs Probability of Activation", files)]
files <- c(files[7], files[17:18], files[1:6], files[8:16])
#Set panel names 1a - 1r.
panel_names <- c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "1h", "1i", "1j", "1k", "1l", "1m", "1n", "1o", "1p", "1q", "1r")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("cluster radius (nm)", "prob. of activation"), ylimits = c(0, 1.05))
  savetwoplots(conc(output_dir, panel_name))
}

#SI Figure 2 - number of clusters
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Number of Clusters vs Probability of Activation", files)]
files <- c(files[7], files[17:18], files[1:6], files[8:16])
#Set panel names 2a - 2r.
panel_names <- c("2a", "2b", "2c", "2d", "2e", "2f", "2g", "2h", "2i", "2j", "2k", "2l", "2m", "2n", "2o", "2p", "2q", "2r")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("number of clusters", "prob. of activation"), ylimits = c(0, 1.05))
  savetwoplots(conc(output_dir, panel_name))
}

#SI Figure 3 - points per cluster
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Points per Cluster vs Probability of Activation", files)]
files <- c(files[7], files[17:18], files[1:6], files[8:16])
#Set panel names 3a - 3r.
panel_names <- c("3a", "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i", "3j", "3k", "3l", "3m", "3n", "3o", "3p", "3q", "3r")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("%points per cluster", "prob. of activation"), ylimits = c(0, 1.05))
  savetwoplots(conc(output_dir, panel_name))
}

#SI Figure 4 - points per cluster vs average step size
#Get list of all files from input directory.
files <- list.files(input_dir, full.names = TRUE)
#Get names which contain "Cluster Radius vs Probability of Activation".
files <- files[grep("Points per Cluster vs Step Size", files)]
files <- c(files[7], files[17:18], files[1:6], files[8:16])
#Set panel names 4a - 4r.
panel_names <- c("4a", "4b", "4c", "4d", "4e", "4f", "4g", "4h", "4i", "4j", "4k", "4l", "4m", "4n", "4o", "4p", "4q", "4r")
for(i in 1:length(files)) {
  #Get data from file.
  data <- read.csv(files[i])
  #Get panel name.
  panel_name <- panel_names[i]
  #Create plot.
  plotlinesrectangle(data, labels = c("%points per cluster", "avg. step size"), ylimits = c(0, 65))
  savetwoplots(conc(output_dir, panel_name))
}