#Creates all panels for Figure 2.
#Data input directory goes here.
input_dir <- "data/Figure2/"

#Plot output directory goes here.
output_dir <- "figures/Figure2Panels/"

#Get plotting functions.
source(paste(getwd(), "figures/PlotFunctions.R", sep = "/"))

#Update input and output directories.
input_dir <- paste(getwd(), input_dir, sep = "/")
output_dir <- paste(getwd(), output_dir, sep = "/")

#Panel a
#Get example simulation.
sim <- readRDS(conc(input_dir, "ExampleSimulation.RData"))
Hs <- readRDS(conc(input_dir, "ExampleKFunctions.RData"))
tar <- read.csv(conc(input_dir, "ExampleTarget.csv"))

#Fix targets
number_points <- nrow(sim[[1]])
tar <- HtoK(tar)
tar[,2] <- (tar[,2] - number_points) / number_points

#i - simulated point cloud examples
pts <- sim[[length(sim)]]
pts <- tomato(pts, search_radius = 105, persistence = 9)
plotpointcloudsquare(pts, pointsize = 2)
savetwoplots(paste(output_dir, "ai"), type = "png", mode = "square")

#ii - corresponding Ripley's functions vs target
K <- HtoK(Hs[[length(Hs)]])
K[,2] <- (K[,2] - number_points) / number_points
K <- cbind(K, tar[,2])
K <- rbind(0, K)
plotlinessquare(K, labels = c("r", "K(r)"), shade = TRUE)
savetwoplots(paste(output_dir, "aii"), type = "png", mode = "square")

#iii - cluster radius
#Get differences for simulated data.
differences <- read.csv(conc(input_dir, "Difference from Cluster Properties.csv"))
differences <- cbind(1:nrow(differences), differences)
plotlinesrectangle(differences[,c(1,3)], ylimits = c(-10, 40), labels = c("t(s)", expression(paste(r[dif], " (nm)"))))
savetwoplots(paste(output_dir, "aiii"), type = "png", mode = "rectangle")

#iv - number of clusters
plotlinesrectangle(differences[,c(1,2)], ylimits = c(-2, 2), labels = c("t(s)", expression(n[dif])))
savetwoplots(paste(output_dir, "aiv"), type = "png", mode = "rectangle")

#v - points per cluster
plotlinesrectangle(differences[,c(1,4)], ylimits = c(-20, 20), labels = c("t(s)", expression(p[dif])))
savetwoplots(paste(output_dir, "av"), type = "png", mode = "rectangle")