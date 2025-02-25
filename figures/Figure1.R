#Creates all panels for Figure 1.
#Data input directory goes here.
input_dir <- "data/Figure1/"

#Plot output directory goes here.
output_dir <- "figures/Figure1Panels/"

#Get plotting functions.
source(paste(getwd(), "figures/PlotFunctions.R", sep = "/"))

#Update input and output directories.
input_dir <- paste(getwd(), input_dir, sep = "/")
output_dir <- paste(getwd(), output_dir, sep = "/")

#Panel b - Change in error over time.
#Get example simulation.
sim <- readRDS(conc(input_dir, "ExampleSimulation.RData"))
err <- read.csv(conc(input_dir, "ExampleError.csv"))
Hs <- readRDS(conc(input_dir, "ExampleKFunctions.RData"))
tar <- read.csv(conc(input_dir, "ExampleTarget.csv"))

#Fix target.
number_points <- nrow(sim[[1]])
tar <- HtoK(tar)
tar[,2] <- (tar[,2] - number_points) / number_points

# i - Plots of simulation with clustering.
# Start
pts <- sim[[1]]
plotpointcloudsquare(pts, pointsize = 4)
saveplotsquare(conc(output_dir, "bi(i)"), type = "png")

# Mid 1
pts <- sim[[1000]]
plotpointcloudsquare(pts, pointsize = 4)
saveplotsquare(conc(output_dir, "bi(ii)"), type = "png")

# Mid 2
pts <- sim[[5000]]
plotpointcloudsquare(pts, pointsize = 4)
saveplotsquare(conc(output_dir, "bi(iii)"), type = "png")

# End
pts <- sim[[length(sim)]]
pts <- tomato(pts, search_radius = 100, persistence = 9)
plotpointcloudsquare(pts, pointsize = 4)
saveplotsquare(conc(output_dir, "bi(iv)"), type = "png")

# ii - Ripley's function versus target across simulation.
# Start
K1 <- HtoK(Hs[[1]])
K1[,2] <- (K1[,2] - number_points) / number_points
K1 <- cbind(K1, tar[,2])
K1 <- rbind(0, K1)
K1 <- K1[-(17:21),]

# Mid 1
K2 <- HtoK(Hs[[1000]])
K2[,2] <- (K2[,2] - number_points) / number_points
K2 <- cbind(K2, tar[,2])
K2 <- rbind(0, K2)
K2 <- K2[-(17:21),]

# Mid 2
K3 <- HtoK(Hs[[5000]])
K3[,2] <- (K3[,2] - number_points) / number_points
K3 <- cbind(K3, tar[,2])
K3 <- rbind(0, K3)
K3 <- K3[-(17:21),]

# End
K4 <- HtoK(Hs[[length(Hs)]])
K4[,2] <- (K4[,2] - number_points) / number_points
K4 <- cbind(K4, tar[,2])
K4 <- rbind(0, K4)
K4 <- K4[-(17:21),]

#Create consistent plots.
plots <- plotlinesconsistent(list(K1, K2, K3, K4), labels = c("r", "K(r)"), shade = TRUE, mode = "square")

#Save plots above.
print(plots[[1]])
saveplotsquare(conc(output_dir, "bii(i)"), type = "png")
print(plots[[2]])
saveplotsquare(conc(output_dir, "bii(ii)"), type = "png")
print(plots[[3]])
saveplotsquare(conc(output_dir, "bii(iii)"), type = "png")
print(plots[[4]])
saveplotsquare(conc(output_dir, "bii(iv)"), type = "png")

# iii - Error versus step size.
D_min <- 10
D_max <- 50
E <- 0:100 / 100
D <- (D_max - D_min) * (E ** 2) + D_min
plotlinesrectangle(cbind(E,D), labels = c("error", "step size"), ylimits = c(0, D_max + 10), xlimits = c(0, 1.05))
saveplotrectangle(conc(output_dir, "biii"))

# iv - Change in error over time, marked with line of convergence.
err[,1] <- err[,1] / 100
plotlinesrectangle(err, labels = c("t (s)", "e"), ylimits = c(0.1, 0.8), xpand = c(0, 0.02))
saveplotrectangle(conc(output_dir, "biv"), type = "png")