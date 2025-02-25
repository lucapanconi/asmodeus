#Creates all panels for Figure 3.
#Data input directory goes here.
input_dir <- "data/Figure3/"

#Plot output directory goes here.
output_dir <- "figures/Figure3Panels/"

#Get plotting functions.
source(paste(getwd(), "figures/PlotFunctions.R", sep = "/"))

#Update input and output directories.
input_dir <- paste(getwd(), input_dir, sep = "/")
output_dir <- paste(getwd(), output_dir, sep = "/")


#Panels a - o: varied configurations.
#a - all populations at CSR.
set.seed(1)
ac <- simulatecsr()
ag <- simulatecsr()
inh <- simulatecsr()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "a"), mode = "square")

#b - clustered activators, all others CSR.
set.seed(2)
ac <- simulatecircularclusters()
ag <- simulatecsr()
inh <- simulatecsr()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "b"), mode = "square")

#c - clustered agents, all others CSR.
set.seed(3)
ac <- simulatecsr()
ag <- simulatecircularclusters()
inh <- simulatecsr()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "c"), mode = "square")

#d - clustered inhibitors, all others CSR.
set.seed(4)
ac <- simulatecsr()
ag <- simulatecsr()
inh <- simulatecircularclusters()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "d"), mode = "square")

#e - CSR inhibitors, all others clustered.
set.seed(5)
ac <- simulatecircularclusters()
ag <- simulatecircularclusters()
inh <- simulatecsr()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "e"), mode = "square")

#f - CSR agents, all others clustered.
set.seed(6)
ac <- simulatecircularclusters()
ag <- simulatecsr()
inh <- simulatecircularclusters()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "f"), mode = "square")

#g - CSR activators, all others clustered.
set.seed(8)
ac <- simulatecsr()
ag <- simulatecircularclusters()
inh <- simulatecircularclusters()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "g"), mode = "square")

#h - all clustered.
set.seed(9)
ac <- simulatecircularclusters()
ag <- simulatecircularclusters()
inh <- simulatecircularclusters()
pts <- rbind(cbind(ac, 1), cbind(ag, 2), cbind(inh, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "h"), mode = "square")

#i - co-clustered activators and agents, CSR inhibitors.
set.seed(13)
c1 <- simulatecircularclusters()
c2 <- simulatecsr()
pts <- rbind(cbind(c1, sample(c(1,2), size = nrow(c1), replace = TRUE)), cbind(c2, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "i"), mode = "square")

#j - co-clustered activators and inhibitors, CSR agents
set.seed(16)
c1 <- simulatecircularclusters()
c2 <- simulatecsr()
pts <- rbind(cbind(c1, sample(c(1,3), size = nrow(c1), replace = TRUE)), cbind(c2, 2))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "j"), mode = "square")

#k - co-clustered inhibitors and agents, CSR activators.
set.seed(19)
c1 <- simulatecircularclusters()
c2 <- simulatecsr()
pts <- rbind(cbind(c1, sample(c(2,3), size = nrow(c1), replace = TRUE)), cbind(c2, 1))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "k"), mode = "square")

#l - all clustered, activators and agents co-clustered.
set.seed(20)
c1 <- simulatecircularclusters()
c2 <- simulatecircularclusters()
pts <- rbind(cbind(c1, sample(c(1,2), size = nrow(c1), replace = TRUE)), cbind(c2, 3))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "l"), mode = "square")

#m - all clustered, activators and inhibitors co-clustered.
set.seed(21)
c1 <- simulatecircularclusters()
c2 <- simulatecircularclusters()
pts <- rbind(cbind(c1, sample(c(1,3), size = nrow(c1), replace = TRUE)), cbind(c2, 2))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "m"), mode = "square")

#n - all clustered, inhibitors and agents co-clustered.
set.seed(22)
c1 <- simulatecircularclusters()
c2 <- simulatecircularclusters()
pts <- rbind(cbind(c1, sample(c(2,3), size = nrow(c1), replace = TRUE)), cbind(c2, 1))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "n"), mode = "square")

#o - all populations co-clustered.
set.seed(24)
c1 <- simulatecircularclusters()
pts <- cbind(c1, sample(1:3, size = nrow(c1), replace = TRUE))
plotpointcloudsquare(pts, colours = ibmcols[1:3], repeatcolours = FALSE)
savetwoplots(conc(output_dir, "o"), mode = "square")

#q - A/I vs percent of activated agents.
data <- read.csv(conc(input_dir, "A-I vs Percent Activated CSR-CSR-CSR.csv"))
plotlinesrectangle(data, labels = c("A/I", "% active agents"), ylimits = c(0, 1), xlimits = c(0.16, 3))
savetwoplots(conc(output_dir, "q"), mode = "rectangle")

#r - A/I vs probability of activation.
data <- read.csv(conc(input_dir, "A-I vs Probability of Activation CSR-CSR-CSR.csv"))
plotlinesrectangle(data, labels = c("A/I", "prob. of activation"), ylimits = c(0, 1), xlimits = c(0.16, 3))
savetwoplots(conc(output_dir, "r"), mode = "rectangle")

#s - sigmoid fit.
check <- TRUE
try(expr = {
  #Fit a sigmoidal curve using non-linear least squares.
  data <- as.data.frame(data)
  model <- nls(probability_of_activation ~ SSlogis(uniqueAIs, Asym, xmid, scal), data = data,
               start = list(Asym = 1, xmid = 1, scal = 0.2))
  
  #Extract model predictions.
  data$fit <- predict(model, newdata = data)
  
  #Extract the point of inflection and scale from the model.
  inflection_point <- coef(model)["xmid"]
  scal_parameter <- coef(model)["scal"]
  check <- FALSE
}, silent = TRUE)
plotlinesrectangle(data[,c(1,3)], labels = c("A/I", "prob. of activation"), ylimits = c(0, 1), xlimits = c(0.16, 3))

x <- pracma::linspace(0.16, 3)
y <- 1 /(1 + exp(- (1 / scal_parameter) * (x - inflection_point)))
plotlinesrectangle(cbind(x,y), labels = c("A/I", "prob. of activation"), ylimits = c(0, 1), xlimits = c(0.16, 3), colours = ibmcols[2])
savetwoplots(conc(output_dir, "s"), mode = "rectangle")
