# Point Label Analysis of Super-resolved Membrane Attributes
Agent-based Spatiotemporal MOlcular Dynamics Evolving Under Simulation, or ASMODEUS, is an R package for agent-based modelling of molecular dynamics on locally flat surfaces, with an emphasis on simulating organisation of transmembrane receptors. We use a novel supervised learning approach, which inherits spatial statistics from existing data sets, such as those derived from single molecule localisation microscopy (SMLM).

## Installation
The package can be installed using pre-existing functions in the [**devtools**](https://cran.r-project.org/web/packages/devtools/index.html) package. It is highly recommended that you use **RStudio**. If you do not have **devtools** installed, enter the following line into the command prompt:
```{r}
install.packages("devtools")
```
Then, simply enter the line below into the command prompt:
```{r}
devtools::install_github("lucapanconi/asmodeus")
```

## Usage
### Seting Up
First, load the package into your R session.
```{r}
#Implement ASMODEUS library.
library(asmodeus)
```

You may also set a seed for reproducibility.

```{r}
#Set seed.
set.seed(1)
```

It is often convenient to set a main file directory for saving results. Enter a file directory of your choice here.

```{r}
#Define main save location.
main <- "C:/Users/.../"
```

### Static Data
ASMODEUS offers built-in functions for simulating static point clouds with circular clusters. These may be used to provide target statistics and test workflows.

```{r}
#Simulate a data set with small circular clusters.
small_cluster_data <- simulatecircularclusters(ROI = 1000,
                                         number_of_clusters = 10,
                                         cluster_radius = 20,
                                         points_per_cluster = 15,
                                         background_to_cluster_ratio = 0.33)

#Simulate a data set with large circular clusters.
big_cluster_data <- simulatecircularclusters(ROI = 1000,
                                               number_of_clusters = 5,
                                               cluster_radius = 40,
                                               points_per_cluster = 30,
                                               background_to_cluster_ratio = 0.33)

#Simulate a data set with very large circular clusters.
very_big_cluster_data <- simulatecircularclusters(ROI = 1000,
                                             number_of_clusters = 3,
                                             cluster_radius = 80,
                                             points_per_cluster = 50,
                                             background_to_cluster_ratio = 0.33)
```

We may also simulate complete spatially random data sets.

```{r}
#Simulate a data set with complete spatially random (CSR) data. This will have the same number of points as the cluster data.
csr_data <- simulatecsr(ROI = 1000, number_of_points = nrow(small_cluster_data))
```

We can plot static point clouds with the *plotpoints* function.

```{r}
#We can plot these point clouds individually.
plotpoints(small_cluster_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
plotpoints(big_cluster_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
plotpoints(very_big_cluster_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
plotpoints(csr_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
```

<img src="https://github.com/lucapanconi/asmodeus/blob/master/disp/Example_Point_Clouds.png" width="801">

We can extract the Ripley's K function from any static point cloud with the *ripleyk* function.

```{r}
#Get Ripley's K functions.
small_cluster_K <- ripleyk(points = small_cluster_data, ROI = 1000)
big_cluster_K <- ripleyk(points = big_cluster_data, ROI = 1000)
very_big_cluster_K <- ripleyk(points = very_big_cluster_data, ROI = 1000)
csr_k <- ripleyk(points = csr_data, ROI = 1000)
```

These can be plotted separately or altogether using the *plotlines* function.

```{r}
#Plot Ripley's K functions.
plotlines(data = cbind(small_cluster_K,
                       big_cluster_K[,2],
                       very_big_cluster_K[,2],
                       csr_k[,2]),
          labels = c("r", "K(r)"))
```

<img src="https://github.com/lucapanconi/asmodeus/blob/master/disp/Example_K_Functions.png" width="801">

### Basic Dynamic Simulations
The primary function of ASMODEUS is to simulate agent-based models of molecular dynamics in which each agent (molecule) is iteratively fit to a target spatial statistic (the Ripley's K function). The basic, single population simulation initialises as a CSR distribution and fits to the global K function of a given target point cloud.

```{r}
#Create a simulation with default parameters which tries to fit an initially CSR point cloud to the small cluster data set.
sim1 <- globalripleysim(point_cloud = small_cluster_data)
```

Here, we try to rederive the *small_cluster_data* point clouds from Markov Chain Monte Carlo simulation. We can animate this simulating using the *animate_points* function.

```{r}
#Animate point distribution.
animate_points(simulation = sim1)

#Save the animation using gganimate's anim_save function.
gganimate::anim_save(filename = paste(main, "1_Small_Clusters_Simulation.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/1_Small_Clusters_Simulation.gif)

If we know the target K function, we can animate the change in the global Ripley's K function across the simulation.

```{r}
#Animate change in Ripley's K function.
animate_target(simulation = sim1, ROI = 1000, target = small_cluster_K)

#We can remove the legend by setting legendposition = "none". The target here is always static anyway.
animate_target(simulation = sim1, ROI = 1000, target = small_cluster_K, legendposition = "none")
gganimate::anim_save(filename = paste(main, "2_Small_Clusters_K.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/2_Small_Clusters_K.gif)

Alternatively, we can animate both the K and H function by directly plugging in the target point cloud. The K function appears as above.

```{r}
#Animate K function directly from point cloud (without manually calculating K function).
animate_K_from_points(simulation = sim1, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")

#Animate H function directly from point cloud (without manually calculating H function).
animate_H_from_points(simulation = sim1, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")
gganimate::anim_save(filename = paste(main, "3_Small_Clusters_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/3_Small_Clusters_H.gif)

We can define simulation parameters manually, use *help("globalripleysim")* for details. In particular, we can specify an initial distribution to start the simulation from. In this case, we have set the initial distribution as the big clusters data.

```{r}
#Define simulation parameters manually, use help("globalripleysim") for details. In particular, we can specify an initial distribution to start the simulation from. In this case, we have set this initial distribution as the big clusters data.
sim2 <- globalripleysim(D_min = 0,
                       D_max = 63,
                       ROI = 1000,
                       times = 1200,
                       point_cloud = small_cluster_data,
                       rmax = 200,
                       nrval = 20,
                       initial_distribution = big_cluster_data)

#The simulation now starts from large clusters.
animate_points(simulation = sim2)
gganimate::anim_save(filename = paste(main, "4_Big_Clusters_to_Small_Clusters_Simulation.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/4_Big_Clusters_to_Small_Clusters_Simulation.gif)

This change of initial distribution is reflected in the H function.

```{r}
#This is reflected in the H function.
animate_H_from_points(simulation = sim2, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")
gganimate::anim_save(filename = paste(main, "5_Big_Clusters_to_Small_Clusters_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/5_Big_Clusters_to_Small_Clusters_H.gif)

If we do not have a target point cloud, but do have a target statistic, we can input this directly. Remember to define the number of points to be used in the simulation.

```{r}
#In some cases, we can define our own target K function too, but we must specify how many points should be used in the simulation.
sim3 <- globalripleysim(D_min = 0,
                        D_max = 63,
                        ROI = 1000,
                        times = 1200,
                        target = small_cluster_K,
                        number_of_points = 982,
                        rmax = 200,
                        nrval = 20,
                        initial_distribution = big_cluster_data)

#In this case, we still try to fit to the small clusters K function, so there is no difference. 
animate_points(simulation = sim3)
gganimate::anim_save(filename = paste(main, "6_Big_Clusters_to_Small_Clusters_Simulation_from_Target.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/6_Big_Clusters_to_Small_Clusters_Simulation_from_Target.gif)

The target used here is defined manually.

```{r}
#The target here is defined manually.
animate_H_from_points(simulation = sim3, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")
gganimate::anim_save(filename = paste(main, "7_Big_Clusters_to_Small_Clusters_H_from_Target.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/7_Big_Clusters_to_Small_Clusters_H_from_Target.gif)

### Multiple Targets

We can provide multiple possible target statistics or point clouds. These may be considered simultaneously or at distinct time points. For distinct time points, we specify the times for which the target must change.

```{r}
#We can define multiple targets by inputting a list of point clouds or targets.
target_clouds <- list(big_cluster_data, small_cluster_data)

#We can force the simulator to switch from one target to another at given time points. This can be done for as many targets as we like.
sim4 <- distripleysim(point_clouds = target_clouds, time_points = c(500, 1000))

#The target is big clusters up until time point 500, then switches to small clusters until the end of the simulation (time point 1000).
animate_points(simulation = sim4)
gganimate::anim_save(filename = paste(main, "8_Discrete_Time_Points.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/8_Discrete_Time_Points.gif)

When animating the global H function, we can observe the shift in the target.

```{r}
#We can observe the shift in target in the H function. Make sure to specify the time points in the animation function.
animate_H_from_points(simulation = sim4, ROI = 1000, point_clouds = target_clouds, time_points = c(500, 1000), legendposition = "none")
gganimate::anim_save(filename = paste(main, "9_Discrete_Time_Points_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/9_Discrete_Time_Points_H.gif)

When providing targets simultaneously, each agent will align with whichever target is closest. We can introduce perturbations at given times which offset each point randomly by a given strength (distance).

```{r}
#We can also provide both targets simultaneously. Each point will move towards which ever target is closest.
sim5 <- simripleysim(point_clouds = target_clouds, times = 1000, perturbation_times = 500, perturbation_strength = 100)

#Points may drift towards either target. We can introduce perturbations which offest each point with distance given by perturbation_strength. Here we use a 100nm offset at time point 500.
animate_points(simulation = sim5)
gganimate::anim_save(filename = paste(main, "10_Simultaneous_Targets.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/10_Simultaneous_Targets.gif)

The impact of simultaneous targets is often unpredictable.

```{r}
#The H function may assume one target over another, or fall somewhere in between.
animate_H_from_points(simulation = sim5, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "11_Simultaneous_Targets_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/11_Simultaneous_Targets_H.gif)

We may also perturb points by cross-linking. This gives the effect of "squeezing" the points into clusters.

```{r}
#Instead of offsetting points, we can cross-link points together. This gives the effect of "squeezing" points into clusters.
sim6 <- squeezeripleysim(point_clouds = target_clouds, times = 1000, perturbation_time = 500, number_cross_linked_clusters = 5, number_cross_linked = 30, cross_linked_cluster_radius = 40)

#We form 5 cross-linked clusters at time point 500. Each cluster contains 30 points and is of radius 40.
animate_points(simulation = sim6)
gganimate::anim_save(filename = paste(main, "12_Simultaneous_Targets_with_Squeeze.gif", sep = ""))

#A shift in the H function is shown at time point 500, when the squeeze perturbation is introduced.
animate_H_from_points(simulation = sim6, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "13_Simultaneous_Targets_with_Squeeze_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/12_Simultaneous_Targets_with_Squeeze.gif)

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/13_Simultaneous_Targets_with_Squeeze_H.gif)

If we don't wish to immobilise agents after cross-linking, we can release them. This allows them to continue drifting.

```{r}
#If we don't wish to immobilise points after cross-linking, we can release them.
sim7 <- squeezereleaseripleysim(point_clouds = target_clouds, times = 1000, perturbation_time = 500, number_cross_linked_clusters = 5, number_cross_linked = 30, cross_linked_cluster_radius = 40)
animate_points(simulation = sim7)
gganimate::anim_save(filename = paste(main, "14_Simultaneous_Targets_with_Squeeze_and_Release.gif", sep = ""))
animate_H_from_points(simulation = sim7, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "15_Simultaneous_Targets_with_Squeeze_and_Release_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/14_Simultaneous_Targets_with_Squeeze_and_Release.gif)

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/15_Simultaneous_Targets_with_Squeeze_and_Release_H.gif)

Alternatively, we can manually alter agent step sizes (speed) at distinct time points.

```{r}
#We can manually change the step size (speed) of points during the simulation by inputting a vector of D_min and D_max values. We can also provide a perturbation to offset points.
sim8 <- speedripleysim(D_min = c(0, 10), D_max = c(10, 100), point_clouds = target_clouds, times = 1000, perturbation_times = 500, perturbation_strength = 100)
animate_points(simulation = sim7)
gganimate::anim_save(filename = paste(main, "16_Simultaneous_Targets_with_Speed_Change.gif", sep = ""))
animate_H_from_points(simulation = sim7, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "17_Simultaneous_Targets_with_Speed_Change_H.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/16_Simultaneous_Targets_with_Speed_Change.gif)

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/17_Simultaneous_Targets_with_Speed_Change_H.gif)

### Multiple Populations

We can simulate multiple isolated or interacting molecular species by giving a target distribution for each. Populations simulated in isolation will not take account of other molecular species when calculating spatial statistics.

```{r}
#We can simulate multiple populations representing different molecular species. Each population can be given its own target function.
target_clouds <- list(small_cluster_data, big_cluster_data, very_big_cluster_data)

#Here we simulate them independently of each other.
sim9 <- popripleysimisol(point_clouds = target_clouds)

#When animating, we can specify labels for each population.
animate_populations(simulation = sim9, labels = c("Small Clusters", "Big Clusters", "Very Big Clusters"))
gganimate::anim_save(filename = paste(main, "18_Multiple_Populations_Isolated.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/18_Multiple_Populations_Isolated.gif)

We can also animate without labels.

```{r}
#We can also plot this without labels.
animate_populations(simulation = sim9, legendposition = "none")
gganimate::anim_save(filename = paste(main, "19_Multiple_Populations_Isolated_without_Legend.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/19_Multiple_Populations_Isolated_without_Legend.gif)

We can also simulate interactions between populations. In this case, each agent takes account of all other agents, regardless of their population type. To do this, we must convert the point clouds to a single data frame with three columns: x coordinate, y coordinate and population number.

```{r}
#We can also allow different populations to interact. To do this, we must create a point cloud with three columns: x coordinate, y coordinate and population number.
all_points <- rbind(cbind(small_cluster_data, 1), cbind(big_cluster_data, 2), cbind(very_big_cluster_data, 3))
sim10 <- popripleysim(point_cloud = all_points)
animate_populations(simulation = sim10, legendposition = "none")
gganimate::anim_save(filename = paste(main, "20_Multiple_Populations.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/20_Multiple_Populations.gif)

We can induce and track interactions between populations. Here we assume the first point cloud is made of activators, the second is made of inhibitors, and the third is made of agents, which may be acted on by either activators or inhibitors. When an inactive agent (population 3) comes within 5nm of an activator (population 1), it becomes an active agent. Conversely, when an active agent (population 4) comes within 5nm of an activator (population 2), it becomes an inactive agent.

```{r}
#We can induce molecular interactions after the simulation is complete. Here we assume the first point cloud is made of activators, the second is made of inhibitors, and the third is made of agents, which may be acted on by either activators or inhibitors.
sim11 <- population_changer(simulation = sim10, A = c(3, 4), B = c(4, 3), C = c(1, 2), D = c(5, 5))

#When an inactive agent (population 3) comes within 5nm of an activator (population 1), it becomes an active agent. Conversely, when an active agent (population 4) comes within 5nm of an activator (population 2), it becomes an inactive agent.
animate_populations(simulation = sim11, labels = c("Activator", "Inhibitor", "Inactive Agent", "Active Agent"))
gganimate::anim_save(filename = paste(main, "21_Multiple_Populations_with_Interactions.gif", sep = ""))
```

![Alt Text](https://github.com/lucapanconi/asmodeus/blob/master/disp/21_Multiple_Populations_with_Interactions.gif)

We can track the change in number of a particular population over time.

```{r}
#We can track changes in a population over time.
changes <- track_changes(simulation = sim11, A = 4)
plotlines(changes, labels = c("time", "number active"))
```

<img src="https://github.com/lucapanconi/asmodeus/blob/master/disp/Active_Agents_vs_Time.png" width="801">

## License
Licensed under GNU General Public License v3.0.